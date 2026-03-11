#!/usr/bin/env python3
"""Switch Vivaldi theme and Dark Reader via Chrome DevTools Protocol.

Operates in two modes:
  daemon  - persistent background process maintaining CDP connections
            so Emulation.setEmulatedMedia (prefers-color-scheme) survives.
  light|dark - one-shot client that:
            1) tells the daemon to update prefers-color-scheme
            2) injects Vivaldi UI CSS vars
            3) sets Vivaldi theme prefs
            4) toggles Dark Reader scheme

Requires Vivaldi launched with --remote-debugging-port=9222.

Usage:
  vivaldi-ctl.py daemon          # start persistent daemon
  vivaldi-ctl.py light|dark      # switch theme
"""
import http.client
import json
import socket
import struct
import os
import base64
import sys
import time
import select
import errno

SOCK_PATH = os.path.expanduser("~/.cache/vivaldi-theme.sock")
CDP_HOST = "localhost"
CDP_PORT = 9222


# -- WebSocket helpers -------------------------------------------------------

def ws_connect(host, port, path):
    """Open a WebSocket connection, return the socket."""
    s = socket.create_connection((host, port), timeout=5)
    key = base64.b64encode(os.urandom(16)).decode()
    s.sendall(
        f"GET /{path} HTTP/1.1\r\n"
        f"Host: {host}:{port}\r\n"
        f"Upgrade: websocket\r\n"
        f"Connection: Upgrade\r\n"
        f"Sec-WebSocket-Key: {key}\r\n"
        f"Sec-WebSocket-Version: 13\r\n\r\n".encode()
    )
    s.recv(4096)
    return s


def ws_send_frame(s, message):
    """Send a masked WebSocket text frame."""
    data = message.encode()
    frame = bytearray([0x81])
    length = len(data)
    if length < 126:
        frame.append(0x80 | length)
    elif length < 65536:
        frame.append(0x80 | 126)
        frame += struct.pack(">H", length)
    else:
        frame.append(0x80 | 127)
        frame += struct.pack(">Q", length)
    mask = os.urandom(4)
    frame += mask
    frame += bytes(b ^ mask[i % 4] for i, b in enumerate(data))
    s.sendall(frame)


def ws_recv_frame(s):
    """Receive a WebSocket frame, return text payload."""
    def rx(n):
        buf = b""
        while len(buf) < n:
            c = s.recv(n - len(buf))
            if not c:
                raise ConnectionError("WebSocket closed")
            buf += c
        return buf

    h = rx(2)
    masked = h[1] & 0x80
    length = h[1] & 0x7F
    if length == 126:
        length = struct.unpack(">H", rx(2))[0]
    elif length == 127:
        length = struct.unpack(">Q", rx(8))[0]
    mk = rx(4) if masked else None
    p = rx(length)
    if mk:
        p = bytes(b ^ mk[i % 4] for i, b in enumerate(p))
    return p.decode()


def cdp_send(s, method, params, msg_id, session_id=None):
    """Send a CDP command and wait for its response."""
    msg = {"id": msg_id, "method": method, "params": params}
    if session_id:
        msg["sessionId"] = session_id
    ws_send_frame(s, json.dumps(msg))
    while True:
        d = json.loads(ws_recv_frame(s))
        if d.get("id") == msg_id:
            return d


def cdp_eval(s, expression, msg_id, session_id=None):
    """Evaluate JS via Runtime.evaluate, optionally in a session."""
    msg = {
        "id": msg_id,
        "method": "Runtime.evaluate",
        "params": {"expression": expression, "returnByValue": True},
    }
    if session_id:
        msg["sessionId"] = session_id
    ws_send_frame(s, json.dumps(msg))
    while True:
        d = json.loads(ws_recv_frame(s))
        if d.get("id") == msg_id:
            return d


def parse_ws_url(ws_url):
    """Parse ws://host:port/path into (host, port, path)."""
    rest = ws_url.replace("ws://", "")
    host_port, _, path = rest.partition("/")
    host, port = host_port.split(":")
    return host, int(port), path


# -- Color helpers -----------------------------------------------------------

def hex_to_rgb(h):
    h = h.lstrip("#")
    return tuple(int(h[i:i + 2], 16) for i in (0, 2, 4))


def rgb_to_hex(r, g, b):
    return f"#{int(r):02x}{int(g):02x}{int(b):02x}"


def lighten(color, amount):
    r, g, b = hex_to_rgb(color)
    return rgb_to_hex(
        min(255, r + (255 - r) * amount),
        min(255, g + (255 - g) * amount),
        min(255, b + (255 - b) * amount),
    )


def darken(color, amount):
    r, g, b = hex_to_rgb(color)
    return rgb_to_hex(r * (1 - amount), g * (1 - amount), b * (1 - amount))


def alpha(color, a):
    return color + format(int(a * 255), "02x")


def build_css_vars(bg, fg, accent_bg, highlight_bg, window_bg,
                   accent_fg="#ffffff", highlight_fg="#ffffff",
                   success="#50fa7b", warning="#f1fa8c", error="#ff5555",
                   is_dark=True):
    """Compute the full set of Vivaldi CSS custom properties."""
    if not window_bg:
        window_bg = bg
    v = {}
    # Background shades
    v["--colorBg"] = bg
    v["--colorBgAlpha"] = alpha(bg, 0.90)
    v["--colorBgAlphaHeavy"] = alpha(bg, 0.65)
    v["--colorBgAlphaHeavier"] = alpha(bg, 0.25)
    v["--colorBgAlphaBlur"] = alpha(bg, 0.85)
    v["--colorBgDark"] = darken(bg, 0.08)
    v["--colorBgDarker"] = darken(bg, 0.15)
    v["--colorBgLight"] = lighten(bg, 0.05)
    v["--colorBgLighter"] = lighten(bg, 0.10)
    v["--colorBgLightIntense"] = lighten(bg, 0.12)
    v["--colorBgIntense"] = lighten(bg, 0.08)
    v["--colorBgIntenser"] = lighten(bg, 0.10)
    v["--colorBgIntserAlpha"] = alpha(lighten(bg, 0.05), 0.95)
    v["--colorBgInverse"] = lighten(bg, 0.04)
    v["--colorBgInverser"] = lighten(bg, 0.08)
    v["--colorBgFaded"] = darken(bg, 0.10)
    # Foreground shades
    v["--colorFg"] = fg
    v["--colorFgAlpha"] = alpha(fg, 0.10)
    v["--colorFgIntense"] = lighten(fg, 0.05) if is_dark else darken(fg, 0.10)
    v["--colorFgFaded"] = darken(fg, 0.30) if is_dark else lighten(fg, 0.30)
    v["--colorFgFadedMore"] = darken(fg, 0.50) if is_dark else lighten(fg, 0.50)
    v["--colorFgFadedMost"] = darken(fg, 0.65) if is_dark else lighten(fg, 0.65)
    # Accent
    v["--colorAccentBg"] = accent_bg
    v["--colorAccentBgAlpha"] = alpha(accent_bg, 0.55)
    v["--colorAccentBgAlphaHeavy"] = alpha(accent_bg, 0.35)
    v["--colorAccentBgAlphaBlur"] = alpha(accent_bg, 0.85)
    v["--colorAccentBgDark"] = darken(accent_bg, 0.15)
    v["--colorAccentBgDarker"] = darken(accent_bg, 0.30)
    v["--colorAccentBgFaded"] = darken(accent_bg, 0.40)
    v["--colorAccentBgFadedMore"] = darken(accent_bg, 0.20)
    v["--colorAccentBgFadedMost"] = darken(accent_bg, 0.10)
    v["--colorAccentBorder"] = accent_bg
    v["--colorAccentBorderDark"] = darken(accent_bg, 0.10)
    v["--colorAccentFg"] = accent_fg
    v["--colorAccentFgFaded"] = darken(accent_fg, 0.20) if is_dark else lighten(accent_fg, 0.20)
    v["--colorAccentFgAlpha"] = alpha(accent_fg, 0.15)
    v["--colorAccentFgAlphaHeavy"] = alpha(accent_fg, 0.05)
    # Highlight
    v["--colorHighlightBg"] = highlight_bg
    v["--colorHighlightBgFaded"] = lighten(highlight_bg, 0.20)
    v["--colorHighlightBgAlpha"] = alpha(highlight_bg, 0.10)
    v["--colorHighlightBgDark"] = darken(highlight_bg, 0.15)
    v["--colorHighlightFg"] = highlight_fg
    v["--colorHighlightFgAlpha"] = alpha(highlight_fg, 0.50)
    v["--colorHighlightFgAlphaHeavy"] = alpha(highlight_fg, 0.25)
    # Borders
    v["--colorBorder"] = lighten(bg, 0.15) if is_dark else darken(bg, 0.12)
    v["--colorBorderDisabled"] = lighten(bg, 0.05) if is_dark else darken(bg, 0.04)
    v["--colorBorderSubtle"] = lighten(bg, 0.08) if is_dark else darken(bg, 0.06)
    v["--colorBorderIntense"] = lighten(bg, 0.25) if is_dark else darken(bg, 0.20)
    # Status
    v["--colorSuccessBg"] = success
    v["--colorSuccessBgAlpha"] = alpha(success, 0.10)
    v["--colorSuccessFg"] = "#000000" if is_dark else "#ffffff"
    v["--colorWarningBg"] = warning
    v["--colorWarningBgAlpha"] = alpha(warning, 0.10)
    v["--colorWarningFg"] = "#000000"
    v["--colorErrorBg"] = error
    v["--colorErrorBgAlpha"] = alpha(error, 0.10)
    v["--colorErrorFg"] = "#ffffff"
    # Window
    v["--colorWindowBg"] = window_bg
    v["--colorWindowFg"] = "#ffffff" if is_dark else "#000000"
    v["--colorTabBar"] = "var(--colorAccentBg)"
    return v


# -- Palettes ---------------------------------------------------------------

DARK = build_css_vars(
    bg="#282a36", fg="#f8f8f2",
    accent_bg="#ff79c6", highlight_bg="#bd93f9",
    window_bg="#282a36", is_dark=True,
    success="#50fa7b", warning="#f1fa8c", error="#ff5555",
)

LIGHT = build_css_vars(
    bg="#f8f8f2", fg="#282a36",
    accent_bg="#ef3939", highlight_bg="#2c78f2",
    window_bg="#f8f8f2", is_dark=False,
    success="#06a700", warning="#ffcc00", error="#c64539",
)


DARK_READER_ID = "eimadpbcbfnmbkopoojfekhnkhdbieeh"
VIVALDI_UI_ID = "mpognobbkildjkofajifpdfhcoklimli"

VIVALDI_THEME_DARK = "fca11113-6bf3-4242-b7b5-55da461ec7d9"  # Dracula Modified
VIVALDI_THEME_LIGHT = "ab4f94a4-1e3c-4d7a-b4d8-d56dc651e6c4"  # Dracula Light

VIVALDI_PREFS = os.path.expanduser("~/.config/vivaldi/Default/Preferences")


# -- Daemon ------------------------------------------------------------------

def daemon_main():
    """Persistent daemon: maintains CDP sessions for prefers-color-scheme."""
    # Remove stale socket
    try:
        os.unlink(SOCK_PATH)
    except FileNotFoundError:
        pass

    # Create Unix socket for receiving commands
    srv = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    srv.bind(SOCK_PATH)
    srv.listen(2)
    srv.setblocking(False)

    mode = "dark"
    sessions = {}  # sessionId -> targetId
    browser_ws = None
    msg_counter = 100

    def get_msg_id():
        nonlocal msg_counter
        msg_counter += 1
        return msg_counter

    def connect_browser():
        """Connect to browser-level WebSocket and set up auto-attach."""
        nonlocal browser_ws, msg_counter
        conn = http.client.HTTPConnection(CDP_HOST, CDP_PORT, timeout=2)
        conn.request("GET", "/json/version")
        ver = json.loads(conn.getresponse().read())
        conn.close()

        h, p, path = parse_ws_url(ver["webSocketDebuggerUrl"])
        browser_ws = ws_connect(h, p, path)
        browser_ws.setblocking(False)

        # Enable auto-attach to all targets (flatten = multiplexed sessions)
        msg = {
            "id": get_msg_id(),
            "method": "Target.setAutoAttach",
            "params": {
                "autoAttach": True,
                "waitForDebuggerOnStart": False,
                "flatten": True,
            },
        }
        ws_send_frame(browser_ws, json.dumps(msg))

        # Also discover existing targets
        msg2 = {
            "id": get_msg_id(),
            "method": "Target.setDiscoverTargets",
            "params": {"discover": True},
        }
        ws_send_frame(browser_ws, json.dumps(msg2))

    def apply_emulation(session_id):
        """Send Emulation.setEmulatedMedia to a specific session."""
        try:
            msg = {
                "id": get_msg_id(),
                "sessionId": session_id,
                "method": "Emulation.setEmulatedMedia",
                "params": {
                    "features": [{"name": "prefers-color-scheme", "value": mode}],
                },
            }
            ws_send_frame(browser_ws, json.dumps(msg))
        except Exception:
            pass

    def apply_all():
        """Apply emulation to all known sessions."""
        for sid in list(sessions):
            apply_emulation(sid)

    def handle_ws_message(raw):
        """Process a CDP event from the browser WebSocket."""
        try:
            d = json.loads(raw)
        except json.JSONDecodeError:
            return

        method = d.get("method", "")

        if method == "Target.attachedToTarget":
            params = d.get("params", {})
            sid = params.get("sessionId")
            info = params.get("targetInfo", {})
            tid = info.get("targetId", "")
            if sid:
                sessions[sid] = tid
                apply_emulation(sid)

        elif method == "Target.detachedFromTarget":
            sid = d.get("params", {}).get("sessionId")
            sessions.pop(sid, None)

        elif method == "Target.targetCreated":
            # Auto-attach should handle this, but manually attach just in case
            info = d.get("params", {}).get("targetInfo", {})
            tid = info.get("targetId", "")
            if tid and info.get("type") in ("page", "other"):
                try:
                    msg = {
                        "id": get_msg_id(),
                        "method": "Target.attachToTarget",
                        "params": {"targetId": tid, "flatten": True},
                    }
                    ws_send_frame(browser_ws, json.dumps(msg))
                except Exception:
                    pass

    def recv_all_ws():
        """Read all available WebSocket frames without blocking."""
        while True:
            try:
                raw = ws_recv_frame(browser_ws)
                handle_ws_message(raw)
            except (BlockingIOError, socket.timeout):
                break
            except ConnectionError:
                raise

    # Main loop with reconnection
    while True:
        try:
            if browser_ws is None:
                connect_browser()
                time.sleep(0.5)
                # Read initial events (auto-attach responses)
                try:
                    recv_all_ws()
                except (BlockingIOError, socket.timeout):
                    pass

            # select on both browser WS and Unix socket
            read_fds = [browser_ws, srv]
            try:
                readable, _, _ = select.select(read_fds, [], [], 5.0)
            except (ValueError, OSError):
                # browser_ws closed
                browser_ws = None
                sessions.clear()
                time.sleep(2)
                continue

            for fd in readable:
                if fd is srv:
                    # New client connection
                    try:
                        client, _ = srv.accept()
                        cmd = client.recv(64).decode().strip()
                        if cmd in ("light", "dark"):
                            mode = cmd
                            apply_all()
                            client.send(b"ok\n")
                        elif cmd == "stop":
                            client.send(b"stopped\n")
                            client.close()
                            srv.close()
                            if browser_ws:
                                browser_ws.close()
                            try:
                                os.unlink(SOCK_PATH)
                            except FileNotFoundError:
                                pass
                            return
                        elif cmd == "ping":
                            client.send(b"pong\n")
                        client.close()
                    except Exception:
                        pass

                elif fd is browser_ws:
                    try:
                        recv_all_ws()
                    except ConnectionError:
                        browser_ws = None
                        sessions.clear()
                        time.sleep(2)

        except Exception:
            browser_ws = None
            sessions.clear()
            time.sleep(5)


# -- Client ------------------------------------------------------------------

def daemon_send(cmd):
    """Send a command to the running daemon. Returns True on success."""
    try:
        s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        s.settimeout(2)
        s.connect(SOCK_PATH)
        s.send(cmd.encode())
        resp = s.recv(64).decode().strip()
        s.close()
        return resp == "ok"
    except (ConnectionRefusedError, FileNotFoundError, socket.timeout, OSError):
        return False


def daemon_running():
    """Check if daemon is running."""
    try:
        s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        s.settimeout(1)
        s.connect(SOCK_PATH)
        s.send(b"ping")
        resp = s.recv(64).decode().strip()
        s.close()
        return resp == "pong"
    except (ConnectionRefusedError, FileNotFoundError, socket.timeout, OSError):
        return False


def oneshot_main(mode):
    """One-shot: Vivaldi UI CSS vars, theme prefs, Dark Reader toggle."""
    light = mode == "light"
    palette = LIGHT if light else DARK
    css_str = "; ".join(f"{k}: {v}" for k, v in palette.items())

    # --- Persist theme in Preferences file for next Vivaldi launch ---
    if os.path.isfile(VIVALDI_PREFS):
        try:
            with open(VIVALDI_PREFS, "r") as f:
                prefs = json.load(f)
            theme_id = VIVALDI_THEME_LIGHT if light else VIVALDI_THEME_DARK
            prefs.setdefault("vivaldi", {}).setdefault("themes", {})["current"] = theme_id
            prefs["vivaldi"].setdefault("theme", {}).setdefault("schedule", {})["enabled"] = 0
            with open(VIVALDI_PREFS, "w") as f:
                json.dump(prefs, f, separators=(",", ":"))
        except Exception:
            pass

    try:
        # Fetch all targets
        conn = http.client.HTTPConnection(CDP_HOST, CDP_PORT, timeout=2)
        conn.request("GET", "/json")
        targets = json.loads(conn.getresponse().read())
        conn.close()
    except Exception:
        return

    # --- Vivaldi UI: inject CSS vars on every window.html ---
    windows = [
        t for t in targets
        if f"{VIVALDI_UI_ID}/window.html" in t.get("url", "")
    ]
    theme_id = VIVALDI_THEME_LIGHT if light else VIVALDI_THEME_DARK
    for win in windows:
        try:
            h, p, path = parse_ws_url(win["webSocketDebuggerUrl"])
            s = ws_connect(h, p, path)
            cdp_eval(s, f"""
                (function() {{
                    var b = document.getElementById("browser");
                    if (!b) return;
                    {json.dumps(css_str)}.split("; ").forEach(function(pair) {{
                        var i = pair.indexOf(": ");
                        b.style.setProperty(pair.slice(0, i), pair.slice(i + 2));
                    }});
                }})()
            """, 1)
            # Disable OS theme scheduling (broken on NixOS/Hyprland) and set theme directly
            cdp_eval(s, 'vivaldi.prefs.set({path:"vivaldi.theme.schedule.enabled",value:"off"})', 2)
            cdp_eval(s, f'vivaldi.prefs.set({{path:"vivaldi.themes.current",value:"{theme_id}"}})', 3)
            s.close()
        except Exception:
            pass

    # --- Dark Reader ---
    dark_reader_sw = next(
        (t for t in targets
         if t.get("type") == "service_worker"
         and DARK_READER_ID in t.get("url", "")),
        None,
    )
    if dark_reader_sw:
        try:
            conn2 = http.client.HTTPConnection(CDP_HOST, CDP_PORT, timeout=2)
            conn2.request("GET", "/json/version")
            ver = json.loads(conn2.getresponse().read())
            conn2.close()

            h, p, path = parse_ws_url(ver["webSocketDebuggerUrl"])
            bs = ws_connect(h, p, path)

            # Attach to service worker to change settings and reload extension
            r = cdp_send(bs, "Target.attachToTarget",
                         {"targetId": dark_reader_sw["id"], "flatten": True}, 1)
            session_id = r["result"]["sessionId"]

            dr_mode = 0 if light else 1
            cdp_send(bs, "Runtime.evaluate", {
                "expression": f"""(async () => {{
                    const data = await chrome.storage.sync.get('theme');
                    const theme = data.theme || {{}};
                    theme.mode = {dr_mode};
                    await chrome.storage.sync.set({{theme}});
                    chrome.runtime.reload();
                }})()""",
                "returnByValue": True,
                "awaitPromise": True,
            }, 2, session_id)
            bs.close()

            # Wait for new service worker, then reload tabs
            old_sw_id = dark_reader_sw["id"]
            time.sleep(2)
            conn3 = http.client.HTTPConnection(CDP_HOST, CDP_PORT, timeout=2)
            conn3.request("GET", "/json")
            new_targets = json.loads(conn3.getresponse().read())
            conn3.close()
            new_sw = next(
                (t for t in new_targets
                 if t.get("type") == "service_worker"
                 and DARK_READER_ID in t.get("url", "")
                 and t.get("id") != old_sw_id),
                None,
            )
            if new_sw:
                conn4 = http.client.HTTPConnection(CDP_HOST, CDP_PORT, timeout=2)
                conn4.request("GET", "/json/version")
                ver2 = json.loads(conn4.getresponse().read())
                conn4.close()
                h2, p2, path2 = parse_ws_url(ver2["webSocketDebuggerUrl"])
                bs2 = ws_connect(h2, p2, path2)
                r2 = cdp_send(bs2, "Target.attachToTarget",
                              {"targetId": new_sw["id"], "flatten": True}, 1)
                sid2 = r2["result"]["sessionId"]
                cdp_send(bs2, "Runtime.evaluate", {
                    "expression": """(async () => {
                        const tabs = await chrome.tabs.query({});
                        for (const tab of tabs) {
                            if (tab.url && !tab.url.startsWith('chrome://') &&
                                !tab.url.startsWith('chrome-extension://'))
                                chrome.tabs.reload(tab.id);
                        }
                    })()""",
                    "returnByValue": True,
                    "awaitPromise": True,
                }, 2, sid2)
                bs2.close()
        except Exception:
            pass


# -- Main --------------------------------------------------------------------

if __name__ == "__main__":
    cmd = sys.argv[1].lower() if len(sys.argv) > 1 else "dark"

    if cmd == "daemon":
        daemon_main()
    elif cmd == "stop":
        daemon_send("stop")
    else:
        # Send mode to daemon for persistent prefers-color-scheme
        daemon_send(cmd)
        # Do one-shot work (CSS vars, theme prefs, Dark Reader)
        oneshot_main(cmd)
        # Re-apply emulation after Dark Reader tab reloads
        time.sleep(1)
        daemon_send(cmd)
