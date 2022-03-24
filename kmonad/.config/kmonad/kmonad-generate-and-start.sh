#!/usr/bin/env bash
keyboardSetup() {
  local FILE=$1
  echo "(defsrc
    esc  f1   f2   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12        ssrq slck pause
    grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc  ins  home pgup  nlck kp/  kp*  kp-
    tab  q    w    e    r    t    y    u    i    o    p    [    ]    \\     del  end  pgdn  kp7  kp8  kp9  kp+
    caps a    s    d    f    g    h    j    k    l    ;    '    ret                        kp4  kp5  kp6
    lsft z    x    c    v    b    n    m    ,    .    /    rsft                 up         kp1  kp2  kp3  kprt
    lctl lmet lalt           spc            ralt rmet cmp  rctl            left down rght  kp0  kp.
  )" >> "$FILE"

  echo "(defalias
    cspc (tap-hold-next-release 150 spc lctrl)
  )" >> "$FILE"

  echo "(deflayer name
    _    _    _    _    _    _    _    _    _    _    _    _    _          _    _    _
    _    _    _    _    _    _    _    _    _    _    _    _    _    _     _    _    _     _    _    _    _
    _    _    _    _    _    _    _    _    _    _    _    _    _    _     _    _    _     _    _    _    _
    lalt    _    _    _    _    _    _    _    _    _    _    _    _                          _    _    _
    _    _    _    _    _    _    _    _    _    _    _    _                    _          _    _    _    _
    _    _    _              @cspc              _    _    _    _               _    _    _     _    _   
  )" >> "$FILE"
}

INTERNAL_KEYBOARD=$(ls /dev/input/by-path |grep -i "platform.*kbd")
EXTERNAL_KEYBOARDS=$(ls /dev/input/by-id |grep -i "kbd" | grep -vi "mouse" | grep -vi "ergodox")

pkill kmonad

for keyboard in $(echo "$INTERNAL_KEYBOARD $EXTERNAL_KEYBOARDS"); do
  HERE="$(dirname "$(readlink -f "${0}")")"
  FILE="$HERE/$keyboard.kbd"
  DEVICE_FILE=$(find /dev/input -name $keyboard)

  echo '' > "$FILE" 

  KEYBOARD=$(echo "(device-file \"$DEVICE_FILE\")")
  echo "(defcfg
       input  $KEYBOARD
       output (uinput-sink \"My KMonad output\"
               \"setxkbmap -layout us -option 'compose:rctrl'\")
       cmp-seq rctrl
       fallthrough true
       allow-cmd false
       )" >> "$FILE"
  keyboardSetup "$FILE"

  kmonad "$FILE" &
done