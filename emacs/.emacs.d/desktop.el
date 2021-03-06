(defun exwm/autorandr-refresh ()
  (interactive)
  (shell/async-command-no-output "autorandr --change --force"))

(use-package desktop-environment
  :after exwm
  :config
  (setq desktop-environment-screenshot-directory "~/Images/")

  (setq desktop-environment-volume-toggle-command "pactl set-sink-mute 0 toggle")
  (setq desktop-environment- "pactl set-sink-mute 0 toggle")

  (exwm-input-set-key (kbd "<XF86MonBrightnessUp>") #'desktop-environment-brightness-increment)
  (exwm-input-set-key (kbd "<XF86MonBrightnessDown>") #'desktop-environment-brightness-decrement)
  (exwm-input-set-key (kbd "S-<XF86MonBrightnessUp>") #'desktop-environment-brightness-increment-slowly)
  (exwm-input-set-key (kbd "S-<XF86MonBrightnessDown>") #'desktop-environment-brightness-decrement-slowly)
  (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") #'desktop-environment-volume-increment)
  (exwm-input-set-key (kbd "<XF86AudioLowerVolume>") #'desktop-environment-volume-decrement)
  (exwm-input-set-key (kbd "S-<XF86AudioRaiseVolume>") #'desktop-environment-volume-increment-slowly)
  (exwm-input-set-key (kbd "S-<XF86AudioLowerVolume>") #'desktop-environment-volume-decrement-slowly)
  (exwm-input-set-key (kbd "<XF86AudioMute>") #'desktop-environment-toggle-mute)
  (exwm-input-set-key (kbd "<XF86AudioMicMute>") #'desktop-environment-toggle-microphone-mute)
  (exwm-input-set-key (kbd "S-<print>") #'desktop-environment-screenshot-part)
  (exwm-input-set-key (kbd "<print>") #'desktop-environment-screenshot)
  (exwm-input-set-key (kbd "<XF86WLAN>") #'desktop-environment-toggle-wifi)
  (exwm-input-set-key (kbd "<XF86Bluetooth>") #'desktop-environment-toggle-bluetooth)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-"))

;; logout function
(defun my-logout ()
  (interactive)
  (shell-command "gnome-screensaver-command -l"))

;; keyboard setup
(defun keys/keyboard-setup ()
  (interactive)
  ;; Rebind CapsLock to Esc
  (start-process-shell-command "qwerty" nil "setxkbmap -option caps:escape us,us_intl '' compose:ralt grp:rctrl_rshift_toggle"))

(defun xrandr/pixels-to-number-list (pixels)
  (if (cl-search ":" pixels)
    (mapcar 'string-to-number (split-string pixels ":"))
    pixels))

(defun xrandr/entries-format (xrandr-entry)
  (mapcar 'xrandr/pixels-to-number-list xrandr-entry))

(defun xrandr/entry-position (xrandr-entry)
 (car (cdr xrandr-entry)))

(defun xrandr/entry-position-x (xrandr-entry)
  (car (xrandr/entry-position xrandr-entry)))

(defun xrandr/entry-position-y (xrandr-entry)
  (car (cdr (xrandr/entry-position xrandr-entry))))

(defun xrandr/entry-resolution (xrandr-entry)
 (car (cdr (cdr xrandr-entry))))

(defun xrandr/entry-resolution-x (xrandr-entry)
  (car (xrandr/entry-resolution xrandr-entry)))

(defun xrandr/entry-resolution-y (xrandr-entry)
  (car (cdr (xrandr/entry-resolution xrandr-entry))))

;; This will return an order list of monitors (from left to right)
;; Format is as following:
;; (("monitor-1" ("position-x" "position-y") ("resolution-x" "resolution-y")))
(defun xrandr/build-active-monitors ()
      (setq xrandr/active-monitors
        (mapcar 'xrandr/entries-format
          (mapcar 'reverse
            (remove nil
                    (mapcar 'split-string
                            (split-string
                             (shell-command-to-string "xrandr --listactivemonitors | cut -d ' ' -f4-6 | sed -e 's|/[0-9]*x|x|g' -e 's|/[0-9]*+| |g' -e 's/[x|+]/:/g'")
                             "\n")))))))

(defun exwm/build-workspace-monitor (monitor current_workspace max_workspace)
  (if (> current_workspace max_workspace)
      '()
    (append (list current_workspace monitor) (exwm/build-workspace-monitor monitor (+ current_workspace 1) max_workspace))))

(defun exwm/build-monitors-aux (current_workspace monitor-list)
  (cond
   ((equal (length monitor-list) 1)
    (list 9 (car monitor-list) 0 (car monitor-list))
    )
   ((equal (length monitor-list) 2)
    (append (exwm/build-workspace-monitor (car monitor-list) current_workspace 8) (exwm/build-monitors-aux (+ current_workspace 2) (cdr monitor-list)))
    )
   (t
    (append (exwm/build-workspace-monitor (car monitor-list) current_workspace (+ current_workspace 1)) (exwm/build-monitors-aux (+ current_workspace 2) (cdr monitor-list))))))

(defun exwm/build-monitors ()
  (xrandr/build-active-monitors)
  (exwm/build-monitors-aux 1 (mapcar 'car xrandr/active-monitors)))

;; This defines a function to refresh the workspaces position and xrandr
(defun exwm/refresh-monitors ()
  (interactive)
  (exwm/autorandr-refresh)
  (setq exwm-randr-workspace-monitor-plist (exwm/build-monitors)))

;; Display time every minute. will be used to display time and battery to a buffer displayed in child fames
  (require 'battery)

  (defun panel/battery ()
    (setq battery-string (replace-regexp-in-string "\\[" ""
      (replace-regexp-in-string "\\+" ""
        (replace-regexp-in-string "%]" ""
          (battery-format battery-mode-line-format (funcall battery-status-function))))))
    (setq battery-value (string-to-number battery-string))
    (setq battery-icon
      (if (and (> battery-value 95))
         ""
         (if (and (< battery-value 96) (> battery-value 60))
           ""
           (if (and (< battery-value 61) (> battery-value 25))
             ""
             (if (and (< battery-value 26) (> battery-value 2))
               "" 
               "")))))
    (concat battery-icon "  " battery-string "%"))

    (panel/battery)

  (defun panel/time ()
    (setq current-date-time-format "%a %d %b %Y %H:%M")
    (format-time-string current-date-time-format (current-time)))

  (defun panel/print ()
    (concat (panel/time) "   " (panel/battery)))

  (defun panel/write-buffer ()
    (setq my-panel-buffer (get-buffer-create "*panel*"))
    (with-current-buffer "*panel*" ; replace with the name of the buffer you want to append
      (erase-buffer)
      (insert (panel/print))))

  (defun utils/get-next-minute ()
    (setq hour-minute-format "%H:%M")
    (format-time-string hour-minute-format (time-add (current-time) (seconds-to-time 60))))

  (panel/write-buffer)
  (setq panel/timer (run-at-time (utils/get-next-minute) 60 'panel/write-buffer))

  (setq panel/length 0)

  (add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (cond
             ((equal (frame-parameter frame 'name) "panel-frame")
              (let ((window (frame-root-window frame)))
                (set-window-parameter window 'mode-line-format 'none)
                (set-window-parameter window 'header-line-format 'none))
              (display-buffer "*panel*" nil nil)
              (setq panel/length (point-max))))
            (other-window -1)))

  (setq panel/list '())

(defun default-font-width () 
  "Return the width in pixels of a character in the current
window's default font.  More precisely, this returns the
width of the letter ‘m’.  If the font is mono-spaced, this
will also be the width of all other printable characters."
  (let ((window (selected-window))
        (remapping face-remapping-alist))
    (with-temp-buffer
      (make-local-variable 'face-remapping-alist)
      (setq face-remapping-alist remapping)
      (set-window-buffer window (current-buffer))
      (insert "m")
      (aref (aref (font-get-glyphs (font-at 1) 1 2) 0) 4))))

  ;; Width is the frame width
  (defun panel/get-width ()
    238)
     ;; (+ (* panel/length (default-font-width)) 4))

  ;; Height is character height + 4 pixels (2 pixels arround the text)
  (defun panel/get-height ()
    (+ (aref (font-info (face-font 'default)) 2) 4))

(defun panel/resize-and-position (frame xrandr-entry)
 ;; (set-frame-size frame panel/length 1)
 (set-frame-size frame 34 1)
 (set-frame-position frame
                     (- (+ (xrandr/entry-position-x xrandr-entry) (xrandr/entry-resolution-x xrandr-entry)) (+ (panel/get-width) 90))
                     (- (+ (xrandr/entry-position-y xrandr-entry) (xrandr/entry-resolution-y xrandr-entry)) (panel/get-height))))

  (defun panel/make-frame (xrandr-entry)
    (setq current-panel (make-frame
     `((name . "panel-frame")
       (parent-frame . nil)
       (no-accept-focus . nil)
       (window-min-width . 1)
       (window-min-height . 1)
       (min-width  . t)
       (min-height . t)
       (border-width . 0)
       (internal-border-width . 0)
       (vertical-scroll-bars . nil)
       (horizontal-scroll-bars . nil)
       (left-fringe . 10)
       (right-fringe . 0)
       (menu-bar-lines . 0)
       (tool-bar-lines . 0)
       (line-spacing . 0)
       (unsplittable . t)
       (no-other-frame . t)
       (undecorated . t)
       (unsplittable . t)
       (cursor-type . nil)
       (minibuffer . nil)
       (no-special-glyphs . t))))
    (push current-panel panel/list)
    (panel/resize-and-position current-panel xrandr-entry))

  (defun panel/hide ()
    (interactive)
    (cl-loop for frame in panel/list
      collect (delete-frame frame))
    (setq panel/list '()))

  (defun panel/display ()
    (interactive)
    (panel/hide)
    (cl-loop for xrandr-entry in xrandr/active-monitors
      do (panel/make-frame xrandr-entry)))

(defun app/qutebrowser ()
  (interactive)
  (shell/async-command-no-output "qutebrowser"))

(defun app/teams ()
  (interactive)
  (shell/async-command-no-output "teams"))

(defun app/arandr ()
  (interactive)
  (shell/async-command-no-output "arandr"))

(defcustom my-skippable-buffer-regexp
  (rx bos (or (seq "*" (zero-or-more anything))
              (seq "magit" (zero-or-more anything))
              (seq "qutebrowser" (zero-or-more anything))
              (seq "Firefox" (zero-or-more anything)))
      eos)
  "Matching buffer names are ignored by `my-next-buffer'
        and `my-previous-buffer'."
  :type 'regexp)

;; only switch to next relevant buffer
(defcustom my-browser-buffer-regexp
  (rx bos (or (seq "qutebrowser" (zero-or-more anything))
              (seq "Firefox" (zero-or-more anything)))
      eos)
  "Matching only browser windows"
  :type 'regexp)

(defun my-change-buffer (change-buffer buffer-to-skip)
  "Call CHANGE-BUFFER until `buffer-to-skip' doesn't match."
  (let ((initial (current-buffer)))
    (funcall change-buffer)
    (let ((first-change (current-buffer)))
      (catch 'loop
        (while (funcall buffer-to-skip)
          (funcall change-buffer)
          (when (eq (current-buffer) first-change)
            (switch-to-buffer initial)
            (throw 'loop t)))))))

(defun my-next-buffer ()
  "Variant of `next-buffer' that skips `my-skippable-buffer-regexp'."
  (interactive)
  (my-change-buffer 'next-buffer (lambda () (string-match-p my-skippable-buffer-regexp (buffer-name)))))

(defun my-previous-buffer ()
  "Variant of `previous-buffer' that skips `my-skippable-buffer-regexp'."
  (interactive)
  (my-change-buffer 'previous-buffer (lambda () (string-match-p my-skippable-buffer-regexp (buffer-name)))))

(defun my-next-browser ()
  "Variant of `next-buffer' that skips `my-skippable-buffer-regexp'."
  (interactive)
  (my-change-buffer 'next-buffer (lambda () (not (string-match-p my-browser-buffer-regexp (buffer-name))))))

(defun my-previous-browser ()
  "Variant of `previous-buffer' that skips `my-skippable-buffer-regexp'."
  (interactive)
  (my-change-buffer 'previous-buffer (lambda () (not (string-match-p my-browser-buffer-regexp (buffer-name))))))

(defun my-window-vsplit ()
  (interactive)
  (evil-window-vsplit)
  (balance-windows)
  (run-at-time "0.1 seconds" nil (lambda ()
                                   (windmove-right))))

(defun my-window-split ()
  (interactive)
  (evil-window-split)
  (run-at-time "0.1 seconds" nil (lambda ()
                                   (windmove-down))))

(defun exwm/exwm-init-hook ()
  (keys/keyboard-setup)
  ;; Launch apps that will run in the background
  (shell/run-in-background "nm-applet")
  (shell/run-in-background "pasystray")
  (shell/run-in-background "blueman-applet"))

(defun exwm/win-title ()
  (replace-regexp-in-string (concat " . " exwm-class-name) "" exwm-title))

(defun exwm/exwm-update-title ()
  (exwm-workspace-rename-buffer
  (concat exwm-class-name ": "
         (if (<= (length exwm-title) 100) exwm-title
           (concat (substring exwm-title 0 99) "...")))))

(use-package exwm
  :config
  (keys/leader-keys
    "a"  '(:ignore t :which-key "applications")
    "aa" '(app/qutebrowser :which-key " Qutebrowser")
    "at" '(app/teams :which-key " Teams")
    "s"  '(:ignore t :which-key "Settings")
    "sk" '(keys/keyboard-setup :which-key " Qwerty")
    "sm" '(app/arandr :which-key " Monitors")
    )

  ;; When EXWM starts up, do some extra confifuration
  (add-hook 'exwm-init-hook #'exwm/exwm-init-hook)

  ;; Automatically move EXWM buffer to current workspace when selected
  (setq exwm-layout-show-all-buffers t)

  ;; Display all EXWM buffers in every workspace buffer list
  (setq exwm-workspace-show-all-buffers t)

  ;; Automatically send the mouse cursor to the selected workspace's display
  (setq exwm-workspace-warp-cursor t)

  ;; These keys should always pass through to Emacs
  (add-to-list 'exwm-input-prefix-keys ?\s-d)

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\s-,] 'exwm-input-send-next-key)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)
          ([?\s-R] . exwm-input-release-keyboard)

          ;; refresh monitors
          ([?\s-M] . exwm/refresh-monitors)

          ([?\s-=] . balance-windows)
          ([?\s-+] . zoom)
          ([?\s-G] . zoom-mode)

          ;; move to another window using switch-window
          ([?\s-o] . ace-window)
          ([?\s-O] . ace-swap-window)

          ;; easy window switching
          ([?\s-h] . evil-window-left)
          ([?\s-k] . evil-window-up)
          ([?\s-j] . evil-window-down)
          ([?\s-l] . evil-window-right)

          ([s-left] . evil-window-left)
          ([s-up] . evil-window-up)
          ([s-down] . evil-window-down)
          ([s-right] . evil-window-right)

          ;; easy window moving
          ([?\s-H] . windmove-swap-states-left)
          ([?\s-J] . windmove-swap-states-down)
          ([?\s-K] . windmove-swap-states-up)
          ([?\s-L] . windmove-swap-states-right)

          ([S-s-left] . windmove-swap-states-left)
          ([S-s-down] . windmove-swap-states-down)
          ([S-s-up] . windmove-swap-states-up)
          ([S-s-right] . windmove-swap-states-right)

          ;; easy window resize
          ;; ([C-s-h] . windsize-left)
          ;; ([C-s-j] . windsize-down)
          ;; ([C-s-k] . windsize-up)
          ;; ([C-s-l] . windsize-right)

          ([C-s-left] . windsize-left)
          ([C-s-down] . windsize-down)
          ([C-s-up] . windsize-up)
          ([C-s-right] . windsize-right)

          ([?\s-V] . my-window-vsplit)
          ([?\s-S] . my-window-split)

          ([?\s-u] . winner-undo)
          ([?\s-U] . winner-redo)

          ([?\s-b] . exwm-workspace-switch-to-buffer)
          ([?\s-B] . ibuffer)

          ([s-tab] . my-next-buffer)
          ([s-iso-lefttab] . my-previous-buffer)

          ([?\s-i] . my-next-browser)
          ([?\s-I] . my-previous-browser)

          ([?\s-t] . treemacs)

          ([?\s-e] . ranger)
          ([?\s-E] . deer)

          ([?\s-W] . delete-other-windows)
          ([?\s-w] . delete-window)
          ([?\s-X] . kill-current-buffer)
          ([?\s-Q] . (lambda () (interactive) (kill-current-buffer) (delete-window)))

          ([?\s-f] . exwm-layout-toggle-fullscreen)
          ([?\s-F] . exwm-floating-toggle-floating)

          ([?\s-T] . my-logout)
          ([?\s-x] . counsel-M-x)
          ([s-backspace] . counsel-M-x)
          ([?\s-.] . counsel-find-file)

          ([?\s-a] . counsel-linux-app)
          ([s-return] . vterm)
          ([S-s-return] . vterm)

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))
          ))

  ;; Send copy/paste easily
  (setq exwm-input-simulation-keys
        `(
          ([?\s-p] . [?\C-v])
          ([?\s-y] . [?\C-c])
          ))

  ;; Should be set in the previous list but does not work atm
  (exwm-input-set-key (kbd "C-s-h") #'windsize-left)
  (exwm-input-set-key (kbd "C-s-l") #'windsize-right)
  (exwm-input-set-key (kbd "C-s-j") #'windsize-down)
  (exwm-input-set-key (kbd "C-s-k") #'windsize-up)

  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  (exwm-enable)

  (exwm/refresh-monitors)
  ;; This is for multiscreen support
  (require 'exwm-randr)
  (exwm/refresh-monitors)
  (exwm-randr-enable)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'exwm/exwm-update-title)

  ;; When window title updates, use it to set the buffer name
  (add-hook 'exwm-update-title-hook #'exwm/exwm-update-title)

  ;; When randr changes, refresh monitor setup
  (add-hook 'exwm-randr-screen-change-hook 'exwm/refresh-monitors))
