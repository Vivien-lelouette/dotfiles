(defun shell/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun shell/async-command-no-output (command)
  (call-process-shell-command (concat command " &") nil 0))

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

(defun car-string-to-number (list-two-elements)
  (append (list (string-to-number (car list-two-elements))) (cdr list-two-elements)))

(defun sort-from-car (a b)
  (< (car a) (car b)))

(defun xrandr-active-monitors ()
  (mapcar 'car-string-to-number
          (remove nil
                  (mapcar 'split-string
                          (split-string
                           (shell-command-to-string "xrandr --listactivemonitors | grep / | cut -d '/' -f3 | sed -e 's/^[0-9]\\++//g' -e 's/+[0-9]\\+//g'")
                           "\n")))))

(defun order-monitors ()
  (apply #'append
    (mapcar 'cdr
      (sort
       (xrandr-active-monitors)
       'sort-from-car))))

(defun build-workspace-monitor (monitor current_workspace max_workspace)
  (if (> current_workspace max_workspace)
      '()
    (append (list current_workspace monitor) (build-workspace-monitor monitor (+ current_workspace 1) max_workspace))))

(defun build-exwm-monitors-aux (current_workspace monitor-list)
  (cond
   ((equal (length monitor-list) 1)
    (list 9 (car monitor-list) 0 (car monitor-list))
    )
   ((equal (length monitor-list) 2)
    (append (build-workspace-monitor (car monitor-list) current_workspace 8) (build-exwm-monitors-aux (+ current_workspace 2) (cdr monitor-list)))
    )
   (t
    (append (build-workspace-monitor (car monitor-list) current_workspace (+ current_workspace 1)) (build-exwm-monitors-aux (+ current_workspace 2) (cdr monitor-list))))))

(defun build-exwm-monitors ()
  (build-exwm-monitors-aux 1 (order-monitors)))

;; This defines a function to refresh the workspaces position and xrandr
(defun exwm/refresh-monitors ()
  (interactive)
  (exwm/autorandr-refresh)
  (setq exwm-randr-workspace-monitor-plist (build-exwm-monitors)))

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

(defun +ivy-posframe-display-exwm (str)
  (ivy-posframe--display str
   (lambda (info)
     (let* ((workarea (elt exwm-workspace--workareas exwm-workspace-current-index))
            (x (aref workarea 0))
            (y (aref workarea 1))

            (fw (aref workarea 2))
            (fh (aref workarea 3))

            (pw (plist-get info :posframe-width))
            (ph (plist-get info :posframe-height)))

       (cons (+ x (/ (- fw pw) 2)) (+ y (/ (- fh ph) 2)))))))

(use-package exwm
  :config
  (setq ivy-posframe-display-functions-alist
      '((t . +ivy-posframe-display-exwm))

      ivy-posframe-parameters '((parent-frame nil)
                                (z-group . above)
                                (left-fringe . 20)
                                (right-fringe . 20)))

  ;; force set frame-position on every posframe display
  (advice-add 'posframe--set-frame-position :before
          (lambda (&rest args)
            (setq-local posframe--last-posframe-pixel-position nil)))

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

          ([?\s-b] . exwm-workspace-switch-to-buffer)
          ([?\s-B] . ibuffer)

          ([s-tab] . my-next-buffer)
          ([s-iso-lefttab] . my-previous-buffer)

          ([?\s-i] . my-next-browser)
          ([?\s-I] . my-previous-browser)

          ([?\s-t] . treemacs)

          ([?\s-e] . ranger)
          ([?\s-E] . deer)

          ([?\s-W] . delete-window)
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

;; Display time every minute. will be used to display time and battery to a buffer displayed in child fames
  (require 'battery)

  (defun panel/battery ()
    (setq battery-string (replace-regexp-in-string "\\[" ""
      (replace-regexp-in-string "\\..\\%]" ""
        (battery-format battery-mode-line-format (funcall battery-status-function)))))
    (setq battery-value (string-to-number battery-string))
    (setq battery-icon
      (if (and (> battery-value 95))
         ""
         (if (and (< battery-value 95) (> battery-value 60))
           ""
           (if (and (< battery-value 60) (> battery-value 25))
             ""
             (if (and (< battery-value 25) (> battery-value 2))
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

  (add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (cond
             ((equal (frame-parameter frame 'name) "panel-frame")
              (let ((window (frame-root-window frame)))
                (set-window-parameter window 'mode-line-format 'none)
                (set-window-parameter window 'header-line-format 'none))
              (display-buffer "*panel*" nil nil)))
            (other-window -1)))

  (setq panel/list '())

  (defun panel/make-frame (x-offset)
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
       (width . 40)
       (height . 0)
       (no-special-glyphs . t))))
    (push current-panel panel/list)
    (set-frame-position current-panel (+ (* x-offset 1920) 1630) 1064)
    (setq index-monitor (+ index-monitor 1)))

(defun panel/clean-frames ()
  (cl-loop for frame in panel/list
    collect (delete-frame frame))
  (setq panel/list '()))

  (message panel/list)
  (defun panel/display ()
    (interactive)
    (panel/clean-frames)
    (setq index-monitor 0)
    (cl-loop for monitor in (order-monitors)
      do (panel/make-frame index-monitor)))
