(defun shell/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun shell/async-command-no-output (command)
  (call-process-shell-command (concat command " &") nil 0))

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

(defun order-monitors ()    (apply #'append
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
  (setq exwm-randr-workspace-monitor-plist (build-exwm-monitors))

  ;; Start the Polybar panel
  (shell/run-in-background "~/.config/polybar/start_polybar.sh"))

;; logout function
(defun my-logout ()
  (interactive)
  (shell-command "gnome-session-quit --force"))

;; keyboard setup
(defun keys/keyboard-setup ()
  (interactive)
  ;; Rebind CapsLock to Esc
  (start-process-shell-command "qwerty" nil "setxkbmap -layout us -option 'compose:rctrl'"))

(use-package app-launcher
  :straight '(app-launcher :host github :repo "SebastienWae/app-launcher"))

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
  (exwm/refresh-monitors)
  ;; Launch apps that will run in the background
  (shell/run-in-background "gsettings set org.gnome.gnome-flashback.desktop.icons show-home false")
  (shell/run-in-background "gsettings set org.gnome.gnome-flashback.desktop.icons show-trash false"))

(defun exwm/win-title ()
  (replace-regexp-in-string (concat " . " exwm-class-name) "" exwm-title))

(defun exwm/exwm-update-title ()
  (exwm-workspace-rename-buffer
   (concat exwm-class-name ": "
           (if (<= (length exwm-title) 100) exwm-title
             (concat (substring exwm-title 0 99) "...")))))


(defun exwm/exwm-set-fringe ()
  (setq left-fringe-width 4)
  (setq right-fringe-width 4))

(use-package exwm
  :config
  (winner-mode 1)
  (keys/keyboard-setup)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'exwm/exwm-update-title)

  ;; When window title updates, use it to set the buffer name
  (add-hook 'exwm-update-title-hook #'exwm/exwm-update-title)

  ;; When EXWM starts up, do some extra confifuration
  (add-hook 'exwm-init-hook #'exwm/exwm-init-hook)

  (add-hook 'exwm-mode-hook #'exwm/exwm-set-fringe)

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
          ([C-s-R] . exwm-reset)
          ([?\s-r] . exwm-input-release-keyboard)

          ([?\s-/] . winner-undo)
          ([?\s-?] . winner-redo)

          ;; refresh monitors
          ([?\s-R] . exwm/refresh-monitors)
          ([?\s-x] . execute-extended-command)
          ;; move to another window using switch-window
          ([?\s-o] . ace-window)
          ([?\s-O] . ace-swap-window)

          ([?\s-m] . exwm-layout-toggle-fullscreen)
          ([?\s-M] . exwm-floating-toggle-floating)

          ([?\s-L] . (shell/run-in-background "gnome-screensaver-command -l"))

          ([S-s-x] . execute-extended-command)

          ([?\s-a] . app-launcher-run-app)
          ([s-return] . eshell)
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
          ([?\s-y] . [?\C-v])
          ([?\s-w] . [?\C-c])
          ))

  (exwm-enable)
  (exwm/refresh-monitors)
  ;; This is for multiscreen support
  (require 'exwm-randr)
  (add-hook 'exwm-randr-screen-change-hook 'exwm/refresh-monitors)
  (exwm-randr-enable)
  (load-theme 'modus-vivendi t))
