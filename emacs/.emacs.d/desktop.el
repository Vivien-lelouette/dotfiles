(defun shell/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun shell/async-command-no-output (command)
  (call-process-shell-command (concat command " &") nil 0))

(defun apps/chrome-browser ()
  (interactive)
  (shell/async-command-no-output "google-chrome-stable"))

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

(defun build-exwm-monitors ()
  (setq 
   index 0
   exwm-monitor-workspaces (list ))

  (let ((monitors (order-monitors)))
    (while (< index (length monitors))
      (progn
        (setq index (1+ index))
        (add-to-list 'exwm-monitor-workspaces index t)
        (add-to-list 'exwm-monitor-workspaces (nth (1- index) monitors) t))))
  exwm-monitor-workspaces)

;; This defines a function to refresh the workspaces position and xrandr
(defun exwm/refresh-setup ()
  (interactive)
  (setq exwm-randr-workspace-monitor-plist (build-exwm-monitors))
  ;;(shell/run-in-background "~/.config/polybar/start_polybar.sh")
  (setup/input))

(defun exwm/refresh-setup-and-monitors ()
  (interactive)
  (exwm/refresh-setup)
  (exwm-randr-refresh))

(defun setup/input ()
  (interactive)
  (start-process-shell-command "trackball" nil "bash ~/.scripts/trackball-setup.sh"))

(use-package app-launcher
  :straight '(app-launcher :host github :repo "SebastienWae/app-launcher"
                            :fork (:host github
                                        :repo "vivien-lelouette/app-launcher")))

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

(defun exwm/force-tiled-fullscreen-when-fullscreen ()
  (interactive)
  (with-current-buffer (window-buffer)
    (exwm-layout-unset-fullscreen exwm--id))
  (remove-hook 'exwm-input-input-mode-change-hook #'exwm/force-tiled-fullscreen-when-fullscreen))

(defun window/force-tiled-fullscreen ()
  "Toggle fullscreen mode."
  (interactive)
  (execute-kbd-macro (kbd "<f11>"))
  (add-hook 'exwm-input-input-mode-change-hook #'exwm/force-tiled-fullscreen-when-fullscreen)
  (run-with-timer 0.1 nil (lambda ()
                            (pcase exwm-class-name
                              ("Google-chrome" (execute-kbd-macro (kbd "C-l"))))))
  (run-with-timer 2 nil (lambda ()
                            (remove-hook 'exwm-input-input-mode-change-hook #'exwm/force-tiled-fullscreen-when-fullscreen))))

(defun window/force-tile ()
  (interactive)
  (with-current-buffer (window-buffer)
    (exwm-floating--unset-floating exwm--id)))

(defun window/force-tile-to-other-window ()
  (interactive)
  (window/force-tile)
  (with-current-buffer (window-buffer)
    (aw-move-window (next-window))))

(defun window/configure-window-by-class ()
  (interactive)
  (pcase exwm-class-name
    ((rx (sequence "Ardour" (zero-or-more (any "ascii")))) (window/force-tile-to-other-window))
    ("Google-chrome" (window/force-tiled-fullscreen))))

(add-hook 'exwm-manage-finish-hook #'window/configure-window-by-class)

(setq helm-ag-show-status-function (lambda ()))
(defun exwm/exwm-init-hook ()
  (exwm/refresh-setup))
;; Launch apps that will run in the background
;;(shell/run-in-background "gsettings set org.gnome.gnome-flashback.desktop.icons show-home false")
;;(shell/run-in-background "gsettings set org.gnome.gnome-flashback.desktop.icons show-trash false"))

(defun exwm/win-title ()
  (replace-regexp-in-string (concat " . " exwm-class-name) "" exwm-title))

(defun exwm/exwm-update-title ()
  (exwm-workspace-rename-buffer
   (concat
    (if (window-parameter (selected-window) 'split-window)
        " "
      "")
    exwm-class-name ": "
    (if (<= (length exwm-title) 100) exwm-title
      (concat (substring exwm-title 0 99) "...")))))


(defun exwm/exwm-set-fringe ()
  (setq left-fringe-width 1
        right-fringe-width 1))

(defun exwm/kill-current-buffer-and-window ()
  (interactive)
  (kill-current-buffer)
  (delete-window))

(defun kde/lock-screen ()
  (interactive)
  (shell/run-in-background "loginctl lock-session"))

(defun kde/logout ()
  (interactive)
  (shell/run-in-background "loginctl terminate-session"))

(defun kde/shutdown ()
  (interactive)
  (shell/run-in-background "shutdown -h 0"))

(defun kde/reboot ()
  (interactive)
  (shell/run-in-background "reboot"))

(defun kwin/replace ()
  (interactive)
  (shell/run-in-background "kwin_x11 --replace"))

(defun settings/manager ()
  (interactive)
  (shell/run-in-background "systemsettings5"))

(defun settings/appearance ()
  (interactive)
  (shell/run-in-background "systemsettings5 kcm_lookandfeel"))

(defun settings/display ()
  (interactive)
  (shell/run-in-background "systemsettings5 kcm_kscreen"))

(defun settings/keyboard ()
  (interactive)
  (shell/run-in-background "systemsettings5 kcm_keyboard"))

(defun settings/mouse ()
  (interactive)
  (shell/run-in-background "systemsettings5 kcm_mouse"))

(defun settings/network ()
  (interactive)
  (shell/run-in-background "systemsettings5 kcm_networkmanagement"))

(defun settings/sound ()
  (interactive)
  (shell/run-in-background "systemsettings5 kcm_pulseaudio"))

(defun warpd/hint ()
  (interactive)
  (shell/run-in-background "warpd --oneshot --foreground --hint"))

(use-package exwm
  :config
  (setq x-no-window-manager t)
  (winner-mode 1)
  (setup/input)

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

  ;; god-mode integration
  ;; (add-hook 'god-mode-enabled-hook (lambda () (setq exwm-input-line-mode-passthrough t)))
  ;; (add-hook 'god-mode-disabled-hook (lambda () (setq exwm-input-line-mode-passthrough nil))

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; mouse jump
          ([?\s-\;] . warpd/hint)

          ;; refresh setup
          ([?\s-r] . exwm-reset)
          ([?\s-R] . exwm/refresh-setup-and-monitors)

          ([?\s-i] . exwm-input-release-keyboard)
          ([?\s-I] . exwm-input-grab-keyboard)

          ([?\s-/] . winner-undo)
          ([?\s-?] . winner-redo)

          ([?\s-x] . execute-extended-command)

          ;; move to another window using switch-window
          ([?\s-j] . ace-window)
          ([?\s-J] . ace-swap-window)

          ([?\s-}] . enlarge-window)
          ([?\s-{] . shrink-window)
          ([?\s-\[] . shrink-window-horizontally)
          ([?\s-\]] . enlarge-window-horizontally)
          ([?\s-=] . balance-windows)
          ([?\s-+] . zoom)

          ([?\s-k] . kill-current-buffer)
          ([?\s-K] . exwm/kill-current-buffer-and-window)

          ([?\s-m] . exwm-layout-toggle-fullscreen)
          ([?\s-M] . exwm-floating-toggle-floating)
          ([?\s-n] . window/force-tiled-fullscreen)

          ([?\s-p ?\s-l] . kde/lock-screen)
          ([?\s-p ?\M-l] . kde/logout)
          ([?\s-p ?\M-s] . kde/shutdown)
          ([?\s-p ?\M-r] . kde/reboot)
          ([?\s-p ?\M-w] . kwin/replace)

          ([?\s-o ?\s-o] . settings/manager)
          ([?\s-o ?\s-a] . settings/appearance)
          ([?\s-o ?\s-d] . settings/display)
          ([?\s-o ?\s-k] . settings/keyboard)
          ([?\s-o ?\s-m] . settings/mouse)
          ([?\s-o ?\s-n] . settings/network)
          ([?\s-o ?\s-s] . settings/sound)

          ([?\s-a] . app-launcher-run-app)

          ([?\s-b] . consult-buffer)
          ([?\s-B] . ibuffer-jump)

          ([?\s-f] . consult-bookmark)
          ([?\s-F] . blist)

          ([s-return] . eshell)
          ([S-s-return] . multi-term)
          ([C-s-return] . utils/x-terminal)

          ([?\s-q] . delete-window)
          ([?\s-Q] . delete-other-windows)
          ([?\s-S] . split-window-below)
          ([?\s-s] . split-window-right)

          ;; Applications
          ([?\s-c] . chrome/do-start-with-url-or-search)
          ([?\s-C] . apps/chrome-browser)

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-w s %d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))

          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-w %d" i)) .
                        (lambda ()
                          (interactive)
                          (tab-bar-select-tab ,i))))
                    (number-sequence 0 9))

          ([?\s-w ?\s-w] . tab-bar-new-tab)
          ([?\s-w ?\s-W] . tab-bar-duplicate-tab)
          ([?\s-w ?\s-k] . tab-bar-close-tab)

          ([?\s-w ?\s-n] . tab-bar-switch-to-next-tab)
          ([?\s-w ?\s-p] . tab-bar-switch-to-prev-tab)


          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (select-window (nth (- ,i 1) (aw-window-list))))))
                    (number-sequence 1 9))

          ([?\s-0] . (lambda ()
                       (interactive)
                       (select-window (nth 9 (aw-window-list)))))

          ([?\s-!] . (lambda ()
                       (interactive)
                       (aw-move-window (nth 0 (aw-window-list)))))

          ([?\s-@] . (lambda ()
                       (interactive)
                       (aw-move-window (nth 1 (aw-window-list)))))

          ([?\s-#] . (lambda ()
                       (interactive)
                       (aw-move-window (nth 2 (aw-window-list)))))

          ([?\s-$] . (lambda ()
                       (interactive)
                       (aw-move-window (nth 3 (aw-window-list)))))

          ([?\s-%] . (lambda ()
                       (interactive)
                       (aw-move-window (nth 4 (aw-window-list)))))

          ([?\s-^] . (lambda ()
                       (interactive)
                       (aw-move-window (nth 5 (aw-window-list)))))

          ([?\s-&] . (lambda ()
                       (interactive)
                       (aw-move-window (nth 6 (aw-window-list)))))

          ([?\s-*] . (lambda ()
                       (interactive)
                       (aw-move-window (nth 7 (aw-window-list)))))

          ([?\s-\(] . (lambda ()
                        (interactive)
                        (aw-move-window (nth 8 (aw-window-list)))))

          ([?\s-\)] . (lambda ()
                        (interactive)
                        (aw-move-window (nth 9 (aw-window-list)))))

          ,@(mapcar (lambda (i)
                      `(,(kbd (format "M-s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (aw-swap-window (nth (- ,i 1) (aw-window-list))))))
                    (number-sequence 1 9))

          ([M-s-0] . (lambda ()
                       (interactive)
                       (aw-swap-window (nth 9 (aw-window-list)))))

          ,@(mapcar (lambda (i)
                      `(,(kbd (format "C-s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (aw-delete-window (nth (- ,i 1) (aw-window-list))))))
                    (number-sequence 1 9))

          ([C-s-0] . (lambda ()
                       (interactive)
                       (aw-delete-window (nth 9 (aw-window-list)))))
          ))

  ;; Send copy/paste easily
  (setq exwm-input-simulation-keys
        '(
          ([?\M-b] . [C-left])
          ([?\M-f] . [C-right])
          ([?\C-b] . [left])
          ([?\C-f] . [right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])

          ([?\M-B] . [C-\S-left])
          ([?\M-F] . [C-\S-right])
          ([?\C-\S-b] . [S-left])
          ([?\C-\S-f] . [S-right])
          ([?\C-\S-p] . [S-up])
          ([?\C-\S-n] . [S-down])
          ([?\C-\S-a] . [S-home])
          ([?\C-\S-e] . [S-end])

          ([?\C-s] . [?\C-f])
          ([?\C-x ?\C-s] . [?\C-s])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])

          ([?\M-d] . [C-delete])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end ?\C-x])

          ([?\C-y] . [?\C-v])
          ([?\M-w] . [?\C-c])
          ([?\C-w] . [?\C-x])
          ([?\s-g] . [escape])))

  (setq exwm-manage-configurations '(((string-match-p "^Xfce4-" exwm-title)
                                      floating nil)))

  (setq exwm-replace t)

  (exwm-enable)
  (exwm/refresh-setup)
  ;; This is for multiscreen support
  (require 'exwm-randr)
  (add-hook 'exwm-randr-screen-change-hook 'exwm/refresh-setup)
  (exwm-randr-enable)
  (require 'exwm-systemtray)
  (exwm-systemtray-enable))

(use-package exwm-edit
  :straight (exwm-edit :type git :host github :repo "agzam/exwm-edit"))

  (setq exwm-edit-split  "below")

  (defun exwm-edit--finish-and-press-return ()
    (interactive)
    (exwm-edit--finish)
    (run-with-timer 0.2 nil (lambda () (exwm-input--fake-key 'return))))
  (defun exwm-edit--finish-and-press-control-return ()
    (interactive)
    (exwm-edit--finish)
    (run-with-timer 0.2 nil (lambda () (exwm-input--fake-key 'C-return))))

  (add-hook 'exwm-edit-mode-hook
    (lambda ()
      (define-key exwm-edit-mode-map (kbd "C-c <return>") 'exwm-edit--finish-and-press-return)
      (define-key exwm-edit-mode-map (kbd "C-c C-<return>") 'exwm-edit--finish-and-press-control-return)))

(defvar chrome/input-history nil)
(eval-after-load "savehist"
  '(add-to-list 'savehist-additional-variables 'chrome/input-history))

(defun chrome/do-start-with-url-or-search ()
  (interactive)
  (if (string= exwm-class-name "Google-chrome")
      (let ((split-title (mapcar (lambda (str) (s-replace " - Google Chrome" "" str)) (split-string exwm-title " - http"))))
        (message (concat "http" (car (last split-title))))
        (chrome/start-with-url-or-search (completing-read "URL or search " chrome/input-history nil nil (concat "http" (car (last split-title))) 'chrome/input-history)))
    (chrome/start-with-url-or-search (completing-read "URL or search " chrome/input-history nil nil nil 'chrome/input-history))))

(defun chrome/start-with-url-or-search (input)
  (interactive)
  (let ((trimmed-input (string-trim input)))
    (if (string-match-p " " trimmed-input)
        (shell/async-command-no-output (concat "google-chrome-stable --new-window '? " input "'"))
      (shell/async-command-no-output (concat "google-chrome-stable --new-window '" input "'"))))
  (run-with-timer 0.5 nil (lambda () (window/force-tiled-fullscreen))))

(defun bookmark/chrome-bookmark-handler (record)
  "Jump to an chrome bookmarked location."
  (with-current-buffer (window-buffer)
    (chrome/start-with-url-or-search (bookmark-prop-get record 'location))))

(defun bookmark/chrome-bookmark-make-record ()
  "Return a bookmark record for the current chrome buffer."
  (interactive)
  (let ((split-title (mapcar (lambda (str) (s-replace " - Google Chrome" "" str)) (split-string exwm-title " - http"))))
    `(,(concat "chrome/" (car split-title))
      (location . ,(concat "http" (car (last split-title))))
      (handler . bookmark/chrome-bookmark-handler))))

(defun bookmark/chrome-set-bookmark-handler ()
  "This tells Emacs which function to use to create bookmarks."
  (interactive)
  (if (string= exwm-class-name "Google-chrome")
      (set (make-local-variable 'bookmark-make-record-function)
           #'bookmark/chrome-bookmark-make-record)))

(add-hook 'exwm-manage-finish-hook #'bookmark/chrome-set-bookmark-handler)
