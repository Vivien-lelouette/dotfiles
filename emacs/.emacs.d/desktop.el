(defun shell/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun shell/async-command-no-output (command)
  (call-process-shell-command (concat command " &") nil 0))

(defun apps/vivaldi-browser (&optional url)
  (interactive)
  (shell/async-command-no-output (concat "vivaldi --new-window " url)))

(defun apps/cosmic-term ()
  (interactive)
  (shell/async-command-no-output "cosmic-term"))

(defvar emacs-header-bar-menu (make-sparse-keymap "-- Emacs menu --"))
(define-key global-map [menu-bar emacs-header-menu] (cons "-- Emacs menu --" emacs-header-bar-menu))

(defun menu/custom-menu ()
  (interactive)

(defvar system-bar-menu (make-sparse-keymap "System"))
(define-key global-map [menu-bar system-menu] (cons "System" system-bar-menu))
(define-key system-bar-menu [shutdown]
            '(menu-item "Shutdown" cosmic/shutdown :help "Shutdown the computer"))
(define-key system-bar-menu [reboot]
            '(menu-item "Reboot" cosmic/reboot :help "Reboot the computer"))
(define-key system-bar-menu [logout]
            '(menu-item "Logout" cosmic/logout :help "Logout user"))

(defvar application-bar-menu (make-sparse-keymap "Applications"))
(define-key global-map [menu-bar application-menu] (cons "Applications" application-bar-menu))

)
(menu/custom-menu)

(defun menu/close-dropdown ()
  (interactive)
  (let ((buf (get-buffer " *dropdown-menu*")))
    (when buf
      (let ((win (get-buffer-window buf)))
        (when win (delete-window win)))
      (kill-buffer buf))))

(defun menu/show-dropdown (keymap)
  (if (get-buffer " *dropdown-menu*")
      (menu/close-dropdown)
    (let ((items '())
          (buf (get-buffer-create " *dropdown-menu*")))
      (map-keymap
       (lambda (_key binding)
         (when (and (consp binding) (eq (car binding) 'menu-item))
           (push (cons (nth 1 binding) (nth 2 binding)) items)))
       keymap)
      (setq items (nreverse items))
      (when (null items) (kill-buffer buf) (user-error "Menu is empty"))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (dolist (item items)
            (let ((cmd (cdr item)))
              (insert-text-button
               (concat "  " (car item) "  ")
               'action (lambda (_btn)
                         (let ((fn cmd))
                           (menu/close-dropdown)
                           (funcall fn)))
               'follow-link t)
              (insert "\n")))
          (goto-char (point-min))
          (setq-local cursor-type nil)
          (read-only-mode 1)))
      (display-buffer-in-side-window buf '((side . top) (window-height . fit-window-to-buffer))))))

(defun tab-bar-system-menu ()
  (interactive)
  (menu/show-dropdown system-bar-menu))

(defun tab-bar--raise-icon (icon)
  "Vertically center ICON in the tab-bar."
  (propertize icon 'display '(raise 0.1)))

(defun tab-bar-format-system-menu ()
  `((system-menu menu-item ,(concat " " (tab-bar--raise-icon (char-to-string #x23FB)) " ") tab-bar-system-menu)))

(require 'battery)
(defvar tab-bar--datetime-cache nil "Cached datetime tab-bar item.")
(defvar tab-bar--battery-cache nil "Cached battery tab-bar item.")
(defvar tab-bar--mail-count 0 "Cached unread mail count.")

(defun tab-bar--update-status ()
  "Update cached datetime, battery and mail tab-bar items."
  (setq tab-bar--datetime-cache
        `((datetime menu-item ,(concat " " (format-time-string "%a %d %b  %H:%M") " ") ignore)))
  (when (bound-and-true-p battery-status-function)
    (let* ((data (funcall battery-status-function))
           (pct-str (cdr (assq ?p data)))
           (pct (string-to-number (or pct-str "0")))
           (status (cdr (assq ?L data)))
           (charging (or (string= "AC" status) (string= "on-line" status)))
           (level (cond ((>= pct 90) "") ((>= pct 80) "90") ((>= pct 70) "80")
                        ((>= pct 60) "70") ((>= pct 50) "60") ((>= pct 40) "50")
                        ((>= pct 30) "40") ((>= pct 20) "30") ((>= pct 10) "20")
                        (t "10")))
           (name (if charging
                     (concat "nf-md-battery_charging" (if (string= level "") "_100" (concat "_" level)))
                   (concat "nf-md-battery" (if (string= level "") "" (concat "_" level)))))
           (icon (tab-bar--raise-icon (nerd-icons-mdicon name))))
      (setq tab-bar--battery-cache
            `((battery menu-item ,(format " %s %s%% " icon (string-trim (or pct-str ""))) ignore)))))
  (when (bound-and-true-p gnus-newsrc-alist)
    (let ((total 0))
      (mapc (lambda (g)
              (let ((unread (eval `(gnus-group-unread ,(car g)))))
                (when (and (numberp unread) (> unread 0))
                  (setq total (+ total unread)))))
            gnus-newsrc-alist)
      (setq tab-bar--mail-count total)))
  (force-mode-line-update t))

(run-with-timer (- 60 (decoded-time-second (decode-time))) 60 #'tab-bar--update-status)

(defun tab-bar-format-datetime ()
  tab-bar--datetime-cache)

(defun tab-bar-format-battery ()
  tab-bar--battery-cache)

(defun tab-bar-format-mail ()
  (when (> tab-bar--mail-count 0)
    (let* ((color `(:foreground ,(face-background 'cursor)))
           (icon (tab-bar--raise-icon (nerd-icons-mdicon "nf-md-email")))
           (count (number-to-string tab-bar--mail-count))
           (text (format " %s %s " icon count)))
      (add-face-text-property 0 (length text) color nil text)
      `((mail menu-item ,text gnus)))))

(defvar tab-bar--notification-cache nil "Cached notification tab-bar item.")

(defun tab-bar--update-notification (_old &optional new)
  "Update the tab-bar notification item when a notification changes."
  (setq tab-bar--notification-cache
        (when-let ((n (or new (car (ednc-notifications)))))
          (let* ((summary (ednc-notification-summary n))
                 (app (ednc-notification-app-name n))
                 (app-icon (alist-get 'icon (ednc-notification-amendments n)))
                 (color `(:foreground ,(face-background 'cursor)))
                 (icon (or app-icon
                           (let ((bell (tab-bar--raise-icon (nerd-icons-mdicon "nf-md-bell"))))
                             (add-face-text-property 0 (length bell) color nil bell)
                             bell)))
                 (count (length (ednc-notifications)))
                 (badge (if (> count 1)
                            (propertize (format "(%d) " count) 'face color)
                          ""))
                 (text (format " %s %s%s " icon badge
                               (propertize
                                (truncate-string-to-width
                                 (format "%s: %s" app summary) 120 nil nil "...")
                                'face color))))
            `((notification menu-item ,text ednc-pop-to-notification-in-log-buffer)))))
  (force-mode-line-update t))

(defun tab-bar-format-notification ()
  tab-bar--notification-cache)

(use-package ednc
  :config
  (add-hook 'ednc-notification-presentation-functions #'tab-bar--update-notification)
  (ednc-mode 1))

(defvar ewm/saved-layouts nil
  "Alist of saved window layouts, keyed by slot number.")

(eval-after-load "savehist"
  '(add-to-list 'savehist-additional-variables 'ewm/saved-layouts))

(defun ewm/save-layout (n)
  "Save current window layout to slot N."
  (setf (alist-get n ewm/saved-layouts)
        (window-state-get (frame-root-window) t))
  (message "Layout saved to slot %d" n))

(defun ewm/load-layout (n)
  "Load window layout from slot N."
  (if-let ((state (alist-get n ewm/saved-layouts)))
      (progn
        (window-state-put state (frame-root-window) t)
        (message "Layout loaded from slot %d" n))
    (message "No layout in slot %d" n)))

(defun helm-ag/noop-status ()
  "No-op status function for helm-ag.")
(setq helm-ag-show-status-function #'helm-ag/noop-status)

(defun system/lock-screen ()
  (interactive)
  (shell/async-command-no-output "hyprlock"))

(defun system/logout ()
  (interactive)
  (shell/run-in-background "cosmic-session exit"))

(defun system/shutdown ()
  (interactive)
  (shell/run-in-background "shutdown -h 0"))

(defun system/reboot ()
  (interactive)
  (shell/run-in-background "reboot"))

(defun settings/manager ()
  (interactive)
  (shell/run-in-background "cosmic-settings"))

(defun settings/appearance ()
  (interactive)
  (shell/run-in-background "cosmic-settings appearance"))

(defun settings/display ()
  (interactive)
  (shell/run-in-background "cosmic-settings displays"))

(defun settings/keyboard ()
  (interactive)
  (shell/run-in-background "cosmic-settings keyboard"))

(defun settings/mouse ()
  (interactive)
  (shell/run-in-background "cosmic-settings mouse"))

(defun settings/network ()
  (interactive)
  (shell/run-in-background "cosmic-settings network"))

(defun settings/sound ()
  (interactive)
  (shell/run-in-background "cosmic-settings sound"))

(use-package ewm
  :ensure nil
  :config
  (winner-mode 1)

  ;; Compose key (Caps Lock) for accented characters
  (setq ewm-input-config '((keyboard :xkb-options "compose:caps")))

  ;; Disable text-input intercept so compose sequences reach Wayland clients
  (ewm-text-input-auto-mode-disable)

  ;; C-x C-x / C-c C-c: passthrough next keypress to surface
  (defvar ewm--passthrough-timer nil)

  (defun ewm/passthrough-prefix (key)
    "Temporarily stop intercepting C-KEY so the next press goes to the surface."
    (let ((prefix (aref (kbd (concat "C-" key)) 0)))
      (when ewm--passthrough-timer (cancel-timer ewm--passthrough-timer))
      (setq ewm-intercept-prefixes (delq prefix ewm-intercept-prefixes))
      (ewm--send-intercept-keys)
      (message "C-%s passthrough — press it now" key)
      (setq ewm--passthrough-timer
            (run-at-time 1 nil
                         (lambda (p)
                           (add-to-list 'ewm-intercept-prefixes p)
                           (ewm--send-intercept-keys)
                           (setq ewm--passthrough-timer nil))
                         prefix))))

  ;; Intercept C-c from surfaces (like C-x) — must be set before ewm-mode enables
  (add-to-list 'ewm-intercept-prefixes ?\C-c)

  ;; Passthrough bindings: C-x C-x / C-c C-c send next keypress to the surface
  (defun ewm/passthrough-x ()
    "Passthrough C-x to surface app."
    (interactive)
    (ewm/passthrough-prefix "x"))
  (defun ewm/passthrough-c ()
    "Passthrough C-c to surface app."
    (interactive)
    (ewm/passthrough-prefix "c"))
  (define-key ewm-surface-mode-map (kbd "C-x C-x") #'ewm/passthrough-x)
  (define-key ewm-surface-mode-map (kbd "C-c C-c") #'ewm/passthrough-c)

  ;; Dynamic application icons for EWM surfaces in doom-modeline
  ;; Looks up real app icons via .desktop files and the hicolor icon theme
  (defvar ewm-surface-icon-cache (make-hash-table :test 'equal)
    "Cache mapping app-id to propertized icon string or `none'.")

  (defun ewm-surface--xdg-data-dirs ()
    "Return list of XDG data directories."
    (let ((dirs (getenv "XDG_DATA_DIRS")))
      (if dirs (split-string dirs ":" t)
        '("/usr/share" "/usr/local/share"))))

  (defun ewm-surface--find-desktop-file (app-id)
    "Find .desktop file for APP-ID in XDG data dirs."
    (let ((lowered (downcase app-id)))
      (cl-loop for dir in (ewm-surface--xdg-data-dirs)
               for apps-dir = (expand-file-name "applications" dir)
               when (file-directory-p apps-dir)
               thereis
               (cl-loop for f in (directory-files apps-dir t "\\.desktop\\'")
                        for base = (downcase (file-name-sans-extension
                                              (file-name-nondirectory f)))
                        when (or (string= base lowered)
                                 (string-match-p (regexp-quote lowered) base))
                        return f))))

  (defun ewm-surface--desktop-icon-name (desktop-file)
    "Extract Icon= value from DESKTOP-FILE."
    (with-temp-buffer
      (insert-file-contents desktop-file)
      (when (re-search-forward "^Icon=\\(.+\\)$" nil t)
        (match-string 1))))

  (defun ewm-surface--find-icon-file (icon-name)
    "Find icon file for ICON-NAME in hicolor icon theme or pixmaps."
    (if (and (file-name-absolute-p icon-name) (file-exists-p icon-name))
        icon-name
      (let ((sizes '("scalable" "48x48" "32x32" "24x24" "64x64" "16x16" "22x22"))
            (exts  '("svg" "png")))
        (or (cl-loop for dir in (ewm-surface--xdg-data-dirs)
                     for icons-dir = (expand-file-name "icons/hicolor" dir)
                     when (file-directory-p icons-dir)
                     thereis
                     (cl-loop for size in sizes thereis
                              (cl-loop for ext in exts
                                       for path = (expand-file-name
                                                   (format "%s/apps/%s.%s" size icon-name ext)
                                                   icons-dir)
                                       when (file-exists-p path) return path)))
            ;; Fallback: pixmaps directory
            (cl-loop for dir in (ewm-surface--xdg-data-dirs)
                     for path = (expand-file-name (concat "pixmaps/" icon-name ".png") dir)
                     when (file-exists-p path) return path)))))

  (defun ewm-surface--make-icon-string (file)
    "Create propertized string displaying icon image from FILE.
  Includes a face plist so doom-modeline-propertize-icon processes it correctly;
  the display property with the image takes precedence for rendering."
    (propertize " " 'display
                (create-image file nil nil
                              :height (frame-char-height)
                              :ascent 'center)
                'face '(:inherit default :family "" :height 1.0)))

  (defun ewm-surface--lookup-icon (app-id)
    "Look up icon for APP-ID via .desktop files, with caching."
    (let ((cached (gethash app-id ewm-surface-icon-cache)))
      (if cached
          (unless (eq cached 'none) cached)
        (let* ((desktop   (ewm-surface--find-desktop-file app-id))
               (icon-name (and desktop (ewm-surface--desktop-icon-name desktop)))
               (icon-file (and icon-name (ewm-surface--find-icon-file icon-name)))
               (result    (and icon-file (ewm-surface--make-icon-string icon-file))))
          (puthash app-id (or result 'none) ewm-surface-icon-cache)
          result))))

  (defun ewm-surface-icon-for-buffer (orig-fn &rest args)
    "Return app icon for EWM surface buffers, fall back to ORIG-FN."
    (if (and (bound-and-true-p ewm-surface-app)
             (not (string-empty-p ewm-surface-app)))
        (or (ewm-surface--lookup-icon ewm-surface-app)
            (nerd-icons-mdicon "nf-md-application"))
      (apply orig-fn args)))

  (with-eval-after-load 'doom-modeline-core
    (advice-add 'doom-modeline-icon-for-buffer :around #'ewm-surface-icon-for-buffer)

    ;; Minimal modeline for Wayland surface buffers
    (doom-modeline-def-modeline 'ewm-surface
      '(window-number buffer-info)
      '(misc-info major-mode bar))

    ;; ewm-surface-app is set AFTER ewm-surface-mode activates, so
    ;; doom-modeline's after-change-major-mode-hook fires too early.
    ;; Re-trigger the icon update and set the minimal modeline.
    (defun ewm-surface--on-app-set (_sym _val op where)
      "Update doom-modeline icon and layout when ewm-surface-app is set."
      (when (and (eq op 'set) (bufferp where))
        (with-current-buffer where
          (doom-modeline-update-buffer-file-icon)
          (doom-modeline-set-modeline 'ewm-surface))))
    (add-variable-watcher 'ewm-surface-app #'ewm-surface--on-app-set))

  (defvar tab-bar--tabs-start-px nil "Cached pixel position where tabs area starts.")
  (defvar tab-bar--right-reserved-px nil "Cached max pixel width of right-side elements.")

  (defun tab-bar--tabs-start-px ()
    (or tab-bar--tabs-start-px
        (setq tab-bar--tabs-start-px
              (let ((sep-w (string-pixel-width " ")))
                (+ sep-w ; left separator
                   (string-pixel-width (concat " " (char-to-string #x23FB) " "))
                   sep-w))))) ; separator after menu

  (defun tab-bar--right-reserved-px ()
    (or tab-bar--right-reserved-px
        (setq tab-bar--right-reserved-px
              (let ((sep-w (string-pixel-width " ")))
                (+ (string-pixel-width
                    (concat " " (nerd-icons-mdicon "nf-md-bell") " (99) "
                            (make-string 120 ?m) " "))
                   sep-w
                   (string-pixel-width
                    (concat " " (nerd-icons-mdicon "nf-md-email") " 9999 "))
                   sep-w
                   (string-pixel-width
                    (concat " " (format-time-string "%a %d %b  %H:%M") " "))
                   sep-w
                   (string-pixel-width
                    (concat " " (nerd-icons-mdicon "nf-md-battery") " 100% "))
                   sep-w))))) ; trailing separator

  (defun tab-bar/format-tab-name (tab i)
    "Format TAB name with index I, fixed pixel-width using align-to."
    (let* ((current-p (eq (car tab) 'current-tab))
           (tabs (frame-parameter nil 'tabs))
           (tab-count (max 1 (length tabs)))
           (name (alist-get 'name tab))
           (prefix (if (> tab-count 1) (format "%d: " i) ""))
           (face (if current-p 'tab-bar-tab 'tab-bar-tab-inactive))
           (start (tab-bar--tabs-start-px))
           (per-tab (max (* 10 (frame-char-width))
                         (/ (- (frame-inner-width) start (tab-bar--right-reserved-px))
                            tab-count)))
           (tab-end (+ start (* i per-tab)))
           (max-chars (max 5 (- (/ per-tab (frame-char-width)) 1)))
           (content (concat " " prefix name " "))
           (truncated (truncate-string-to-width content max-chars nil nil "..."))
           (pad (propertize " " 'display `(space :align-to (,tab-end)) 'face face)))
      (concat (propertize truncated 'face face) pad)))

  (defun ewm/refresh-setup ()
    (interactive)
    (setopt epg-pinentry-mode 'loopback)
    (setq agent-shell-display-action
        (if (featurep 'ewm)
            '((display-buffer-same-window))
          '((vv/display-buffer-pop-up-frame-maybe display-buffer-in-side-window)
            (side . left)
            (slot . 1)
            (window-width . 100)
            (preserve-size . (t . nil)))))

    (theme/apply)
    (set-face-attribute 'window-divider nil :foreground "#282a36")
    (set-face-attribute 'window-divider-first-pixel nil :foreground "#282a36")
    (set-face-attribute 'window-divider-last-pixel nil :foreground "#282a36")
    (modify-all-frames-parameters '((right-divider-width . 8)
                                    (bottom-divider-width . 8)))

    (menu/custom-menu)
    (setq tab-bar-auto-width nil
          tab-bar-close-button-show nil
          tab-bar-format '(tab-bar-separator
                           tab-bar-format-system-menu
                           tab-bar-separator
                           tab-bar-format-tabs
                           tab-bar-format-align-right
                           tab-bar-format-notification
                           tab-bar-separator
                           tab-bar-format-mail
                           tab-bar-separator
                           tab-bar-format-datetime
                           tab-bar-separator
                           tab-bar-format-battery
                           tab-bar-separator))
    (setq tab-bar-tab-name-format-function #'tab-bar/format-tab-name)
    (tab-bar-mode -1)
    (tab-bar-mode 1)
    ;; Re-sync intercepted keys with compositor (useful for manual s-R refresh)
    (when ewm--module-mode
      (ewm--send-intercept-keys)))

  (add-hook 'emacs-startup-hook #'ewm/refresh-setup)

  ;; s-w prefix key: window & tab management
  (define-prefix-command 'ewm/window-map)
  (define-prefix-command 'ewm/layout-map)
  (define-prefix-command 'ewm/layout-save-map)

  (define-key ewm-mode-map (kbd "s-w") 'ewm/window-map)

  ;; s-w (s-)n / s-w (s-)k: tab management
  (define-key ewm/window-map (kbd "s-n") #'tab-bar-duplicate-tab)
  (define-key ewm/window-map (kbd "n")   #'tab-bar-duplicate-tab)
  (define-key ewm/window-map (kbd "s-k") #'tab-bar-close-tab)
  (define-key ewm/window-map (kbd "k")   #'tab-bar-close-tab)

  ;; s-w (s-)w: layout sub-prefix
  (define-key ewm/window-map (kbd "s-w") 'ewm/layout-map)
  (define-key ewm/window-map (kbd "w")   'ewm/layout-map)

  ;; s-w s-w (s-)s <n>: save layout to slot n
  (define-key ewm/layout-map (kbd "s-s") 'ewm/layout-save-map)
  (define-key ewm/layout-map (kbd "s")   'ewm/layout-save-map)
  ;; s-w s-w (s-)<n>: load layout from slot n
  ;; s-w s-w (s-)w: load layout from slot 0 (quick access)
  (dotimes (i 9)
    (let ((n (1+ i)))
      (define-key ewm/layout-save-map (kbd (number-to-string n))
        `(lambda () (interactive) (ewm/save-layout ,n)))
      (define-key ewm/layout-map (kbd (format "s-%d" n))
        `(lambda () (interactive) (ewm/load-layout ,n)))
      (define-key ewm/layout-map (kbd (number-to-string n))
        `(lambda () (interactive) (ewm/load-layout ,n)))))
  (defun ewm/save-layout-0 ()
    "Save window layout to quick-access slot 0."
    (interactive)
    (ewm/save-layout 0))
  (defun ewm/load-layout-0 ()
    "Load window layout from quick-access slot 0."
    (interactive)
    (ewm/load-layout 0))
  (define-key ewm/layout-save-map (kbd "w") #'ewm/save-layout-0)
  (define-key ewm/layout-map (kbd "s-w") #'ewm/load-layout-0)
  (define-key ewm/layout-map (kbd "w") #'ewm/load-layout-0)

  ;; Keybindings in ewm-mode-map (always active, intercepted from surfaces)
  :bind (:map ewm-mode-map
              ;; refresh setup
              ("s-R" . ewm/refresh-setup)

              ("s-/" . winner-undo)
              ("s-?" . winner-redo)

              ("s-a" . ewm-launch-app)
              ("s-d" . nil)
              ("s-t" . nil)
              ("s-l" . system/lock-screen)
              ("s-x" . helm-M-x)

              ;; Applications
              ("s-i" . apps/vivaldi-browser)
              ("s-I" . eww)

              ("s-<return>" . eshell/new-or-current)
              ("S-s-<return>" . eat)
              ("C-s-<return>" . apps/cosmic-term)))


