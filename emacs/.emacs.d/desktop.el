(require 'dracula-common)

(setq epg-pinentry-mode 'loopback)

(defun shell/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun shell/async-command-no-output (command)
  (call-process-shell-command (concat command " &") nil 0))

(defun apps/vivaldi-browser (&optional url)
  (interactive)
  (shell/async-command-no-output (concat "vivaldi --remote-debugging-port=9222 --new-window " url)))

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
            '(menu-item "Shutdown" system/shutdown :help "Shutdown the computer"))
(define-key system-bar-menu [reboot]
            '(menu-item "Reboot" system/reboot :help "Reboot the computer"))
(define-key system-bar-menu [logout]
            '(menu-item "Logout" system/logout :help "Logout user"))

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

(defvar tab-bar--popup-menu-buf " *tab-bar-popup*")

(defun tab-bar--popup-menu-close ()
  "Close the popup menu posframe."
  (posframe-hide tab-bar--popup-menu-buf)
  (remove-hook 'pre-command-hook #'tab-bar--popup-menu-dismiss))

(defun tab-bar--popup-menu-dismiss ()
  "Close popup menu when clicking outside it."
  (unless (eq (window-buffer (selected-window))
              (get-buffer tab-bar--popup-menu-buf))
    (tab-bar--popup-menu-close)))

(defun tab-bar--popup-menu-show (keymap)
  "Show KEYMAP as a popup menu posframe near the tab-bar."
  (if (and (get-buffer tab-bar--popup-menu-buf)
           (frame-visible-p (buffer-local-value
                             'posframe--frame
                             (get-buffer tab-bar--popup-menu-buf))))
      (tab-bar--popup-menu-close)
    (let ((items '()))
      (map-keymap
       (lambda (_key binding)
         (when (and (consp binding) (eq (car binding) 'menu-item))
           (push (cons (nth 1 binding) (nth 2 binding)) items)))
       keymap)
      (setq items (nreverse items))
      (when items
        (with-current-buffer (get-buffer-create tab-bar--popup-menu-buf)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (dolist (item items)
              (let ((cmd (cdr item)))
                (insert-text-button
                 (concat "  " (car item) "  ")
                 'action (lambda (_btn)
                           (let ((fn cmd))
                             (tab-bar--popup-menu-close)
                             (call-interactively fn)))
                 'follow-link t
                 'face '(:inherit default :height 1.0))
                (insert "\n")))
            (goto-char (point-min))
            (setq-local cursor-type nil)
            (read-only-mode 1)))
        (posframe-show tab-bar--popup-menu-buf
                       :position (cons 0 0)
                       :poshandler #'posframe-poshandler-frame-top-left-corner
                       :border-width 1
                       :border-color (face-foreground 'shadow)
                       :min-width 20
                       :y-pixel-offset (tab-bar-height nil t))
        (add-hook 'pre-command-hook #'tab-bar--popup-menu-dismiss)))))

(defun tab-bar-system-menu ()
  (interactive)
  (popup-menu system-bar-menu))

(defun tab-bar--raise-icon (icon)
  "Vertically center ICON in the tab-bar."
  (propertize icon 'display '(raise 0.1)))

(defun tab-bar-format-system-menu ()
  (or tab-bar--system-menu-cache
      (setq tab-bar--system-menu-cache
            `((system-menu menu-item ,(concat " " (tab-bar--raise-icon (char-to-string #x23FB)) " ") tab-bar-system-menu)))))

(require 'battery)
(defvar tab-bar--datetime-cache nil "Cached datetime tab-bar item.")
(defvar tab-bar--battery-cache nil "Cached battery tab-bar item.")
(defvar tab-bar--mail-count 0 "Cached unread mail count.")
(defvar tab-bar--mail-cache nil "Cached mail tab-bar item.")
(defvar tab-bar--system-menu-cache nil "Cached system menu tab-bar item.")
(defvar tab-bar--recording-icon nil "Cached recording icon string.")

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
              (let ((unread (gnus-group-unread (car g))))
                (when (and (numberp unread) (> unread 0))
                  (setq total (+ total unread)))))
            gnus-newsrc-alist)
      (setq tab-bar--mail-count total)))
  (setq tab-bar--mail-cache
        (when (> tab-bar--mail-count 0)
          (let* ((color `(:foreground ,(face-background 'cursor)))
                 (icon (tab-bar--raise-icon (nerd-icons-mdicon "nf-md-email")))
                 (count (number-to-string tab-bar--mail-count))
                 (text (format " %s %s " icon count)))
            (add-face-text-property 0 (length text) color nil text)
            `((mail menu-item ,text gnus)))))
  (force-mode-line-update t))

(run-with-timer (- 60 (decoded-time-second (decode-time))) 60 #'tab-bar--update-status)

(defun tab-bar-format-datetime ()
  tab-bar--datetime-cache)

(defun tab-bar-format-battery ()
  tab-bar--battery-cache)

(defun tab-bar-format-recording ()
  (when (and (bound-and-true-p ewm--recording-start-time)
             (bound-and-true-p ewm--recording-process)
             (process-live-p ewm--recording-process))
    (let* ((elapsed (time-subtract (current-time) ewm--recording-start-time))
           (secs (floor (float-time elapsed)))
           (icon (or tab-bar--recording-icon
                     (setq tab-bar--recording-icon
                           (tab-bar--raise-icon (nerd-icons-mdicon "nf-md-record_circle")))))
           (text (format " %s %d:%02d " icon (/ secs 60) (% secs 60))))
      (add-face-text-property 0 (length text) `(:foreground ,(dracula-color 'dracula-red)) nil text)
      `((recording menu-item ,text ewm/record-region)))))

(defun tab-bar-format-mail ()
  tab-bar--mail-cache)

(defvar tab-bar--notification-cache nil "Cached notification tab-bar item.")
(defvar tab-bar--notification-timer nil "Timer to auto-dismiss notification.")

(defun tab-bar--dismiss-notification ()
  "Clear the notification from the tab bar."
  (setq tab-bar--notification-cache nil)
  (force-mode-line-update t))

(defun tab-bar--update-notification (_old &optional new)
  "Update the tab-bar notification item when a notification changes."
  (when tab-bar--notification-timer
    (cancel-timer tab-bar--notification-timer))
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
                                 (format "%s: %s" app summary) 80 nil nil "...")
                                'face color))))
            `((notification menu-item ,text ednc-pop-to-notification-in-log-buffer)))))
  (when tab-bar--notification-cache
    (setq tab-bar--notification-timer (run-at-time 30 nil #'tab-bar--dismiss-notification)))
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

(use-package winum
  :ensure t
  :config
  (setq winum-auto-setup-mode-line nil)
  (winum-mode 1)
  (with-eval-after-load 'doom-modeline
    (doom-modeline-def-segment window-number
      (let ((num (winum-get-number-string)))
        (propertize (format " %s " num)
                    'face (doom-modeline-face 'doom-modeline-buffer-major-mode))))))

(defun system/lock-screen ()
    (interactive)
    (shell/async-command-no-output "grim /tmp/lockscreen.png && hyprlock"))

  (defun system/logout ()
    (interactive)
    (kill-emacs))

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

    ;; Hide scroll bars on EWM surface windows
    (defun ewm/hide-surface-scroll-bars (_frame)
      (walk-windows
       (lambda (win)
         (when (with-current-buffer (window-buffer win)
                 (derived-mode-p 'ewm-surface-mode))
           (set-window-scroll-bars win 0 nil)))))
    (add-hook 'window-buffer-change-functions #'ewm/hide-surface-scroll-bars)

    ;; Compose key (Caps Lock) for accented characters
    (setq ewm-input-config '((keyboard :xkb-options "compose:caps")))

    ;; Disable text-input intercept so compose sequences reach Wayland clients
    (ewm-text-input-auto-mode-disable)

    ;; C-x C-x / C-c C-c: passthrough next keypress to surface
    (defvar ewm--passthrough-timer nil)

    (defun ewm--passthrough-restore (prefix)
      "Restore interception of PREFIX after passthrough timeout."
      (add-to-list 'ewm-intercept-prefixes prefix)
      (ewm--send-intercept-keys)
      (setq ewm--passthrough-timer nil)
      (message nil))

    (defun ewm/passthrough-prefix (key)
      "Temporarily stop intercepting C-KEY so the next press goes to the surface."
      (let ((prefix (aref (kbd (concat "C-" key)) 0)))
        (when ewm--passthrough-timer (cancel-timer ewm--passthrough-timer))
        (setq ewm-intercept-prefixes (delq prefix ewm-intercept-prefixes))
        (ewm--send-intercept-keys)
        (message "C-%s passthrough — press it now" key)
        (setq ewm--passthrough-timer
              (run-at-time 1 nil #'ewm--passthrough-restore prefix))))

    ;; Intercept C-c from surfaces (like C-x) — must be set before ewm-mode enables
    (add-to-list 'ewm-intercept-prefixes ?\C-c)

    ;; Intercept all Super-key combos from surfaces.
    ;; EWM requires individual key specs, so we generate all s-<letter>,
    ;; s-S-<letter>, s-<digit>, s-S-<digit>, s-<special>, C-s-<special>.
    ;; String specs for single-char keys (key-parse friendly)
    (dolist (key (append
                  (cl-loop for c from ?a to ?z
                           collect (format "s-%c" c)
                           collect (format "s-%c" (upcase c)))
                  (cl-loop for c from ?0 to ?9
                           collect (format "s-%c" c)
                           collect (format "s-S-%c" c))
                  '("s-=" "s-/" "s-SPC" "C-s-=")))
      (add-to-list 'ewm-intercept-prefixes key))
    ;; Event specs for special keys (multi-char names need event-convert-list)
    (dolist (sym '(left right up down return tab iso-lefttab))
      (dolist (mods '((super) (super shift) (control super)))
        (add-to-list 'ewm-intercept-prefixes
                     (event-convert-list (append mods (list sym))))))

    ;; Auto-intercept all ewm-mode-map bindings from surfaces
    (defun ewm/sync-intercept-keys ()
      "Register every `ewm-mode-map' binding in `ewm-intercept-prefixes'."
      (map-keymap
       (lambda (event _binding)
         (add-to-list 'ewm-intercept-prefixes
                      (key-description (vector event))))
       ewm-mode-map)
      (ewm--send-intercept-keys))
    (add-hook 'ewm-mode-hook #'ewm/sync-intercept-keys)

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
      "Cache mapping app-id to (COLOR-ICON . GRAYSCALE-ICON) or `none'.")

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

    (defun ewm-surface--make-icon-string (file &optional grayscale)
      "Create propertized string displaying icon image from FILE.
      When GRAYSCALE is non-nil, wrap in an SVG with desaturation and darkening.
      Includes a face plist so doom-modeline-propertize-icon processes it correctly;
      the display property with the image takes precedence for rendering."
      (let* ((h (frame-char-height))
             (img (if (not grayscale)
                      (create-image file nil nil :height h :ascent 'center)
                    (let* ((data (with-temp-buffer
                                   (set-buffer-multibyte nil)
                                   (insert-file-contents-literally file)
                                   (base64-encode-region (point-min) (point-max))
                                   (buffer-string)))
                           (svg-p (string-suffix-p ".svg" file))
                           (mime (if svg-p "image/svg+xml"
                                   (format "image/%s" (file-name-extension file))))
                           (fg (and svg-p
                                    (let ((rgb (color-values
                                                (face-foreground 'mode-line-inactive nil t))))
                                      (format "#%02x%02x%02x"
                                              (/ (nth 0 rgb) 256)
                                              (/ (nth 1 rgb) 256)
                                              (/ (nth 2 rgb) 256)))))
                           (filter (if fg
                                       (format "<filter id='g'>
        <feFlood flood-color='%s' result='c'/>
        <feComposite in='c' in2='SourceAlpha' operator='in'/>
      </filter>" fg)
                                     "<filter id='g'>
        <feColorMatrix type='saturate' values='0'/>
        <feComponentTransfer>
          <feFuncR type='linear' slope='0.6'/>
          <feFuncG type='linear' slope='0.6'/>
          <feFuncB type='linear' slope='0.6'/>
        </feComponentTransfer>
      </filter>"))
                           (svg (format
                                 "<svg xmlns='http://www.w3.org/2000/svg' width='%d' height='%d'>
      <defs>%s</defs>
      <image href='data:%s;base64,%s' width='%d' height='%d' filter='url(#g)'/>
    </svg>" h h filter mime data h h)))
                      (create-image svg 'svg t :height h :ascent 'center)))))
        (propertize " " 'display img 'face '(:family "" :height 1.0))))

    (defun ewm-surface--lookup-icon (app-id &optional grayscale)
      "Look up icon for APP-ID via .desktop files, with caching.
      When GRAYSCALE is non-nil, return the desaturated variant."
      (let ((cached (gethash app-id ewm-surface-icon-cache)))
        (if cached
            (unless (eq cached 'none)
              (if grayscale (cdr cached) (car cached)))
          (let* ((desktop   (ewm-surface--find-desktop-file app-id))
                 (icon-name (and desktop (ewm-surface--desktop-icon-name desktop)))
                 (icon-file (and icon-name (ewm-surface--find-icon-file icon-name)))
                 (color     (and icon-file (ewm-surface--make-icon-string icon-file)))
                 (gray      (and icon-file (ewm-surface--make-icon-string icon-file t))))
            (puthash app-id (if color (cons color gray) 'none) ewm-surface-icon-cache)
            (if grayscale gray color)))))

    (defun ewm-surface-icon-for-buffer (orig-fn &rest args)
      "Return app icon for EWM surface buffers, fall back to ORIG-FN.
      Unfocused surfaces get a grayscale icon."
      (if (and (bound-and-true-p ewm-surface-app)
               (not (string-empty-p ewm-surface-app)))
          (let ((grayscale (not (eq (current-buffer)
                                    (window-buffer (selected-window))))))
            (or (ewm-surface--lookup-icon ewm-surface-app grayscale)
                (nerd-icons-mdicon "nf-md-application")))
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
      (add-variable-watcher 'ewm-surface-app #'ewm-surface--on-app-set)

      (defun ewm-surface--update-icons-on-focus (&rest _)
        "Refresh doom-modeline icons for visible EWM surfaces on focus change."
        (dolist (frame (frame-list))
          (dolist (win (window-list frame 'no-minibuf))
            (with-current-buffer (window-buffer win)
              (when (bound-and-true-p ewm-surface-app)
                (doom-modeline-update-buffer-file-icon))))))
      (add-hook 'window-selection-change-functions
                #'ewm-surface--update-icons-on-focus))

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
                  (+ ;; recording indicator (worst case: visible)
                   (string-pixel-width
                    (concat " " (nerd-icons-mdicon "nf-md-record_circle") " 99:59 "))
                   sep-w
                   (string-pixel-width
                    (concat " " (nerd-icons-mdicon "nf-md-bell") " (99) "
                            (make-string 80 ?m) " "))
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

      (defun theme/apply-window-divider ()
        "Set window-divider faces from the current Dracula palette."
        (set-face-attribute 'window-divider nil :foreground (dracula-color 'dracula-bg-alternate))
        (set-face-attribute 'window-divider-first-pixel nil :foreground (dracula-color 'dracula-bg-alternate))
        (set-face-attribute 'window-divider-last-pixel nil :foreground (dracula-color 'dracula-bg-alternate)))
      (add-hook 'theme/after-apply-hook #'theme/apply-window-divider)

      (defun theme/apply-desktop ()
        "Switch Cosmic, GTK and Qt themes to match the current Emacs theme."
        (let ((light (memq 'dracula-light custom-enabled-themes)))
          ;; Cosmic
          (with-temp-file "~/.config/cosmic/com.system76.CosmicTheme.Mode/v1/is_dark"
            (insert (if light "false" "true")))
          ;; GTK color-scheme
          (start-process "gsettings" nil "gsettings" "set"
                         "org.gnome.desktop.interface" "color-scheme"
                         (if light "prefer-light" "prefer-dark"))
          ;; Qt
          (let ((scheme (if light "CosmicLight" "CosmicDark")))
            (dolist (conf '("~/.config/qt5ct/qt5ct.conf"
                            "~/.config/qt6ct/qt6ct.conf"))
              (let ((path (expand-file-name conf)))
                (when (file-exists-p path)
                  (with-temp-buffer
                    (insert-file-contents path)
                    (when (re-search-forward "^color_scheme_path=.*$" nil t)
                      (replace-match (format "color_scheme_path=%s/.local/share/color-schemes/%s.colors"
                                             (getenv "HOME") scheme)))
                    (write-region (point-min) (point-max) path))))))
          ;; Vivaldi theme + Dark Reader via Chrome DevTools Protocol
          (theme/vivaldi-set light)))
      (add-hook 'theme/after-apply-hook #'theme/apply-desktop)

      (defun theme/vivaldi-daemon-ensure ()
        "Start the vivaldi-ctl daemon if not already running."
        (let ((script (expand-file-name "~/.emacs.d/scripts/vivaldi-ctl.py")))
          (unless (file-exists-p (expand-file-name "~/.cache/vivaldi-theme.sock"))
            (start-process "vivaldi-ctl-daemon" nil "python3" script "daemon"))))

      (defun theme/vivaldi-set (light)
        "Switch Vivaldi theme and Dark Reader to match LIGHT mode."
        (let ((script (expand-file-name "~/.emacs.d/scripts/vivaldi-ctl.py")))
          (theme/vivaldi-daemon-ensure)
          (start-process "vivaldi-ctl" nil "python3" script
                         (if light "light" "dark"))))

      (theme/apply)
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
                             tab-bar-format-recording
                             tab-bar-separator
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
      (scroll-bar-mode 1)
      ;; Re-sync intercepted keys with compositor (useful for manual s-R refresh)
      (when ewm--module-mode
        (ewm/sync-intercept-keys)))

    (add-hook 'emacs-startup-hook #'ewm/refresh-setup)

    (defun ewm/focus-ednc ()
      (interactive)
      (switch-to-buffer "*ednc-log*"))

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
    (define-key ewm/window-map (kbd "s-o") #'other-frame)
    (define-key ewm/window-map (kbd "o")   #'other-frame)
    ;; s-w (s-)w: layout sub-prefix
    (define-key ewm/window-map (kbd "s-w") 'ewm/layout-map)
    (define-key ewm/window-map (kbd "w")   'ewm/layout-map)

    (define-key ewm/window-map (kbd "s-c") 'ewm/layout-map)
    (define-key ewm/window-map (kbd "c")   'ewm/layout-map)

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

    ;; Keep compositor focus on Emacs while ewm-mode-map keys are held
    ;; or a transient map (repeat-mode) is active on a surface buffer.
    ;; Without this, non-prefix intercepted keys are injected once but
    ;; key-repeat events go to the focused surface instead of Emacs.
    (defvar ewm--transient-map-active nil)
    (defvar ewm--surface-focus-timer nil)

    (defun ewm/grab-surface-focus ()
      "Grab compositor focus BEFORE command runs so key-repeat goes to Emacs.
Uses `pre-command-hook' to beat the ~300ms repeat delay.
Only grabs once; subsequent repeats just reset the release timer."
      (when (and ewm--module-mode
                 (derived-mode-p 'ewm-surface-mode)
                 this-command
                 (let ((keys (this-command-keys-vector)))
                   (and (> (length keys) 0)
                        (lookup-key ewm-mode-map keys))))
        ;; Only grab focus on the first press (timer nil = not yet grabbed)
        (unless ewm--surface-focus-timer
          (when-let ((frame-id (frame-parameter (selected-frame)
                                                'ewm-surface-id)))
            (ewm-focus frame-id)))
        ;; Reset the release timer on every repeat
        (when ewm--surface-focus-timer
          (cancel-timer ewm--surface-focus-timer))
        (setq ewm--surface-focus-timer
              (run-with-idle-timer 0.3 nil
                                   (lambda ()
                                     (setq ewm--surface-focus-timer nil)
                                     (when (and ewm--module-mode
                                                (not ewm--transient-map-active))
                                       (ewm--sync-focus)))))))

    (defun ewm/sync-transient-map-focus ()
      "Keep compositor focus on Emacs while a transient map is active."
      (when (and ewm--module-mode
                 (derived-mode-p 'ewm-surface-mode))
        (let ((transient (and overriding-terminal-local-map
                              (keymapp overriding-terminal-local-map))))
          (unless (eq (not (not transient)) ewm--transient-map-active)
            (setq ewm--transient-map-active (not (not transient)))
            (if transient
                (when-let ((frame-id (frame-parameter (selected-frame)
                                                      'ewm-surface-id)))
                  (ewm-focus frame-id))
              (ewm--sync-focus))))))

    (add-hook 'pre-command-hook #'ewm/grab-surface-focus 90)
    (add-hook 'post-command-hook #'ewm/sync-transient-map-focus 90)

    (defun ewm/screenshot-region ()
      "Take a screenshot of a selected region using grim+slurp+swappy."
      (interactive)
      (start-process-shell-command "screenshot" nil
                                   "grim -g \"$(slurp)\" - | swappy -f -"))

    (defvar ewm--recording-process nil "Active wf-recorder process.")
    (defvar ewm--recording-start-time nil "Start time of current recording.")
    (defvar ewm--recording-timer nil "Timer updating the recording indicator.")

    (defun ewm/record-region ()
      "Toggle screen recording of a selected region using wf-recorder.
  If already recording, stop and save to ~/Videos/recordings/."
      (interactive)
      (if (and ewm--recording-process (process-live-p ewm--recording-process))
          ;; Stop recording
          (progn
            (signal-process ewm--recording-process 'SIGINT)
            (when ewm--recording-timer (cancel-timer ewm--recording-timer))
            (setq ewm--recording-process nil
                  ewm--recording-start-time nil
                  ewm--recording-timer nil)
            (force-mode-line-update t)
            (message "Recording stopped"))
        ;; Start recording
        (let* ((geom (string-trim (shell-command-to-string "slurp")))
               (ts (format-time-string "%Y_%m_%d-%H_%M_%S"))
               (dir "~/Videos/recordings/")
               (file (expand-file-name (concat ts ".mkv") dir)))
          (unless (string-empty-p geom)
            (make-directory dir t)
            (setq ewm--recording-start-time (current-time)
                  ewm--recording-process
                  (start-process "wf-recorder" nil
                                 "wf-recorder" "-g" geom "-f" file))
            (set-process-sentinel
             ewm--recording-process
             (lambda (_proc _event)
               (setq ewm--recording-process nil
                     ewm--recording-start-time nil)
               (force-mode-line-update t)))
            (setq ewm--recording-timer
                  (run-with-timer 1 1 #'force-mode-line-update))
            (force-mode-line-update t)
            (message "Recording started")))))

    ;; s-S-N: swap buffer of current window with winum window N
    ;; s-S-1 is seen as s-!, etc. Map shifted symbols back to window numbers.
    (defvar ewm--shift-digit-alist
      '((?! . 1) (?@ . 2) (?# . 3) (?$ . 4) (?% . 5)
        (?^ . 6) (?& . 7) (?* . 8) (?\( . 9)))

    (defun ewm/swap-buffer-with-window (n)
      "Swap the buffer in the current window with the buffer in winum window N."
      (interactive)
      (let ((target (winum-get-window-by-number n))
            (this-win (selected-window)))
        (if (or (null target) (eq target this-win))
            (message "No window %d to swap with" n)
          (let ((this-buf (window-buffer this-win))
                (target-buf (window-buffer target)))
            (set-window-buffer this-win target-buf)
            (set-window-buffer target this-buf)
            (select-window target)))))

    (defun ewm/swap-buffer-with-window-dispatch ()
      "Dispatch buffer swap based on the shifted digit key pressed."
      (interactive)
      (let* ((key (event-basic-type last-command-event))
             (n (cdr (assq key ewm--shift-digit-alist))))
        (when n (ewm/swap-buffer-with-window n))))

    (defun ewm/swap-buffer-in-direction (dir)
      "Swap buffer with the window in direction DIR and follow."
      (let ((target (windmove-find-other-window dir))
            (this-win (selected-window)))
        (if (or (null target) (eq target this-win) (minibufferp (window-buffer target)))
            (message "No window %s" dir)
          (let ((this-buf (window-buffer this-win))
                (target-buf (window-buffer target)))
            (set-window-buffer this-win target-buf)
            (set-window-buffer target this-buf)
            (select-window target)))))

    (defun ewm/swap-buffer-left () (interactive) (ewm/swap-buffer-in-direction 'left))
    (defun ewm/swap-buffer-right () (interactive) (ewm/swap-buffer-in-direction 'right))
    (defun ewm/swap-buffer-up () (interactive) (ewm/swap-buffer-in-direction 'up))
    (defun ewm/swap-buffer-down () (interactive) (ewm/swap-buffer-in-direction 'down))

    ;; Multi-source M-x: Emacs commands + XDG apps in a single Vertico interface
    (defvar ewm/execute-extended-command-history nil
      "History for `ewm/execute-extended-command'.")
    (with-eval-after-load 'savehist
      (add-to-list 'savehist-additional-variables
                   'ewm/execute-extended-command-history)
      (add-hook 'savehist-save-hook
                (lambda ()
                  (setq ewm/execute-extended-command-history
                        (cl-remove-duplicates
                         (mapcar #'consult--tofu-strip
                                 ewm/execute-extended-command-history)
                         :test #'equal)))))

    (defun ewm/history-sort (items)
      "Sort ITEMS placing history matches first (by recency), rest alphabetical."
      (let ((pos (make-hash-table :test #'equal)))
        (cl-loop for h in ewm/execute-extended-command-history
                 for i from 0
                 do (let ((clean (consult--tofu-strip h)))
                      (unless (gethash clean pos)
                        (puthash clean i pos))))
        (sort items
              (lambda (a b)
                (let ((pa (gethash a pos))
                      (pb (gethash b pos)))
                  (cond
                   ((and pa pb) (< pa pb))
                   (pa t)
                   (pb nil)
                   (t (string< a b))))))))

    (defvar consult--source-emacs-command
      `(:name "Emacs command"
        :narrow ?x
        :category command
        :items ,(lambda ()
                  (ewm/history-sort
                   (let (cmds)
                     (mapatoms (lambda (sym)
                                 (when (commandp sym)
                                   (push (symbol-name sym) cmds))))
                     cmds))))
      "Consult multi source for Emacs interactive commands.")

    (defvar consult--source-xdg-app
      `(:name "Application"
        :narrow ?a
        :category app
        :face font-lock-builtin-face
        :items ,(lambda ()
                  (ewm/history-sort
                   (mapcar #'car (ewm-list-xdg-apps))))
        :action ,(lambda (name) (ewm-launch-xdg-command name)))
      "Consult multi source for XDG desktop applications.")

    (defun ewm/execute-extended-command ()
      "Multi-source M-x: Emacs commands + XDG apps."
      (interactive)
      (let ((selected (consult--multi
                       (list consult--source-emacs-command
                             consult--source-xdg-app)
                       :prompt "M-x: "
                       :sort nil
                       :history 'ewm/execute-extended-command-history)))
        (when selected
          (pcase-let ((`(,cand . ,src) selected))
            (unless (plist-get src :action)
              (command-execute (intern cand)))))))

    (defun next-buffer-same-mode ()
      "Switch to the next buffer with the same major mode."
      (interactive)
      (let ((mode major-mode)
            (start (current-buffer)))
        (next-buffer)
        (while (and (not (eq major-mode mode))
                    (not (eq (current-buffer) start)))
          (next-buffer))))

    (defun previous-buffer-same-mode ()
      "Switch to the previous buffer with the same major mode."
      (interactive)
      (let ((mode major-mode)
            (start (current-buffer)))
        (previous-buffer)
        (while (and (not (eq major-mode mode))
                    (not (eq (current-buffer) start)))
          (previous-buffer))))

    ;; Keybindings in ewm-mode-map (always active, intercepted from surfaces)
    :bind (:map ewm-mode-map
                ;; refresh setup
                ("s-R" . ewm/refresh-setup)

                ("s-/" . winner-undo)
                ("s-?" . winner-redo)

                ("s-d" . ewm/focus-ednc)
                ("s-t" . nil)
                ;; ("s-l" . system/lock-screen)
                ("s-x" . ewm/execute-extended-command)
                ("M-x" . ewm/execute-extended-command)

                ;; Screenshot / recording
                ("s-c" . ewm/screenshot-region)
                ("s-C" . ewm/record-region)

                ;; Window selection (winum) — override default s-N tab bindings
                ("s-0" . winum-select-window-0-or-10)
                ("s-1" . winum-select-window-1)
                ("s-2" . winum-select-window-2)
                ("s-3" . winum-select-window-3)
                ("s-4" . winum-select-window-4)
                ("s-5" . winum-select-window-5)
                ("s-6" . winum-select-window-6)
                ("s-7" . winum-select-window-7)
                ("s-8" . winum-select-window-8)
                ("s-9" . winum-select-window-9)

                ;; Buffer swap with target window (s-S-N)
                ("s-!" . ewm/swap-buffer-with-window-dispatch)
                ("s-@" . ewm/swap-buffer-with-window-dispatch)
                ("s-#" . ewm/swap-buffer-with-window-dispatch)
                ("s-$" . ewm/swap-buffer-with-window-dispatch)
                ("s-%" . ewm/swap-buffer-with-window-dispatch)
                ("s-^" . ewm/swap-buffer-with-window-dispatch)
                ("s-&" . ewm/swap-buffer-with-window-dispatch)
                ("s-*" . ewm/swap-buffer-with-window-dispatch)
                ("s-(" . ewm/swap-buffer-with-window-dispatch)

                ;; Buffer swap with adjacent window (s-S-<arrow>)
                ("S-s-<left>" . ewm/swap-buffer-left)
                ("S-s-<right>" . ewm/swap-buffer-right)
                ("S-s-<up>" . ewm/swap-buffer-up)
                ("S-s-<down>" . ewm/swap-buffer-down)

                ;; Window resize mode
                ("s-r" . windresize)

                ;; Killing Buffers & Windows
                ("s-k" . kill-current-buffer)
                ("s-K" . kill-buffer-and-window)
                ("C-s-k" . delete-window)

                ;; Splitting Windows
                ("s-s" . split-window-right)
                ("s-S" . split-window-below)

                ;; Switching buffers
                ("s-b" . consult-buffer)
                ("s-n" . next-buffer-same-mode)
                ("s-p" . previous-buffer-same-mode)


                ;; Tab switching
                ("s-<tab>" . tab-next)
                ("s-<iso-lefttab>" . tab-previous)

                ;; Applications
                ("s-i" . vivaldi/goto-url)
                ("s-I" . vivaldi/goto-url-new-window)

                ("s-<return>" . eshell/new-or-current)
                ("S-s-<return>" . eat)
                ("C-s-<return>" . apps/cosmic-term)))

(defvar vivaldi/input-history nil)
    (eval-after-load "savehist"
      '(add-to-list 'savehist-additional-variables 'vivaldi/input-history))

    (defun vivaldi/surface-p ()
      "Return non-nil if the current buffer is a Vivaldi surface."
      (and (boundp 'ewm-surface-app)
           ewm-surface-app
           (string-match-p "\\`[Vv]ivaldi" ewm-surface-app)))

    (defun vivaldi/get-url ()
      "Extract URL from the Vivaldi window title."
      (interactive)
      (when (vivaldi/surface-p)
        (when (string-match "\\(https?://\\S-+\\) - Vivaldi\\'" ewm-surface-title)
          (match-string 1 ewm-surface-title))))

    (defun vivaldi/get-page-title ()
      "Extract page title from the Vivaldi window title."
      (when (vivaldi/surface-p)
        (when (string-match "\\`\\(.*\\) - https?://\\S-+ - Vivaldi\\'" ewm-surface-title)
          (match-string 1 ewm-surface-title))))

    (defun vivaldi/copy-url ()
      "Copy the current Vivaldi tab URL to the kill ring."
      (interactive)
      (if-let ((url (vivaldi/get-url)))
          (progn (kill-new url) (message "%s" url))
        (message "No Vivaldi URL found")))

    (defun vivaldi/brotab-tab-id ()
      "Find the brotab tab ID matching the current Vivaldi surface.
  Matches by comparing the URL from the window title against brotab's tab list."
      (when-let ((url (vivaldi/get-url)))
        (with-temp-buffer
          (when (zerop (call-process "bt" nil t nil "list"))
            (goto-char (point-min))
            (let ((tab-id nil))
              (while (and (not tab-id) (not (eobp)))
                (when (looking-at "\\(\\S-+\\)\t\\(.*?\\)\t\\(.*\\)$")
                  (let ((id (match-string 1))
                        (tab-url (match-string 3)))
                    (when (string= (string-trim tab-url) url)
                      (setq tab-id id))))
                (forward-line 1))
              tab-id)))))

    (defun vivaldi/history-complete (string pred action)
      "Completion table for `vivaldi/input-history' with metadata."
      (if (eq action 'metadata)
          '(metadata (category . vivaldi-history))
        (complete-with-action action vivaldi/input-history string pred)))

    (defun vivaldi/history-delete-at-point ()
      "Delete the currently selected candidate from `vivaldi/input-history'."
      (interactive)
      (when-let* ((cand (nth vertico--index vertico--candidates)))
        (setq vivaldi/input-history (delete cand vivaldi/input-history))
        (setq vertico--input t)
        (vertico--exhibit)
        (message "Deleted: %s" cand)))

    (defun vivaldi/goto-url-setup ()
      "Setup minibuffer keymap for `vivaldi/goto-url'."
      (local-set-key (kbd "C-k") #'vivaldi/history-delete-at-point)
      (when (> (point-max) (minibuffer-prompt-end))
        (set-mark (minibuffer-prompt-end))
        (goto-char (point-max))
        (activate-mark)))

    (defun vivaldi/goto-url (&optional new-window)
      "Navigate current Vivaldi tab in-place, or open a new window.
With NEW-WINDOW, always open in a new window."
      (interactive)
      (let* ((current-url (vivaldi/get-url))
             (input (minibuffer-with-setup-hook #'vivaldi/goto-url-setup
                      (completing-read "URL or search: " #'vivaldi/history-complete
                                       nil nil current-url 'vivaldi/input-history)))
             (trimmed (string-trim input))
             (url (if (string-match-p "\\`https?://\\|\\`localhost[:/]\\|\\." trimmed)
                      trimmed
                    (concat "https://www.google.com/search?q=" (url-hexify-string trimmed))))
             (tab-id (when (and (not new-window) (vivaldi/surface-p))
                       (vivaldi/brotab-tab-id))))
        (if tab-id
            (progn
              (call-process "bt" nil nil nil "navigate" tab-id url)
              (message "Navigating to %s" url))
          (shell/async-command-no-output (concat "vivaldi --new-window '" url "'"))
          (message "Opening %s in new window" url))))

    (defun vivaldi/goto-url-new-window ()
      "Open URL in a new Vivaldi window."
      (interactive)
      (vivaldi/goto-url t))

    ;; Bookmarks
    (defun bookmark/vivaldi-bookmark-handler (record)
      "Jump to a Vivaldi bookmarked location."
      (shell/async-command-no-output
       (concat "vivaldi --new-window '" (bookmark-prop-get record 'location) "'")))

    (defun bookmark/vivaldi-bookmark-make-record ()
      "Return a bookmark record for the current Vivaldi buffer."
      (when-let ((url (vivaldi/get-url)))
        (let ((page-title (or (vivaldi/get-page-title) "untitled")))
          `(,(concat "vivaldi/" page-title)
            (location . ,url)
            (handler . bookmark/vivaldi-bookmark-handler)))))

    (defun bookmark/vivaldi-set-bookmark-handler ()
      "Set bookmark handler for Vivaldi surface buffers."
      (when (vivaldi/surface-p)
        (setq-local bookmark-make-record-function #'bookmark/vivaldi-bookmark-make-record)))

    (add-hook 'ewm-update-title-hook #'bookmark/vivaldi-set-bookmark-handler)
