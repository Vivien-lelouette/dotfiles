(defun custom/load-local-settings ()
  (interactive)
  (let ((local-settings "~/.doom.d/local.el"))
    (when (file-exists-p local-settings)
        (load-file local-settings))))

(setq doom-font (font-spec :family "SauceCodePro NF" :size 14)
                doom-big-font (font-spec :family "SauceCodePro NF" :size 24)
                doom-variable-pitch-font (font-spec :family "Cantarell" :size 14)
                doom-variable-pitch-big-font (font-spec :family "Cantarell" :size 24))

(defun hooks/first-frame ()
  (interactive)
        (unless (boundp 'first-frame-created)
          (evil-snipe-mode 0)
          (custom/load-local-settings))
        (setq first-frame-created t))

(add-hook! 'server-after-make-frame-hook #'hooks/first-frame)

(scroll-bar-mode 0)

(defun doom-dashboard-draw-ascii-banner-fn ()
  (let* ((banner
          '("Emacs"))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat
                 line (make-string (max 0 (- longest-line (length line)))
                                   32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(map! :leader
    :desc "Kill ring"
    "y" #'consult-yank-pop)

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(use-package! mixed-pitch
  :hook
  (org-mode . mixed-pitch-mode))

;; Line number styling for mode change
(setq theme/normal-lines-fg nil)
(setq theme/normal-lines-bg nil)
(setq theme/normal-current-line-fg nil)
(setq theme/normal-current-line-bg nil)

(setq theme/insert-lines-fg nil)
(setq theme/insert-lines-bg nil)
(setq theme/insert-current-line-fg nil)
(setq theme/insert-current-line-bg nil)

(setq theme/visual-lines-fg nil)
(setq theme/visual-lines-bg nil)
(setq theme/visual-current-line-fg nil)
(setq theme/visual-current-line-bg nil)

(defun theme/normal-lines ()
  (face-remap-add-relative 'line-number nil :foreground theme/normal-lines-fg :background theme/normal-lines-bg))

(defun theme/normal-current-line ()
  (face-remap-add-relative 'line-number-current-line nil :foreground theme/normal-current-line-fg :background theme/normal-current-line-bg))

(defun theme/insert-lines ()
  (face-remap-add-relative 'line-number nil :foreground theme/insert-lines-fg :background theme/insert-lines-bg))

(defun theme/insert-current-line ()
  (face-remap-add-relative 'line-number-current-line nil :foreground theme/insert-current-line-fg :background theme/insert-current-line-bg))

(defun theme/visual-lines ()
  (face-remap-add-relative 'line-number nil :foreground theme/visual-lines-fg :background theme/visual-lines-bg))

(defun theme/visual-current-line ()
  (face-remap-add-relative 'line-number-current-line nil :foreground theme/visual-current-line-fg :background theme/visual-current-line-bg))

(add-hook 'evil-normal-state-entry-hook 'theme/normal-lines)
(add-hook 'evil-normal-state-entry-hook 'theme/normal-current-line)

(add-hook 'evil-insert-state-entry-hook 'theme/insert-lines)
(add-hook 'evil-insert-state-entry-hook 'theme/insert-current-line)

(add-hook 'evil-visual-state-entry-hook 'theme/visual-lines)
(add-hook 'evil-visual-state-entry-hook 'theme/visual-current-line)

(defun theme/doom-nord ()
  (interactive)
  (set-face-attribute 'fringe nil :background "#2e3440")
  (set-face-attribute 'mode-line-inactive nil :background nil)
  ;; (set-face-attribute 'scroll-bar nil :background "#2b323d")

  ;; Line number styling for mode change
  (setq theme/normal-lines-fg "#6c7686")
  (setq theme/normal-lines-bg "#2e3440")
  (setq theme/normal-current-line-fg "#ffffff")
  (setq theme/normal-current-line-bg "#242832")

  (setq theme/insert-lines-fg "#2e3440")
  (setq theme/insert-lines-bg "#515e46")
  (setq theme/insert-current-line-fg "#ffffff")
  (setq theme/insert-current-line-bg "#a3be8c")

  (setq theme/visual-lines-fg "#2e3440")
  (setq theme/visual-lines-bg "#594656")
  (setq theme/visual-current-line-fg "#ffffff")
  (setq theme/visual-current-line-bg "#b48ead")
  (load-theme 'doom-nord t))

(after! org
  (setq org-directory "~/org/"
        org-hide-emphasis-markers t))

(defun org/org-babel-tangle-config ()
  (when (or (string-equal (buffer-file-name)
                          (expand-file-name "~/dotfiles/README.org"))
            (string-equal (buffer-file-name)
                          (expand-file-name "~/dotfiles/doom-emacs/README.org"))
            (string-equal (buffer-file-name)
                          (expand-file-name "~/dotfiles/qutebrowser/README.org"))
            (string-equal (buffer-file-name)
                          (expand-file-name "~/dotfiles/emacs/README.org"))
            (string-equal (buffer-file-name)
                          (expand-file-name "~/dotfiles/emacs/desktop.org"))
            (string-equal (buffer-file-name)
                          (expand-file-name "~/dotfiles/herbstluftwm/README.org"))
            (string-equal (buffer-file-name)
                          (expand-file-name "~/dotfiles/rofi/README.org"))
            (string-equal (buffer-file-name)
                          (expand-file-name "~/dotfiles/fonts/README.org"))
            (string-equal (buffer-file-name)
                          (expand-file-name "~/dotfiles/polybar/README.org"))
            (string-equal (buffer-file-name)
                          (expand-file-name "~/dotfiles/emacs/local.org")))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'org/org-babel-tangle-config)))

 (use-package! dired
   :hook
   (dired-mode . dired-hide-details-mode))

(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "xdg-open" nil 0 nil file)
    (message "Opening %s done" file)))

(defun dired-open-home-dir ()
  "Open the home directory in dired"
  (interactive)
  (dired "~"))

(defun dired-open-current-dir ()
  "Open the current directory in dired"
  (interactive)
  (dired "."))

 (use-package! all-the-icons-dired
   :hook
   (dired-mode . all-the-icons-dired-mode))

(use-package! dired-single
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
   "h" 'dired-single-up-directory
   "l" 'dired-single-buffer
   (kbd "<C-return>") #'dired-open-file))

(use-package! dired-hide-dotfiles
  :hook
  (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "." 'dired-hide-dotfiles-mode))

(defun blamer-callback-show-commit-diff (commit-info)
  (interactive)
  (let ((commit-hash (plist-get commit-info :commit-hash)))
    (when commit-hash
      (magit-show-commit commit-hash))))

(defun blamer-callback-open-remote (commit-info)
  (interactive)
  (let ((commit-hash (plist-get commit-info :commit-hash)))
    (when commit-hash
      (message commit-hash)
      (forge-browse-commit commit-hash))))

(setq blamer-idle-time 0.5)
(setq blamer-min-offset 60)
(setq blamer-bindings '(("<mouse-3>" . blamer-callback-open-remote)
                          ("<mouse-1>" . blamer-callback-show-commit-diff)))
(setq blamer-view 'overlay)
;; (setq blamer-type 'overlay-popup)
;; (setq blamer--overlay-popup-position 'smart)
(global-blamer-mode 1)

(use-package! adoc-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))
  (add-hook 'adoc-mode-hook (lambda()
                              (buffer-face-mode t))))

(map! :leader
    :desc "Kubel"
    "o k" #'kubel)
(let ((local-settings "~/.emacs.d/.local/straight/repos/kubel/kubel-evil.el"))
  (when (file-exists-p local-settings)
      (load-file local-settings)))

(use-package! lsp-mode
  :custom
  (lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/dev/stderr")))

(setq vterm-shell "/bin/zsh")
(setq vterm-buffer-name-string "vterm: %s")

(theme/doom-nord)
