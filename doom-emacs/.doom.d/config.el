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

(setq display-line-numbers 'relative)
(setq display-line-numbers-width-start t)
(setq display-line-numbers-grow-only t)

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
  (dired-mode . dired-hide-details-mode)
  :config
  (add-hook 'dired-mode-hook (lambda()
                               (setq display-line-numbers 'relative))))

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

(use-package! treemacs
  :config
  (add-hook 'treemacs-mode-hook (lambda()
                               (setq display-line-numbers 'relative))))

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
;; (global-blamer-mode 1)

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

(use-package! dap-mode
  :config
  (map! :map dap-mode-map
      :leader
      :prefix ("d" . "dap")
      ;; basics
      :desc "dap next"          "n" #'dap-next
      :desc "dap step in"       "i" #'dap-step-in
      :desc "dap step out"      "o" #'dap-step-out
      :desc "dap continue"      "c" #'dap-continue
      :desc "dap hydra"         "h" #'dap-hydra
      :desc "dap debug restart" "r" #'dap-debug-restart
      :desc "dap debug"         "s" #'dap-debug

      ;; debug
      :prefix ("dd" . "Debug")
      :desc "dap debug recent"  "r" #'dap-debug-recent
      :desc "dap debug last"    "l" #'dap-debug-last

      ;; eval
      :prefix ("de" . "Eval")
      :desc "eval"                "e" #'dap-eval
      :desc "eval region"         "r" #'dap-eval-region
      :desc "eval thing at point" "s" #'dap-eval-thing-at-point
      :desc "add expression"      "a" #'dap-ui-expressions-add
      :desc "remove expression"   "d" #'dap-ui-expressions-remove

      :prefix ("db" . "Breakpoint")
      :desc "dap breakpoint toggle"      "b" #'dap-breakpoint-toggle
      :desc "dap breakpoint condition"   "c" #'dap-breakpoint-condition
      :desc "dap breakpoint hit count"   "h" #'dap-breakpoint-hit-condition
      :desc "dap breakpoint log message" "l" #'dap-breakpoint-log-message))

(use-package! aggressive-indent
  :config
  (add-to-list 'aggressive-indent-dont-indent-if
             '(and (eq (char-before) ?\s) (looking-at-p "$")))
  (global-aggressive-indent-mode 1))

(use-package! shr
  :config
  (setq gnus-inhibit-images nil)
  (setq shr-use-fonts nil)
  (setq shr-use-colors nil)
  (setq shr-max-image-proportion 1)
  (setq shr-width nil)
  (setq shr-folding-mode t))

;; Used to highlight code
(use-package! shr-tag-pre-highlight
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight))
  (when (version< emacs-version "26")
    (with-eval-after-load 'eww
      (advice-add 'eww-display-html :around
                  'eww-display-html--override-shr-external-rendering-functions))))

(use-package! shrface
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings)
  (setq shrface-href-versatile t)

  ;; Code highlighting
  (require 'shr-tag-pre-highlight)
  (add-to-list 'shr-external-rendering-functions '(pre . shrface-shr-tag-pre-highlight))
  (defun shrface-shr-tag-pre-highlight (pre)
    "Highlighting code in PRE."
    (let* ((shr-folding-mode 'none)
           (shr-current-font 'default)
           (code (with-temp-buffer
                   (shr-generic pre)
                   (setq-local fill-column 120)
                   (indent-rigidly (point-min) (point-max) 2)
                   (if (eq "" (dom-texts pre))
                       nil
                     (progn
                       (setq-local fill-column shrface-paragraph-fill-column)
                       (indent-rigidly (point-min) (point-max) shrface-paragraph-indentation)))
                   (buffer-string)))
           (lang (or (shr-tag-pre-highlight-guess-language-attr pre)
                     (let ((sym (language-detection-string code)))
                       (and sym (symbol-name sym)))))
           (mode (and lang
                      (shr-tag-pre-highlight--get-lang-mode lang))))
      (shr-ensure-newline)
      (insert (propertize (concat "#+BEGIN_SRC " lang) 'face 'org-block-begin-line))
      (shr-ensure-newline)
      (setq start (point))
      (insert
       (or (and (fboundp mode)
                (with-demoted-errors "Error while fontifying: %S"
                  (shrface-tag-pre-highlight-fontify code mode)
                  ))
           code))
      (shr-ensure-newline)
      (setq end (point))
      (insert (propertize "#+END_SRC" 'face 'org-block-end-line ) )
      (shr-ensure-newline)
      (insert "\n"))))

(use-package! eww
  :config
  (define-key eww-image-link-keymap (kbd "TAB") nil)
  (define-key eww-link-keymap (kbd "TAB") nil)
  (define-key eww-mode-map (kbd "TAB") nil)
  (define-key eww-text-map (kbd "TAB") nil)
  (define-key eww-textarea-map (kbd "TAB") nil)
  (define-key eww-mode-map (kbd "<normal-state> ^") nil)
  (define-key eww-mode-map (kbd "<normal-state> <tab>") 'shrface-outline-cycle)
  (define-key eww-mode-map (kbd "<normal-state> <backtab>") nil)

  (require 'shrface))

(add-hook! 'eww-after-render-hook #'shrface-mode)
(add-hook! 'eww-after-render-hook #'mixed-pitch-mode)

(use-package! eww
  :config
  (define-key eww-image-link-keymap (kbd "TAB") nil)
  (define-key eww-link-keymap (kbd "TAB") nil)
  (define-key eww-mode-map (kbd "TAB") nil)
  (define-key eww-text-map (kbd "TAB") nil)
  (define-key eww-textarea-map (kbd "TAB") nil)
  (define-key eww-mode-map (kbd "<normal-state> ^") nil)
  (define-key eww-mode-map (kbd "<normal-state> <tab>") 'shrface-outline-cycle)
  (define-key eww-mode-map (kbd "<normal-state> <backtab>") nil)

  (require 'shrface))

(add-hook! 'eww-after-render-hook #'shrface-mode)
(add-hook! 'eww-after-render-hook #'mixed-pitch-mode)

;(use-package! eaf
;  :load-path "~/.emacs.d/.local/straight/repos/emacs-application-framework"
;  :custom
;  ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
;  (eaf-browser-continue-where-left-off t)
;  (eaf-browser-enable-adblocker t)
;  (eaf-browser-default-search-engine "duckduckgo")
;  (browse-url-browser-function 'eaf-open-browser)
;  (eaf-wm-focus-fix-wms `("i3" "LG3D" "Xpra" "EXWM" "Xfwm4" "herbstluftwm"))
;
;  :config
;  (use-package! ctable)
;  (use-package! deferred)
;  (use-package! epc)
;  (use-package! eaf-browser)
;  (use-package! eaf-pdf-viewer)
;  (use-package! eaf-image-viewer)
;  (use-package! eaf-evil)
;  (defun browser-focus-an-input ()
;    (eaf-call-sync "execute_function" eaf--buffer-id "is_focus"))
;  (define-key key-translation-map (kbd "SPC")
;    (lambda (prompt)
;      (if (derived-mode-p 'eaf-mode)
;          (pcase eaf--buffer-app-name
;            ("browser" (if (browser-focus-an-input)
;                           (kbd "SPC")
;                         (kbd eaf-evil-leader-key)))
;            ("pdf-viewer" (kbd eaf-evil-leader-key))
;            ("image-viewer" (kbd eaf-evil-leader-key))
;            (_  (kbd "SPC")))
;        (kbd "SPC"))))
;  (defalias 'browse-web #'eaf-open-browser))

(use-package! vterm
  :config
  (setq vterm-shell "/bin/zsh")
  (setq vterm-buffer-name-string "vterm: %s")
  (add-hook 'vterm-mode-hook (lambda()
                                  (setq display-line-numbers 'relative))))

(theme/doom-nord)
