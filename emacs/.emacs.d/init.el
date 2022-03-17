(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(setq gc-cons-threshold 50000000)

(setq large-file-warning-threshold 100000000)

(setq read-process-output-max (* 5 (* 1024 1024)))

(setq tab-always-indent 'complete)
(defalias 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "C-z") 'delete-frame)

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))
(setq create-lockfiles nil)
(setq projectile-known-projects-file (expand-file-name "tmp/projectile-bookmarks.eld" user-emacs-directory)
      lsp-session-file (expand-file-name "tmp/.lsp-session-v1" user-emacs-directory))

(use-package no-littering)

(setq indent-tabs-mode nil)
(setq indent-line-function 'insert-tab)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq c-basic-offset tab-width
      c-basic-offset tab-width
      c-basic-offset tab-width
      csharp-tree-sitter-indent-offset tab-width
      c-basic-offset tab-width
      c-basic-offset tab-width
      c-basic-offset tab-width
      js-indent-level tab-width
      js2-basic-offset tab-width
      js3-indent-level tab-width
      js-indent-level tab-width
      lua-indent-level tab-width
      c-basic-offset tab-width
      c-basic-offset tab-width
      perl-indent-level tab-width
      cperl-indent-level tab-width
      raku-indent-offset tab-width
      erlang-indent-level tab-width
      ada-indent tab-width
      sgml-basic-offset tab-width
      nxml-child-indent tab-width
      pascal-indent-level tab-width
      typescript-indent-level tab-width
      sh-basic-offset tab-width
      ruby-indent-level tab-width
      enh-ruby-indent-level tab-width
      crystal-indent-level tab-width
      css-indent-offset tab-width
      rust-indent-offset tab-width
      rustic-indent-offset tab-width
      scala-indent:step tab-width
      powershell-indent tab-width
      ess-indent-offset tab-width
      yaml-indent-offset tab-width
      hack-indent-offset tab-width
      standard-indent tab-width)

(setq warning-minimum-level :error)

(use-package multiple-cursors
    :config
    (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-?") 'mc/mark-all-like-this)
    (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click))

(use-package sudo-edit)

(use-package emacs-everywhere)

(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region)
  (global-set-key (kbd "C--") 'er/contract-region))

(use-package pulsar
  :straight (pulsar :type git :host gitlab :repo "protesilaos/pulsar")
  :config
  (pulsar-setup)
  (global-set-key (kbd "C-c SPC") 'pulsar-pulse-line)
  (setq pulse-flag t)
  (set-face-attribute 'pulsar-cyan nil :background "#81a1c1")
  (setq pulsar-face 'pulsar-cyan))

(scroll-bar-mode 0)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; Add frame borders and window dividers
(modify-all-frames-parameters
 '((right-divider-width . 40)
   (internal-border-width . 40)))

(set-face-attribute 'default nil :font "SauceCodePro NF" :height 100)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "SauceCodePro NF" :height 100)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 100 :weight 'regular)

(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
  :after all-the-icons)

(use-package doom-themes
   :custom-face
   (org-block ((t (:background "#272C36"))))
   (org-block-begin-line ((t (:background "#272C36"))))
   (org-block-end-line ((t (:background "#272C36"))))
   (window-divider ((t (:foreground "#2e3440"))))
   (window-divider-first-pixel ((t (:foreground "#2e3440"))))
   (window-divider-last-pixel ((t (:foreground "#2e3440"))))
   :hook (server-after-make-frame . (lambda () (load-theme
                                              'doom-nord t)))
   :config
   (doom-themes-treemacs-config)
   (defun doom-themes-hide-modeline ())
   (doom-themes-org-config))

(defun darken-buffer ()
  (setq buffer-face-mode-face `(:background "#272C36"))
  (face-remap-add-relative 'hl-line `(:background "#2e3440"))
  (face-remap-add-relative 'fringe `(:background "#272C36"))
  (buffer-face-mode 1))

(add-hook 'treemacs-mode-hook #'darken-buffer)
(add-hook 'help-mode-hook #'darken-buffer)
(add-hook 'helpful-mode-hook #'darken-buffer)

(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

(use-package olivetti
  :config
  (setq olivetti-margin-width 0)
  (setq olivetti-minimum-body-width 100))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package vertico
  :straight (vertico :type git :host github :repo "minad/vertico")
  :config
  (setq vertico-cycle t)
  (vertico-mode))

(use-package embark
  :straight t
  :bind (("C-c e" . embark-act)
	 :map minibuffer-local-map
	 ("C-d" . embark-act)))

(use-package wgrep)

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s e" . consult-isearch-history)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  (setq completion-in-region-function
    (lambda (&rest args)
      (apply (if vertico-mode
                 #'consult-completion-in-region
               #'completion--in-region)
             args))))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
	 :map minibuffer-local-map
	 ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package cape
  ;; Bind dedicated completion commands
  :bind (("C-c p p" . completion-at-point) ;; capf
	 ("C-c p t" . complete-tag)        ;; etags
	 ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
	 ("C-c p f" . cape-file)
	 ("C-c p k" . cape-keyword)
	 ("C-c p s" . cape-symbol)
	 ("C-c p a" . cape-abbrev)
	 ("C-c p i" . cape-ispell)
	 ("C-c p l" . cape-line)
	 ("C-c p w" . cape-dict)
	 ("C-c p \\" . cape-tex)
	 ("C-c p _" . cape-tex)
	 ("C-c p ^" . cape-tex)
	 ("C-c p &" . cape-sgml)
	 ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)

(use-package savehist
  :init
  (savehist-mode))

(use-package helpful
  :config
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function)
  (global-set-key (kbd "C-h C") #'helpful-command))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package consult-flycheck)

(use-package rainbow-mode)

(use-package highlight-parentheses
  :config
  (global-highlight-parentheses-mode 1))

(use-package smartparens
  :config
  (add-hook 'lsp-mode-hook #'smartparens-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package magit)

(use-package yasnippet
  :hook ((lsp-mode . yas-minor-mode)))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/dev/stderr"))
  :bind (
         ("C-h ." . lsp-describe-thing-at-point)
         ("C-." . lsp-execute-code-action)
         )
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (js-mode . lsp)
         ;; if you want which-key integration
         ;;(lsp-mode . (lambda () (add-hook 'before-save-hook #'lsp-format-buffer)))
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq
   lsp-idle-delay 0.500
   lsp-log-io nil
   lsp-headerline-breadcrumb-enable nil
   lsp-eldoc-render-all t))

(use-package dap-mode)

(use-package adoc-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode)))

(use-package restclient
  :config
  (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))

(use-package ob-restclient
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

(use-package kubel)

(use-package dockerfile-mode)

(use-package docker-compose-mode)

(use-package docker
  :config
  (define-derived-mode docker-container-mode tabulated-list-mode "Containers Menu"
    "Major mode for handling a list of docker containers."
    (setq tabulated-list-format [("Id" 5 t)("Image" 5 t)("Command" 10 t)("Created" 10 t)("Status" 10 t)("Ports" 35 t)("Names" 30 t)])
    (setq tabulated-list-padding 2)
    (setq tabulated-list-sort-key docker-container-default-sort-key)
    (add-hook 'tabulated-list-revert-hook 'docker-container-refresh nil t)
    (tabulated-list-init-header)
    (tablist-minor-mode))

  (setq docker-container-shell-file-name "/bin/sh")

  (add-hook 'docker-container-mode 'docker/set-format))

(use-package vterm
    :config
    (setq
     vterm-shell "/bin/zsh"
     vterm-buffer-name-string "vterm: %s"))

(use-package treemacs
  :config
  (setq
   treemacs-width 45
   treemacs-default-visit-action 'treemacs-visit-node-close-treemacs)
  (global-set-key (kbd "C-c t") 'treemacs))

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

(use-package dired
  :straight (:type built-in)
  :bind (
         ("<C-return>" . dired-open-file)
         ("M-p" . dired-up-directory)
         ("M-n" . dired-find-file))
  :hook
  (dired-mode . dired-hide-details-mode))

(use-package dired-single)

(use-package dired-hide-dotfiles
  :hook
  (dired-mode . dired-hide-dotfiles-mode))

(use-package lsp-ltex
  :hook (text-mode . (lambda ()
                       (require 'lsp-ltex)
                       (lsp))))

(use-package org
  :config
  (setq org-confirm-babel-evaluate nil)
  (defun org/org-babel-tangle-config ()
    (when (or (string-equal (buffer-file-name)
      (expand-file-name "~/dotfiles/README.org"))
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
      (expand-file-name "~/dotfiles/polybar/README.org"))
    (string-equal (buffer-file-name)
      (expand-file-name "~/dotfiles/emacs/local.org")))
  ;; Dynamic scoping to the rescue
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle))))
    (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'org/org-babel-tangle-config))))

(use-package org-modern
  :config
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"

   ;; Agenda styling
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")

  ;; Enable org-modern-mode
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

(use-package org-jira
  :straight (org-jira :type git :host github :repo "ahungry/org-jira"
                      :fork (:host github
                                   :repo "Vivien-lelouette/org-jira"))
  :after org)

(let ((local-settings "~/.emacs.d/local.el"))
    (when (file-exists-p local-settings)
	(load-file local-settings)))
