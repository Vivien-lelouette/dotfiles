(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(let ((file-name-handler-alist nil))
  (require 'package)
  (setq package-enable-at-startup nil)

;; Avoid blinding white at startup
(load-theme 'wombat)

(setq inhibit-startup-message t)

;; (scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
;; (set-fringe-mode 10)        ; Give some breathing room

;; (setq-default window-divider-default-right-width 10)
;; (setq-default window-divider-default-bottom-width 10)
;; (setq-default window-divider-default-places t)
;; (add-hook 'after-init-hook #'window-divider-mode)

(menu-bar-mode -1)            ; Disable the menu bar

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

(global-display-line-numbers-mode 1)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Replace yes no answers by y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Let emacs use system clipboard as a default behaviour
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-selection-value)

;; Winner-mode
(winner-mode 1)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Nativ comp flags
(setq comp-speed 3)
(setq comp-async-report-warnings-errors nil)
(setq package-native-compile t)
(setq comp-deferred-compilation t)

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

(defun fonts/set-size (font-size)
  (set-face-attribute 'default nil :font "Fira Code" :height font-size)
  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil :font "Fira Code" :height font-size)
  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font "Cantarell" :height font-size :weight 'regular))

(defun fonts/small-size ()
  (interactive)
  (fonts/set-size 90))

(defun fonts/normal-size ()
    (interactive)
    (fonts/set-size 100))

(defun fonts/big-size ()
  (interactive)
  (fonts/set-size 120))

(defun fonts/huge-size ()
  (interactive)
  (fonts/set-size 140))

(fonts/small-size)

(setq ibuffer-formats
      '((mark modified read-only locked " "
              (icon 2 2 :left :elide)
              #(" " 0 1
                (display
                 (space :align-to 8)))
              (name 50 50 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 12 :left)
              " " filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename)))

(defun ibuffer-custom-filter-groups ()
  (append
   '(
     ("Applications" (and
                      (mode . exwm-mode)
                      (not (name . "qutebrowser:.*"))
                      (not (name . "Firefox:.*"))))
     ("Qutebrowser" (name . "qutebrowser:.*"))
     ("Firefox" (name . "Firefox:.*")))
   (ibuffer-projectile-generate-filter-groups)
   )
  )

;; Useful when using exwm
;; (add-hook 'ibuffer-mode-hook
;;           (lambda ()
;;             (setq ibuffer-saved-filter-groups
;;              (list
;;               (append
;;                '("custom")
;;                (ibuffer-custom-filter-groups))))
;;                (ibuffer-switch-to-saved-filter-groups "custom")
;;             (ibuffer-auto-mode 1)))

(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-projectile-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))
            (ibuffer-auto-mode 1)))

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

(use-package async)

(use-package trashed)

(use-package bbdb)

(use-package dianyou)

(use-package general
  :config
  (general-create-definer keys/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "s-d")

  (keys/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package undo-fu)

(use-package undo-fu-session
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (global-undo-fu-session-mode))

(use-package evil
    :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (setq evil-want-C-u-scroll t)
    (setq evil-want-C-i-jump nil)
    :config
    (evil-mode 1)
    (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
    (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

    ;; Use visual line motions even outside of visual-line-mode buffers
    (evil-global-set-key 'motion "j" 'evil-next-visual-line)
    (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

    (evil-set-initial-state 'messages-buffer-mode 'normal)
    (evil-set-initial-state 'dashboard-mode 'normal)
    (evil-set-undo-system 'undo-fu))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package treemacs-evil
  :after evil)

(use-package evil-multiedit
  :after evil
  :config
  (evil-multiedit-default-keybinds))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode)
  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

(use-package hydra
  :after general)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(keys/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :after all-the-icons
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
  :after all-the-icons)

(use-package ibuffer-vc)

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

(defun theme/doom-dark+ ()
  (interactive)
  (load-theme 'doom-dark+ t)
  (set-face-attribute 'fringe nil :background "#1e1e1e")
  (set-face-attribute 'mode-line-inactive nil :background "#252526")

  ;; Line number styling for mode change
  (setq theme/normal-lines-fg "#707070")
  (setq theme/normal-lines-bg "#1e1e1e")
  (setq theme/normal-current-line-fg "#ffffff")
  (setq theme/normal-current-line-bg "#121212")

  (setq theme/insert-lines-fg "#707070")
  (setq theme/insert-lines-bg "#1c3319")
  (setq theme/insert-current-line-fg "#ffffff")
  (setq theme/insert-current-line-bg "#579c4c")

  (setq theme/visual-lines-fg "#707070")
  (setq theme/visual-lines-bg "#00332a")
  (setq theme/visual-current-line-fg "#ffffff")
  (setq theme/visual-current-line-bg "#009b80"))

(defun theme/doom-nord ()
  (interactive)
  (load-theme 'doom-nord t)
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
  (setq theme/visual-current-line-bg "#b48ead"))

(use-package doom-themes
  :config
  (setq doom-themes-treemacs-theme "doom-colors")
  (theme/doom-nord))

(use-package minions)

(defun simple-modeline-segment-minions ()
  "Displays the current major and minor modes with minions-mode in the mode-line."
  (concat " " (format-mode-line minions-mode-line-modes)))

(use-package simple-modeline
 :hook (after-init . simple-modeline-mode)
 :config
 (setq simple-modeline-segments '((simple-modeline-segment-modified simple-modeline-segment-buffer-name simple-modeline-segment-position) (simple-modeline-segment-input-method simple-modeline-segment-eol simple-modeline-segment-encoding simple-modeline-segment-vc simple-modeline-segment-misc-info simple-modeline-segment-process simple-modeline-segment-minions))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package ace-jump-mode
  :config
  (keys/leader-keys
    "f" '(evil-ace-jump-word-mode :which-key "Go to word")))

(defun efs/treemacs-set-fringe ()
  (setq left-fringe-width 0)
  (setq right-fringe-width 0))

(use-package treemacs
  :config
  (add-hook 'treemacs-mode-hook #'efs/treemacs-set-fringe))

(use-package treemacs-all-the-icons
  :after all-the-icons
  :config
  (treemacs-load-theme "all-the-icons"))

(efs/treemacs-set-fringe)

(use-package volatile-highlights)

(use-package highlight-parentheses
  :config
  (global-highlight-parentheses-mode 1))

(use-package flycheck)

(use-package writeroom-mode
  :config
  (setq writeroom-global-effects '(writeroom-set-alpha writeroom-set-menu-bar-lines writeroom-set-tool-bar-lines writeroom-set-vertical-scroll-bars writeroom-set-bottom-divider-width)))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (keys/leader-keys
    "y" #'counsel-yank-pop))

(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1)
  (setq ivy-initial-inputs-alist nil))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

(use-package company
  :bind (:map company-active-map
              ("<tab>" . company-select-next)
              ("<backtab>" . company-select-previous))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.2)
  :config
  (global-company-mode 1))

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))

(use-package prescient)

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1))
(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1))
(prescient-persist-mode 1)

(use-package avy)

(use-package image-dired)

(use-package dired
  :straight (:type built-in)
  :hook (dired-mode . dired-hide-details-mode)
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "\C-H" 'dired-do-hardlink
    "\C-L" 'dired-do-load))

(use-package dired-single
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-single-up-directory
    "L" 'dired-single-buffer))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "\M-h" 'dired-hide-dotfiles-mode))

(use-package dired-subtree
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

(use-package ranger)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after counsel
  :config (counsel-projectile-mode))

(use-package ibuffer-projectile)

(use-package magit
  :config
  (keys/leader-keys
    "gg" '(magit :which-key "magit status"))
   (keys/leader-keys
    "gf" '(magit-log-buffer-file :which-key "magit file history")))

(use-package forge
  :after magit)

(use-package evil-nerd-commenter
  :after evil
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package format-all
  :bind ("C-c C-f" . format-all-buffer))

(use-package dap-mode)

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top))

(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

(use-package rainbow-mode)

(use-package yasnippet)

(defun my-setup-indent (n)
  ;; java/c/c++
  (setq-local c-basic-offset n)
  ;; web development
  (setq-local coffee-tab-width n) ; coffeescript
  (setq-local javascript-indent-level n) ; javascript-mode
  (setq-local js-indent-level n) ; js-mode
  (setq-local rjsx-basic-offset n)
  (setq-local rjsx-indent-level n)
  (setq-local web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq-local web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq-local web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq-local css-indent-offset n) ; css-mode
)

(defun efs/lsp-mode-setup ()
  (my-setup-indent 2)
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode)
  (let ((lsp-keymap-prefix "C-SPC"))
  (lsp-enable-which-key-integration)))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-SPC")  ;; Or 'C-l', 's-l'
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :config
  (setq lsp-idle-delay 0.500)
  (setq lsp-completion-provider :capf)
  (define-key lsp-mode-map (kbd "C-SPC") lsp-command-map)
  (define-key lsp-mode-map (kbd "s-l") nil))

(add-hook 'lsp-mode-hook 'highlight-indent-guides-mode)

(use-package lsp-ui
  :after lsp
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-position 'at-point))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy
  :after lsp)

(use-package typescript-mode
  :mode ("\\.ts\\'")
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2)
  (require 'dap-node)
  (dap-node-setup))

(defun efs/js-mode-setup ()
  (lsp-deferred)
  (require 'dap-node)
  (dap-node-setup))

(add-hook 'js-mode-hook 'efs/js-mode-setup)

(add-hook 'sh-mode-hook 'lsp-deferred)

(use-package yaml-mode
  :straight (yaml-mode :type git :host github :repo "yoshiki/yaml-mode")
  :config
  (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode))

(use-package json-mode
  :config
  (add-hook 'json-mode-hook 'highlight-indent-guides-mode))

(use-package jq-mode)

(use-package restclient
  :config
  (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))

(use-package company-restclient
  :after restclient
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package adoc-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode)))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-html-inline-images t)
  (setq org-hide-emphasis-markers t)



  (setq org-agenda-files
        '("~/.org-files/tasks.org"
          "~/.org-files/habits.org"
          "~/.org-files/birthdays.org"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
        '(("Archive.org" :maxlevel . 1)
          ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
        '((:startgroup)
                                        ; Put mutually exclusive tags here
          (:endgroup)
          ("@errand" . ?E)
          ("@home" . ?H)
          ("@work" . ?W)
          ("agenda" . ?a)
          ("planning" . ?p)
          ("publish" . ?P)
          ("batch" . ?b)
          ("note" . ?n)
          ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))
            (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

          ("n" "Next Tasks"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))

          ("W" "Work Tasks" tags-todo "+work-email")

          ;; Low-effort next actions
          ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
           ((org-agenda-overriding-header "Low Effort Tasks")
            (org-agenda-max-todos 20)
            (org-agenda-files org-agenda-files)))

          ("w" "Workflow Status"
           ((todo "WAIT"
                  ((org-agenda-overriding-header "Waiting on External")
                   (org-agenda-files org-agenda-files)))
            (todo "REVIEW"
                  ((org-agenda-overriding-header "In Review")
                   (org-agenda-files org-agenda-files)))
            (todo "PLAN"
                  ((org-agenda-overriding-header "In Planning")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "BACKLOG"
                  ((org-agenda-overriding-header "Project Backlog")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "READY"
                  ((org-agenda-overriding-header "Ready for Work")
                   (org-agenda-files org-agenda-files)))
            (todo "ACTIVE"
                  ((org-agenda-overriding-header "Active Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "COMPLETED"
                  ((org-agenda-overriding-header "Completed Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "CANC"
                  ((org-agenda-overriding-header "Cancelled Projects")
                   (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
        `(("t" "Tasks / Projects")
          ("tt" "Task" entry (file+olp "~/Projects/Code/emacs-from-scratch/OrgFiles/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

          ("j" "Journal Entries")
          ("jj" "Journal" entry
           (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
          ("jm" "Meeting" entry
           (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

          ("w" "Workflows")
          ("we" "Checking Email" entry (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

          ("m" "Metrics Capture")
          ("mw" "Weight" table-line (file+headline "~/Projects/Code/emacs-from-scratch/OrgFiles/Metrics.org" "Weight")
           "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj")))

  (efs/org-font-setup))

(use-package hide-mode-line)

(defun org/presentation-setup ()
  ;; Hide the mode line
  ;; (hide-mode-line-mode 1)

  (display-line-numbers-mode 0)

  ;; Display images inline
  (org-display-inline-images) ;; Can also use org-startup-with-inline-images

  ;; Scale the text.  The next line is for basic scaling:
  (setq text-scale-mode-amount 3)
  (text-scale-mode 1)
  (writeroom-mode 1))

(defun org/presentation-end ()
  ;; Show the mode line again
  ;; (hide-mode-line-mode 0)

  (display-line-numbers-mode 1)

  ;; Turn off text scale mode (or use the next line if you didn't use text-scale-mode)
  (text-scale-mode 0)
  (writeroom-mode 0))

(use-package org-tree-slide
  :hook ((org-tree-slide-play . org/presentation-setup)
         (org-tree-slide-stop . org/presentation-end))
  :custom
  (org-tree-slide-activate-message "Presentation started!")
  (org-tree-slide-deactivate-message "Presentation finished!")
  (org-tree-slide-breadcrumbs " > ")
  (org-tree-slide-skip-outline-level 4)
  (org-tree-slide-slide-in-effect nil)
  (org-tree-slide-header t)
  (org-tree-slide-fold-subtrees-skipped nil)
  (org-image-actual-width nil))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

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
                        (expand-file-name "~/dotfiles/emacs/local.org")))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'org/org-babel-tangle-config)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

(setq org-confirm-babel-evaluate nil)

(use-package org-mime
  :after org)

(use-package org-web-tools
  :after org)

(use-package ob-restclient
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

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

  (defun docker/dcup (string-services)
    (interactive "sDocker services to start: ")
    (setq docker-services (split-string string-services))
    (cl-loop for service in docker-services
    collect (docker-compose-run-docker-compose-async "up" service)))

  (setq docker-container-shell-file-name "/bin/sh")

  (add-hook 'docker-container-mode 'docker/set-format)

  (keys/leader-keys
    "d"  'docker
    "D"  'docker-compose))

(use-package kubernetes
  :config
  (setq kubernetes-redraw-frequency 3600)
  (setq kubernetes-poll-frequency 3600))

(use-package kubernetes-evil)

(defun kubernetes/refresh ()
  (interactive)
  (kubernetes-statefulsets-refresh)
  (kubernetes-deployments-refresh-now)
  (kubernetes-jobs-refresh-now)
  (kubernetes-pods-refresh-now))

(use-package flyspell
  :straight (:type built-in)
  :diminish
  :if (executable-find "aspell")
  :hook (((text-mode outline-mode latex-mode) . flyspell-mode))
  :custom
  (flyspell-issue-message-flag nil)
  (ispell-program-name "aspell")
  (ispell-extra-args
   '("--sug-mode=ultra" "--lang=en_US"))
  :config
  (use-package flyspell-correct-ivy
    :after ivy
    :bind
    (:map flyspell-mode-map
          ([remap flyspell-correct-word-before-point] . flyspell-correct-wrapper)
          ("C-." . flyspell-correct-wrapper))
    :custom (flyspell-correct-interface #'flyspell-correct-ivy)))

(use-package guess-language
  :config
  (setq guess-language-languages '(en fr))
  (add-hook 'flyspell-mode-hook (lambda () (guess-language-mode 1))))

(use-package langtool
  :straight (langtool :type git :host github :repo "mhayashi1120/Emacs-langtool")
  :config
  (setq langtool-language-tool-server-jar "~/Tools/LanguageTool/languagetool-server.jar"))

(use-package vterm

  :config
  (setq vterm-shell "/bin/zsh")
  (setq vterm-buffer-name-string "vterm: %s"))

(use-package term
  :config
  (setq explicit-shell-file-name "sh")

  ;; Use 'explicit-<shell>-args for shell-specific args
  ;;(setq explicit-zsh-args '())         

  (setq evil-move-cursor-back t)

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package windmove)

(use-package windsize)

(use-package zoom
  :config
  (setq zoom-size '(0.618 . 0.618)))

(use-package frames-only-mode)

(use-package shr
  :config
  (setq gnus-inhibit-images nil)
  (setq shr-use-fonts nil)
  (setq shr-use-colors nil)
  (setq shr-max-image-proportion 1)
  (setq shr-width nil)
  (setq shr-folding-mode t))

;; Used to highlight code
(use-package shr-tag-pre-highlight
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight))
  (when (version< emacs-version "26")
    (with-eval-after-load 'eww
      (advice-add 'eww-display-html :around
                  'eww-display-html--override-shr-external-rendering-functions))))

(use-package shrface
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

(use-package eww
  :init
  (add-hook 'eww-after-render-hook #'shrface-mode)
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

(use-package gnus
  :init
  (add-hook 'gnus-article-mode-hook #'shrface-mode)
  :config
  (require 'nnir)

  ;; Please note mail folders in `gnus-select-method' have NO prefix like "nnimap+hotmail:" or "nnimap+gmail:"
  (setq gnus-select-method '(nnnil)) ;; Read feeds/atom through gwene

  ;; ask encryption password once
  (setq epa-file-cache-passphrase-for-symmetric-encryption t)

  ;; @see http://gnus.org/manual/gnus_397.html
  (defun gnus/add-gmail-select-method (account-name)
    (add-to-list 'gnus-secondary-select-methods
                 (list 'nnimap account-name
                       (list 'Nnimap-address "imap.gmail.com")
                       (list 'Nnimap-server-port 993)
                       (list 'Nnimap-stream 'ssl)
                       (list 'Nnir-search-engine 'imap)
                       ;; @see http://www.gnu.org/software/emacs/manual/html_node/gnus/Expiring-Mail.html
                       ;; press 'E' to expire email
                       (list 'nnmail-expiry-target (concat "nnimap+" account-name ":[Gmail]/Corbeille"))
                       (list 'nnmail-expiry-wait 90))))

  (defun gnus/add-gmail-topic (account-name)
    (list account-name ; the key of topic
          (concat "nnimap+" account-name ":INBOX")
          (concat "nnimap+" account-name ":[Gmail]/Brouillons")
          (concat "nnimap+" account-name ":[Gmail]/Messages envoyés")
          (concat "nnimap+" account-name ":[Gmail]/Important")
          (concat "nnimap+" account-name ":[Gmail]/Tous les messages")
          (concat "nnimap+" account-name ":[Gmail]/Corbeille")
          (concat "nnimap+" account-name ":[Gmail]/Suivis")
          (concat "nnimap+" account-name ":[Gmail]/Spam")
          (concat "nnimap+" account-name ":Planifié")
          (concat "nnimap+" account-name ":Archive")
          (concat "nnimap+" account-name ":Trash")
          (concat "nnimap+" account-name ":Sent")
          (concat "nnimap+" account-name ":Conversation History")
          (concat "nnimap+" account-name ":Accusés de réception")
          (concat "nnimap+" account-name ":Professionnel")
          (concat "nnimap+" account-name ":Professionnel/OPTRAJ")))

  (add-to-list 'gnus-secondary-select-methods
               '(nnimap "vivperso"
                        (nnimap-address "imap.gmail.com")
                        (nnimap-server-port 993)
                        (nnimap-stream ssl)
                        (nnir-search-engine imap)
                        ;; @see http://www.gnu.org/software/emacs/manual/html_node/gnus/Expiring-Mail.html
                        ;; press 'E' to expire email
                        (nnmail-expiry-target "nnimap+vivperso:[Gmail]/Corbeille")
                        (nnmail-expiry-wait 90)))

  (add-to-list 'gnus-secondary-select-methods
               '(nnimap "lelouette.vivien"
                        (nnimap-address "imap.gmail.com")
                        (nnimap-server-port 993)
                        (nnimap-stream ssl)
                        (nnir-search-engine imap)
                        ;; @see http://www.gnu.org/software/emacs/manual/html_node/gnus/Expiring-Mail.html
                        ;; press 'E' to expire email
                        (nnmail-expiry-target "nnimap+lelouette.vivien:[Gmail]/Corbeille")
                        (nnmail-expiry-wait 90)))

  (setq gnus-thread-sort-functions
        '(gnus-thread-sort-by-most-recent-date
          (not gnus-thread-sort-by-number)))


  ;; press "o" to view all groups
  (defun gnus/group-list-subscribed-groups ()
    "List all subscribed groups with or without un-read messages"
    (interactive)
    (gnus-group-list-all-groups 5))

  ;; BBDB: Address list
  (add-to-list 'load-path "~/.emacs.d/contacts-bbdb/")
  (require 'bbdb)
  (bbdb-initialize 'message 'gnus 'sendmail)
  (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
  (setq bbdb/mail-auto-create-p t
        bbdb/news-auto-create-p t)

  ;; Fetch only part of the article if we can.
  ;; I saw this in someone's .gnus
  (setq gnus-read-active-file 'some)

  ;; open attachment
  (eval-after-load 'mailcap
    '(progn
       (cond
        ;; on macOS, maybe change mailcap-mime-data?
        ((eq system-type 'darwin))
        ;; on Windows, maybe change mailcap-mime-data?
        ((eq system-type 'windows-nt))
        (t
         ;; Linux, read ~/.mailcap
         (mailcap-parse-mailcaps)))))

  ;; Tree view for groups.
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

  (setq gnus-use-cache t)
  (setq gnus-use-full-window nil)

  (setq gnus-asynchronous t)
  (setq gnus-use-article-prefetch 15)

  ;; http://www.gnu.org/software/emacs/manual/html_node/gnus/_005b9_002e2_005d.html
  (setq gnus-use-correct-string-widths nil)

  ;; Threads!  I hate reading un-threaded email -- especially mailing
  ;; lists.  This helps a ton!
  (setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)

  ;; Also, I prefer to see only the top level message.  If a message has
  ;; several replies or is part of a thread, only show the first message.
  ;; `gnus-thread-ignore-subject' will ignore the subject and
  ;; look at 'In-Reply-To:' and 'References:' headers.
  (setq gnus-thread-hide-subtree t)
  (setq gnus-thread-ignore-subject t)

  ;; Read HTML mail:
  ;; You need install the command line web browser 'w3m' and Emacs plugin 'w3m'
  ;; manually. It specify the html render as w3m so my setup works on all versions
  ;; of Emacs.
  ;;
  ;; Since Emacs 24+, a default html rendering engine `shr' is provided:
  ;;   - It works out of box without any cli program dependency or setup
  ;;   - It can render html color
  ;; So below line is optional.
  (setq mm-text-html-renderer 'shr))

(defun shell/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun shell/async-command-no-output (command)
  (call-process-shell-command (concat command " &") nil 0))

(defun browse-url-qutebrowser (url &optional _new-window)
  "Ask the Qutebrowser WWW browser to load URL.
Default to the URL around or before point.
The optional argument NEW-WINDOW is not used."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (shell/async-command-no-output (concat "qutebrowser " url)))
;; (setq browse-url-browser-function 'browse-url-qutebrowser)

(autoload 'exwm-enable "~/.emacs.d/desktop.el")

(let ((local-settings "~/.emacs.d/local.el"))
 (when (file-exists-p local-settings)
   (load-file local-settings)))

;; easy window resize
(global-set-key (kbd "C-s-h") #'windsize-left)
(global-set-key (kbd "C-s-l") #'windsize-right)
(global-set-key (kbd "C-s-j") #'windsize-down)
(global-set-key (kbd "C-s-k") #'windsize-up)

(global-set-key (kbd "C-s-<left>") #'windsize-left)
(global-set-key (kbd "C-s-<down>") #'windsize-down)
(global-set-key (kbd "C-s-<up>") #'windsize-up)
(global-set-key (kbd "C-s-<right>") #'windsize-right)

(global-set-key (kbd "s-b") #'counsel-switch-buffer)
(global-set-key (kbd "s-B") #'ibuffer)

(global-set-key (kbd "s-p") #'treemacs)

(global-set-key (kbd "s-X") #'kill-current-buffer)
(global-set-key (kbd "s-Q") #'(lambda () (interactive) (kill-current-buffer) (delete-window)))

(global-set-key (kbd "s-x") #'counsel-M-x)
(global-set-key (kbd "s-.") #'counsel-find-file)
(global-set-key (kbd "C-H-s-s") #'counsel-projectile-ag)

(global-set-key (kbd "C-H-s-h") #'windsize-left)
(global-set-key (kbd "C-H-s-l") #'windsize-right)
(global-set-key (kbd "C-H-s-j") #'windsize-down)
(global-set-key (kbd "C-H-s-k") #'windsize-up)

(global-set-key (kbd "C-H-s-<left>") #'windsize-left)
(global-set-key (kbd "C-H-s-<down>") #'windsize-down)
(global-set-key (kbd "C-H-s-<up>") #'windsize-up)
(global-set-key (kbd "C-H-s-<right>") #'windsize-right)

(global-set-key (kbd "H-s-b") #'counsel-switch-buffer)
(global-set-key (kbd "H-s-B") #'ibuffer)

(global-set-key (kbd "H-s-p") #'treemacs)

(global-set-key (kbd "H-s-e") #'ranger)
(global-set-key (kbd "H-s-E") #'deer)

(global-set-key (kbd "H-s-X") #'kill-current-buffer)
(global-set-key (kbd "H-s-Q") #'(lambda () (interactive) (kill-current-buffer) (delete-window)))

(global-set-key (kbd "H-s-x") #'counsel-M-x)
(global-set-key (kbd "H-s-.") #'counsel-find-file)

)
(setq gc-cons-threshold (* 2 1000 1000))
(provide 'init)
