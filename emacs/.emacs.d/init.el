(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(let ((file-name-handler-alist nil))
  (require 'package)
  (setq package-enable-at-startup nil)

;; Avoid blinding white at startup
(load-theme 'wombat)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(setq-default window-divider-default-right-width 5)
(setq-default window-divider-default-bottom-width 5)
(setq-default window-divider-default-places t)
(add-hook 'after-init-hook #'window-divider-mode)


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
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Winner-mode
(winner-mode 1)

;; Smooth scrolling
(setq-default scroll-margin 0)
(setq-default scroll-preserve-screen-position t)
(setq-default scroll-conservatively 1)
; Horizontal Scroll
(setq-default hscroll-margin 0)

(setq package-native-compile t)

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

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (setq ibuffer-saved-filter-groups
             (list
              (append
               '("custom")
               (ibuffer-custom-filter-groups))))
               (ibuffer-switch-to-saved-filter-groups "custom")
            (ibuffer-auto-mode 1)))

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package quelpa)

(use-package general
  :config
  (general-create-definer keys/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "s-d")

  (keys/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package undo-tree
  :config
  (global-undo-tree-mode 1))

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
  (evil-set-undo-system 'undo-tree))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package treemacs-evil)

(use-package evil-multiedit
 :config
 (evil-multiedit-default-keybinds))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-goggles
  :config
  (evil-goggles-mode)
  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(keys/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer)

(use-package ibuffer-vc)

(use-package doom-themes
  :init (load-theme 'doom-dark+ t)
  :config
  (setq doom-themes-treemacs-theme "doom-colors")
  (set-face-attribute 'fringe nil :background "#1e1e1e")
  ;; Line number styling for mode change
  (add-hook 'evil-normal-state-entry-hook (lambda () (face-remap-add-relative 'line-number nil :foreground "#707070" :background "#1e1e1e")))
  (add-hook 'evil-normal-state-entry-hook (lambda () (face-remap-add-relative 'line-number-current-line nil :foreground "#ffffff" :background "#121212")))
  (add-hook 'evil-insert-state-entry-hook (lambda () (face-remap-add-relative 'line-number nil :foreground "#707070" :background "#1c3319")))
  (add-hook 'evil-insert-state-entry-hook (lambda () (face-remap-add-relative 'line-number-current-line nil :foreground "#ffffff" :background "#579c4c")))
  (add-hook 'evil-visual-state-entry-hook (lambda () (face-remap-add-relative 'line-number nil :foreground "#707070" :background "#00332a")))
  (add-hook 'evil-visual-state-entry-hook (lambda () (face-remap-add-relative 'line-number-current-line nil :foreground "#ffffff" :background "#009b80"))))

(use-package minions)

(defun simple-modeline-segment-minions ()
  "Displays the current major and minor modes with minions-mode in the mode-line."
  (concat " " (format-mode-line minions-mode-line-modes)))

(use-package simple-modeline
 :hook (after-init . simple-modeline-mode)
 :config
 (setq simple-modeline-segments '((simple-modeline-segment-modified simple-modeline-segment-buffer-name simple-modeline-segment-position) (simple-modeline-segment-input-method simple-modeline-segment-eol simple-modeline-segment-encoding simple-modeline-segment-vc simple-modeline-segment-misc-info simple-modeline-segment-process simple-modeline-segment-minions)))

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

(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (keys/leader-keys
    "o" #'ace-window
    "O" #'ace-swap-window)
  )

(use-package ace-jump-mode)

(defun efs/treemacs-set-fringe ()
  (setq left-fringe-width 0)
  (setq right-fringe-width 0))

(use-package treemacs
  :config
  (add-hook 'treemacs-mode-hook #'efs/treemacs-set-fringe))

(use-package treemacs-all-the-icons
  :config
  (treemacs-load-theme "all-the-icons"))

(efs/treemacs-set-fringe)

(use-package volatile-highlights)

(use-package highlight-parentheses
  :config
  (global-highlight-parentheses-mode 1))

(use-package focus)

(use-package flycheck)

(use-package minimap
  :config
  (setq minimap-window-location 'right))

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
  :ensure t
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

(use-package avy
  :config
  (keys/leader-keys
    "f" '(avy-goto-word-0 :which-key "Go to word")))

(use-package image-dired)

(use-package ranger)

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

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-babel-tangle-config ()
  (when (or (string-equal (buffer-file-name)
                          (expand-file-name "~/dotfiles/README.org"))
            (string-equal (buffer-file-name)
                        (expand-file-name "~/dotfiles/qutebrowser/README.org"))
            (string-equal (buffer-file-name)
                          (expand-file-name "~/dotfiles/emacs/README.org"))
            (string-equal (buffer-file-name)
                        (expand-file-name "~/dotfiles/emacs/desktop.org")))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

(setq org-confirm-babel-evaluate nil)

(use-package org-mime)

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
  :config (counsel-projectile-mode))

(use-package ibuffer-projectile)

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (keys/leader-keys
    "gg" '(magit :which-key "magit status")))

;; (use-package forge)

(use-package evil-nerd-commenter
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

(defun efs/lsp-mode-setup ()
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
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-position 'at-point))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

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

(quelpa
 '(yaml-mode
   :fetcher git
   :url "https://github.com/yoshiki/yaml-mode"))
(require 'yaml-mode)
(add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)

(use-package json-mode
  :config
  (add-hook 'json-mode-hook 'highlight-indent-guides-mode))

(use-package restclient
  :hook (restclient-mode . company-mode))

(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

(use-package company-restclient)

(add-to-list 'company-backends 'company-restclient)

(use-package adoc-mode
  :hook (adoc-mode . company-mode))

(add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))

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
  (setq kubernetes-poll-frequency 3600))
  ;; (setq kubernetes-redraw-frequency 3600))

(use-package kubernetes-evil
  :after kubernetes)

(use-package jenkins)

(use-package flyspell
  :ensure nil
  :diminish
  :if (executable-find "aspell")
  :hook (((prog-mode text-mode outline-mode latex-mode) . flyspell-mode))
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

(quelpa
  '(langtool
    :fetcher git
    :url "https://github.com/mhayashi1120/Emacs-langtool"))
(setq langtool-language-tool-server-jar "~/Tools/LanguageTool/languagetool-server.jar")
 (require 'langtool)

(use-package async)

(use-package trashed)

(use-package w3m)

(use-package bbdb)

(use-package dianyou)

(use-package vterm
  :config
  (setq vterm-shell "/bin/zsh")
  (setq vterm-buffer-name-string "vterm: %s"))

(use-package term
  :config
  (setq explicit-shell-file-name "bash")

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

(quelpa
 '(bitwarden
   :fetcher git
   :url "https://github.com/seanfarley/emacs-bitwarden.git"))
(require 'bitwarden)

(quelpa
 '(eaf
   :fetcher git
   :url "https://github.com/manateelazycat/emacs-application-framework.git"
   :files ("*")))

(use-package epc :defer t)
(use-package ctable :defer t)
(use-package deferred :defer t)
(use-package s :defer t)
(require 'eaf)
;; (require 'eaf-evil)

(add-to-list 'eaf-wm-focus-fix-wms "EXWM")
(eaf-setq eaf-browser-enable-adblocker "true")
(eaf-setq eaf-browser-scroll-behavior "smooth")
(eaf-setq eaf-browser-blank-page-url "https://duckduckgo.com")
(eaf-setq eaf-browser-dark-mode "false")

(setq eaf-browser-continue-where-left-off t)
(setq eaf-browser-search-engines '(("duckduckgo" . "https://duckduckgo.com/?q=%s")))
(setq eaf-browser-default-search-engine "duckduckgo")

(keys/leader-keys
  "i" '(:ignore t :which-key "internet")
  "ia" '(eaf-open-browser :which-key "address")
  "ii" '(eaf-open-browser-with-history :which-key "search & history")
  "ib" '(eaf-open-bookmark :which-key "bookmarks"))

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
(setq browse-url-browser-function 'browse-url-qutebrowser)

(autoload 'exwm-enable "~/.emacs.d/desktop.el")

(let ((local-settings "~/.emacs.d/local.el"))
 (when (file-exists-p local-settings)
   (load-file local-settings)))

)
(setq gc-cons-threshold (* 100 1024 1024))
(provide 'init)
