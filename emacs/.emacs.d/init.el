;; Avoid blinding white at startup
(load-theme 'wombat)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

(global-display-line-numbers-mode 1)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Replace yes no answers by y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Let emacs use system clipboard as a default behaviour
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(defun fonts/set-size (font-size)
  (set-face-attribute 'default nil :font "Fira Code" :height font-size)
  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil :font "Fira Code" :height font-size)
  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font "Cantarell" :height font-size :weight 'regular))

(defun fonts/default-size ()
  (interactive)
  (fonts/set-size 85))

(defun fonts/big-size ()
  (interactive)
  (fonts/set-size 140))

(fonts/default-size)

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
  (setq doom-themes-treemacs-theme "doom-colors"))

(use-package doom-modeline

  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 22)))

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
  (setq aw-background nil)
  (setq aw-dispatch-always t)
  (ace-window-display-mode t)
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

(use-package yascroll

  :config
  (global-yascroll-bar-mode 1)
  (setq yascroll:delay-to-hide nil)
  ;; Don't hide scrollbar when editing
  (defadvice yascroll:before-change (around always-show-bar activate) ()))

(use-package volatile-highlights)

(use-package highlight-parentheses
  :config
  (global-highlight-parentheses-mode 1))

(use-package focus)

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (keys/leader-keys
    "y" #'counsel-yank-pop))

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

(use-package orderless
  :config
  (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder))))

(use-package avy
  :config
  (keys/leader-keys
    "f" '(avy-goto-word-0 :which-key "Go to word")))

(use-package image-dired)

(use-package ranger)

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org

  :hook (org-mode . efs/org-mode-setup)
  :config
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

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

(use-package flycheck)

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (keys/leader-keys
    "gg" '(magit :which-key "magit status")))

;; (use-package forge)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package dap-mode)

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top))

(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

(use-package rainbow-mode
  :straight (rainbow-mode :type git :host github :repo "emacsmirror/rainbow-mode"))

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

(use-package yaml-mode
   :straight (yaml-mode :type git :host github :repo "yoshiki/yaml-mode")
   :config
   (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode))

(use-package json-mode
  :config
  (add-hook 'json-mode-hook 'highlight-indent-guides-mode))

(use-package restclient
  :straight (restclient :type git :host github :repo "pashky/restclient.el")
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
(defun docker/set-format ()
  (interactive)
  (setq tabulated-list-format [("Names" 30 t)("Ports" 30 t)("Status" 20 t)("Id" 16 t)("Image" 15 t)("Command" 30 t)("Created" 23 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key docker-container-default-sort-key)
  (add-hook 'tabulated-list-revert-hook 'docker-container-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(defun docker-container-parse (line)
  "Convert a LINE from \"docker container ls\" to a `tabulated-list-entries' entry."
  (condition-case nil
      (let* ((data (json-read-from-string line))
             (uptime (aref data 6))
             (status (aref data 2)))
        (aset data 6 (format-time-string "%F %T" (date-to-time uptime)))
        (aset data 2 (propertize status 'font-lock-face (docker-container-status-face status)))
        (list (aref data 1) data))
    (json-readtable-error
     (error "Could not read following string as json:\n%s" line))))

(defun docker-container-entries ()
  "Return the docker containers data for `tabulated-list-entries'."
  (let* ((fmt "[{{json .Names}},{{json .Ports}},{{json .Status}},{{json .ID}},{{json .Image}},{{json .Command}},{{json .CreatedAt}}]")
         (data (docker-run-docker "container ls" (docker-container-ls-arguments) (format "--format=\"%s\"" fmt)))
         (lines (s-split "\n" data t)))
    (-map #'docker-container-parse lines)))

(defun docker-container-refresh ()
  "Refresh the containers list."
  (setq tabulated-list-entries (docker-container-entries)))

(define-derived-mode docker-container-mode tabulated-list-mode "Containers Menu"
  "Major mode for handling a list of docker containers."
  (setq tabulated-list-format [("Names" 30 t)("Ports" 40 t)("Status" 20 t)("Id" 16 t)("Image" 15 t)("Command" 30 t)("Created" 23 t)])
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

(use-package kubernetes)

(use-package kubernetes-evil
  :after kubernetes)

(use-package vterm
  :config
  (setq vterm-shell "/bin/zsh")
  (setq vterm-buffer-name-string "vterm: %s"))

(use-package term
  :config
  (setq explicit-shell-file-name "zsh")

  ;; Use 'explicit-<shell>-args for shell-specific args
  ;;(setq explicit-zsh-args '())         

  (setq evil-move-cursor-back t)

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package windmove)

;; (use-package framemove
;;   :straight (framemove :type git :host github :repo "emacsmirror/framemove")
;;   :config
;;   (setq framemove-hook-into-windmove t))

(use-package windsize)

(use-package zoom
  :config
  (setq zoom-size '(0.618 . 0.618)))
