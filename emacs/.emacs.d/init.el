(setq package-enable-at-startup nil)
(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
 			 :ref nil
 			 :files (:defaults "elpaca-test.el" (:exclude "extensions"))
 			 :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
   (build (expand-file-name "elpaca/" elpaca-builds-directory))
   (order (cdr elpaca-order))
   (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
    (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
 	    ((zerop (call-process "git" nil buffer t "clone"
 				  (plist-get order :repo) repo)))
 	    ((zerop (call-process "git" nil buffer t "checkout"
 				  (or (plist-get order :ref) "--"))))
 	    (emacs (concat invocation-directory invocation-name))
 	    ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
 				  "--eval" "(byte-recompile-directory \".\" 0 'force)")))
 	    ((require 'elpaca))
 	    ((elpaca-generate-autoloads "elpaca" repo)))
        (progn (message "%s" (buffer-string)) (kill-buffer buffer))
      (error "%s" (with-current-buffer buffer (buffer-string))))
  ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
  (defun elpaca-log-defaults ())

;; Install use-package support
(elpaca elpaca-use-package
        ;; Enable :elpaca use-package keyword.
        (elpaca-use-package-mode)
        ;; Assume :elpaca t unless otherwise specified.
        (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;; (setq native-comp-deferred-compilation-deny-list nil)

(add-hook 'elpaca-after-init-hook
          #'(lambda ()
              (setq gc-cons-threshold (* 100 1000 1000))))
(add-hook 'focus-out-hook 'garbage-collect)
(run-with-idle-timer 5 t 'garbage-collect)

(setq redisplay-dont-pause t
      jit-lock-stealth-time 1.25
      jit-lock-stealth-nice 0.5
      jit-lock-defer-time 0.3
      jit-lock-chunk-size 4096)

(setq large-file-warning-threshold 100000000)

(setq read-process-output-max (* 5 (* 1024 1024)))
(setq process-adaptive-read-buffering nil)

(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-use-momentum t)

(define-key minibuffer-local-completion-map " " nil)
(define-key minibuffer-local-must-match-map " " nil)
(define-key minibuffer-local-completion-map "?" nil)
(define-key minibuffer-local-must-match-map "?" nil)

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))
(setq create-lockfiles nil)
(setq projectile-known-projects-file (expand-file-name "tmp/projectile-bookmarks.eld" user-emacs-directory)
      lsp-session-file (expand-file-name "tmp/.lsp-session-v1" user-emacs-directory))

(use-package no-littering)

(global-auto-revert-mode 1)
(require 'bind-key)
(bind-key* "C-x k" #'kill-current-buffer)
(bind-key* "C-x K" #'kill-buffer)
(global-set-key (kbd "C-z") 'delete-frame)
(delete-selection-mode 1)
(set-default 'truncate-lines t)

(defun next-code-buffer ()
  (interactive)
  (let (( bread-crumb (buffer-name) ))
    (next-buffer)
    (while
        (and
         (string-match-p "^\*" (buffer-name))
         (not ( equal bread-crumb (buffer-name) )) )
      (next-buffer))))

(defun previous-code-buffer ()
  (interactive)
  (let (( bread-crumb (buffer-name) ))
    (previous-buffer)
    (while
        (and
         (string-match-p "^\*" (buffer-name))
         (not ( equal bread-crumb (buffer-name) )) )
      (previous-buffer))))

(global-set-key (kbd "C-M-o") 'next-code-buffer)
(global-set-key (kbd "C-M-O") 'previous-code-buffer)

(defun reverse-other-window ()
  (interactive)
  (other-window -1))

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'reverse-other-window)

(setq bookmark-save-flag 1)

(setq tab-always-indent t
      indent-tabs-mode nil
      indent-line-function 'insert-tab)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
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

(setq
 display-line-numbers-type 'absolute
 mode-line-percent-position nil)
(global-display-line-numbers-mode 1)
(line-number-mode 0)
(column-number-mode 0)
(global-hl-line-mode 1)

(setq warning-minimum-level :error)

(repeat-mode 1)

(require 'iso-transl)

(scroll-bar-mode 1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(setq window-divider-default-right-width 22
      window-divider-default-bottom-width 22)

(window-divider-mode 1)
(defun theme/minibuffer-echo-area ())

(defun window/set-header-gap (window)
  (with-selected-window window
    (if (window-in-direction 'above)
        (set-window-parameter window 'tab-line-format "")
      (set-window-parameter window 'tab-line-format 'none))))

(defun window/set-current-header-gap ()
  (interactive)
  (window/set-header-gap (selected-window)))

(defun window/set-all-header-gaps ()
  (interactive)
  (theme/minibuffer-echo-area)
  (dolist (frame (frame-list))
    (with-selected-frame frame
      (dolist (window (window-list))
        (window/set-header-gap window)))))


(add-hook 'window-configuration-change-hook #'window/set-all-header-gaps)

(setq-default fill-column 100)

(set-face-attribute 'default nil :font "SauceCodePro NF-11")

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "SauceCodePro NF-11")

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell-11" :weight 'regular)

(defun disable-mixed-pitch ()
  (interactive)
  (mixed-pitch-mode -1))

(use-package mixed-pitch
  :hook
  (org-mode . mixed-pitch-mode))

(load-file "~/.emacs.d/custom_packages/dracula-theme.el")
(load-theme 'dracula t)

(fringe-mode '(24 . 8))

(defun theme/minibuffer-echo-area ()
  (interactive)
  (dolist (buf '( " *Minibuf-1*"))
    (with-current-buffer (get-buffer-create buf)
      (face-remap-add-relative 'default :background "#44475a")
      (face-remap-add-relative 'fringe :background "#44475a")))
  (dolist (buf '(" *Minibuf-0*" " *Echo Area 0*" " *Echo Area 1*"))
    (with-current-buffer (get-buffer-create buf)
      (when (= (buffer-size) 0)
        (insert " "))
      ;; Don't allow users to kill these buffers, as it destroys the hack
      (add-hook 'kill-buffer-query-functions #'ignore nil 'local)
      (set-window-scroll-bars (minibuffer-window) nil nil)
      (face-remap-add-relative 'default :background "#282a36")
      (face-remap-add-relative 'fringe :background "#282a36"))))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package simple-modeline
  :hook (after-init . simple-modeline-mode))

(setq tab-always-indent t
      completions-format 'one-column
      completions-header-format nil
      completion-show-help t
      completion-show-inline-help t
      completions-max-height nil
      completion-auto-select nil)

(setq-default isearch-lazy-count t
              isearch-allow-motion t)

(use-package vertico
  :config
  (load-file "~/.emacs.d/elpaca/repos/vertico/extensions/vertico-multiform.el")
  (load-file "~/.emacs.d/elpaca/repos/vertico/extensions/vertico-flat.el")
  (setq vertico-cycle t
        vertico-flat-format '(:multiple
                              #("| %s" 0 1
                                (face minibuffer-prompt)
                                3 4
                                (face minibuffer-prompt))
                              :single
                              #("| %s" 0 1
                                (face minibuffer-prompt)
                                1 3
                                (face success)
                                3 4
                                (face minibuffer-prompt))
                              :prompt
                              #("| %s" 0 1
                                (face minibuffer-prompt)
                                3 4
                                (face minibuffer-prompt))
                              :separator
                              #("    " 0 3
                                (face minibuffer-prompt))
                              :ellipsis
                              #("…" 0 1
                                (face minibuffer-prompt))
                              :no-match "| No match"))
  (vertico-mode 1))

(use-package company
  :hook (emacs-lisp-mode . (lambda () (setq-local company-backends '(company-elisp))))
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  (:map company-active-map
        ("<return>" . nil)
        ("RET" . nil)
        ("M-<return>" . company-complete-selection))
  :config
  (setq company-require-match nil
        company-minimum-prefix-length 1
        company-idle-delay 0.3
        company-selection-wrap-around t
        company-tooltip-limit 10
        company-backends '((company-files :separate company-yasnippet :separate company-capf)))
  (global-company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq
   company-box-scrollbar nil
   company-box-doc-enable nil))

(use-package embark
  :bind (
         :map minibuffer-local-map
         ("C-c e" . embark-act)))

(use-package consult
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
         ("C-c b" . consult-bookmark)
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
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref
        consult-buffer-sources '(consult--source-hidden-buffer consult--source-modified-buffer consult--source-buffer consult--source-recent-file consult--source-file-register consult--source-project-buffer-hidden consult--source-project-recent-file-hidden))

  (advice-add #'register-preview :override #'consult-register-window)
  :config
  (consult-customize
   consult-theme
   :preview-key "M-."
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-buffer
   :preview-key "M-."
   consult--source-project-recent-file
   :preview-key "M-.")
  (setq consult-narrow-key "<"))

(use-package embark-consult)

(use-package orderless
  :init
  (setq completion-styles '(orderless)
  completion-category-defaults nil
  completion-category-overrides '((file (styles partial-completion)))))

(use-package org
  :config
  (setq
   org-confirm-babel-evaluate nil
   org-image-actual-width 1400
   org-startup-with-inline-images t)

  (load-file "~/.emacs.d/custom_packages/org-flyimage.el")
  (with-eval-after-load "org"
    (require 'org-flyimage)
    (add-hook 'org-mode-hook 'org-flyimage-mode))

  (defun org/org-babel-tangle-config ()
    (when (or (string-equal (buffer-file-name)
                            (expand-file-name "~/.dotfiles/README.org"))
              (string-equal (buffer-file-name)
                            (expand-file-name "~/.dotfiles/hyprland/README.org"))
              (string-equal (buffer-file-name)
                            (expand-file-name "~/.dotfiles/waybar/README.org"))
              (string-equal (buffer-file-name)
                            (expand-file-name "~/.dotfiles/emacs/README.org"))
              (string-equal (buffer-file-name)
                            (expand-file-name "~/.dotfiles/emacs/desktop.org"))
              (string-equal (buffer-file-name)
                            (expand-file-name "~/.dotfiles/emacs/local.org")))
      ;; Dynamic scoping to the rescue
      (let ((org-confirm-babel-evaluate nil))
        (org-babel-tangle))))
  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'org/org-babel-tangle-config)))
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 2.5))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.8))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.4))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.2))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0))))))

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
   org-pretty-entities nil
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
  (add-hook 'orgtbl-mode #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

(use-package time
  :elpaca nil
  :commands world-clock
  :config
  (setq display-time-interval 60)
  (setq display-time-mail-directory nil)
  (setq display-time-default-load-average nil))

(elpaca-wait)

(autoload 'exwm-enable "~/.emacs.d/desktop.el")

(use-package avy
  :config
  (require 'bind-key)
  (bind-key "M-j" #'avy-goto-char-timer))

(elpaca (macrursors :host github :repo "corytertel/macrursors"))

(eval-after-load "macrursors"
  '(progn (dolist (mode '(company-mode))
            (add-hook 'macrursors-pre-finish-hook mode)
            (add-hook 'macrursors-post-finish-hook mode))
          (define-prefix-command 'macrursors-mark-map)
          (global-set-key (kbd "C-c SPC") #'macrursors-select)
          (global-set-key (kbd "C->") #'macrursors-mark-next-instance-of)
          (global-set-key (kbd "C-<") #'macrursors-mark-previous-instance-of)
          (global-set-key (kbd "C-;") 'macrursors-mark-map)
          (define-key macrursors-mark-map (kbd "C-;") #'macrursors-mark-all-instances-of)
          (define-key macrursors-mark-map (kbd ";") #'macrursors-mark-all-instances-of)
          (define-key macrursors-mark-map (kbd "i") #'macrursors-mark-all-lines-or-instances)
          (define-key macrursors-mark-map (kbd "l") #'macrursors-mark-all-lists)
          (define-key macrursors-mark-map (kbd "s") #'macrursors-mark-all-symbols)
          (define-key macrursors-mark-map (kbd "e") #'macrursors-mark-all-sexps)
          (define-key macrursors-mark-map (kbd "f") #'macrursors-mark-all-defuns)
          (define-key macrursors-mark-map (kbd "n") #'macrursors-mark-all-numbers)
          (define-key macrursors-mark-map (kbd ".") #'macrursors-mark-all-sentences)
          (define-key macrursors-mark-map (kbd "r") #'macrursors-mark-all-lines)))

(use-package kmacro-x
  :init (kmacro-x-atomic-undo-mode 1))

(use-package easy-kill
  :config
  (global-set-key (kbd "C-=") 'easy-mark))

(use-package goto-last-change
  :config
  (global-set-key (kbd "C-z") 'goto-last-change))

(use-package vundo
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  (global-unset-key (kbd "C-?"))
  (global-set-key (kbd "C-?") 'vundo))

(use-package frames-only-mode
  :config
  (frames-only-mode))

(use-package hideshow
  :elpaca nil
  :hook
  (prog-mode . hs-minor-mode)
  :bind (
         :map prog-mode-map
         ("C-<tab>" . hs-cycle)
         ("C-<iso-lefttab>" . hs-global-cycle))
  :config
  (defun hs-cycle (&optional level)
    (interactive "p")
    (let (message-log-max
          (inhibit-message t))
      (if (= level 1)
          (pcase last-command
            ('hs-cycle
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))
            ('hs-cycle-children
             ;; TODO: Fix this case. `hs-show-block' needs to be
             ;; called twice to open all folds of the parent
             ;; block.
             (save-excursion (hs-show-block))
             (hs-show-block)
             (setq this-command 'hs-cycle-subtree))
            ('hs-cycle-subtree
             (hs-hide-block))
            (_
             (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))))
        (hs-hide-level level)
        (setq this-command 'hs-hide-level))))

  (defun hs-global-cycle ()
    (interactive)
    (pcase last-command
      ('hs-global-cycle
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all)))))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
  :after all-the-icons)

(add-hook 'prog-mode-hook #'subword-mode)
(defun custom/coding-faces ()
  (interactive)
  (set-face-attribute 'font-lock-keyword-face nil :weight 'ultra-bold)
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic :weight 'normal)
  (set-face-attribute 'font-lock-function-name-face nil :slant 'italic :weight 'semi-bold)
  (set-face-attribute 'font-lock-string-face nil :weight 'normal :slant 'italic))

(add-hook 'prog-mode-hook #'custom/coding-faces)

(use-package ediff
    :elpaca nil
    :config
    (setq ediff-window-setup-function 'ediff-setup-windows-plain
          ediff-split-window-function 'split-window-horizontally))

(use-package string-inflection
  :config
  (global-set-key (kbd "C-c C-u C-u") 'string-inflection-upcase)
  (global-set-key (kbd "C-c C-u C-k") 'string-inflection-kebab-case)

  (global-set-key (kbd "C-c C-u C-c") 'string-inflection-lower-camelcase)
  (global-set-key (kbd "C-c C-u C-S-c") 'string-inflection-camelcase)

  (global-set-key (kbd "C-c C-u C--") 'string-inflection-underscore)
  (global-set-key (kbd "C-c C-u C-_") 'string-inflection-capital-underscore))

(use-package sudo-edit)

(use-package which-key
  :config
  (setq which-key-min-display-lines 25)
  (which-key-mode 1))

(use-package whole-line-or-region
  :config
  (whole-line-or-region-global-mode 1))

(use-package ibuffer-vc
  :config
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 80 80 :left :elide) ; change: 30s were originally 18s
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename)))

  (defun ibuffer/apply-filter-groups ()
    "Combine my saved ibuffer filter groups with those generated
   by `ibuffer-vc-generate-filter-groups-by-vc-root'"
    (interactive)
    (setq ibuffer-filter-groups
          (append
           (ibuffer-vc-generate-filter-groups-by-vc-root)
           ibuffer-saved-filter-groups))

    (let ((ibuf (get-buffer "*Ibuffer*")))
      (when ibuf
        (with-current-buffer ibuf
          (pop-to-buffer ibuf)
          (ibuffer-update nil t)))))

  (add-hook 'ibuffer-hook 'ibuffer/apply-filter-groups)
  (add-hook 'ibuffer-hook 'ibuffer-auto-mode))
(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package blist
  :config
  (setq blist-filter-groups
        (list
         (cons "Chrome" #'blist-chrome-p)
         (cons "Eshell" #'blist-eshell-p)
         (cons "PDF" #'blist-pdf-p)
         (cons "Info" #'blist-info-p)
         (cons "Default" #'blist-default-p)))

  (blist-define-criterion "pdf" "PDF"
                          (eq (bookmark-get-handler bookmark)
                              #'pdf-view-bookmark-jump))

  (blist-define-criterion "info" "Info"
                          (eq (bookmark-get-handler bookmark)
                              #'Info-bookmark-jump))

  (blist-define-criterion "elisp" "ELisp"
                          (string-match-p
                           "\\.el$"
                           (bookmark-get-filename bookmark)))

  (blist-define-criterion "chrome" "Chrome"
                          (eq (bookmark-get-handler bookmark)
                              #'bookmark/chrome-bookmark-handler)))

(use-package wgrep)

(use-package savehist
  :elpaca nil
  :init
  (savehist-mode))

(use-package helpful
  :config
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-funtion #'helpful-variable)
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function)
  (global-set-key (kbd "C-h C") #'helpful-command))

(load-file "~/.emacs.d/custom_packages/siege-mode.el")
(global-set-key (kbd "M-[") #'siege-explicit-call)
(global-set-key (kbd "M-]") #'siege-explicit-call)

(elpaca (explain-pause-mode :host github :repo "lastquestion/explain-pause-mode"))

(use-package free-keys)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package flycheck
  :init (global-flycheck-mode))

(setq electric-pair-pairs
  '(
    (?\' . ?\')
    (?\" . ?\")
    (?\[ . ?\])
    (?\{ . ?\})))

(defun electric-pair/activate ()
  (interactive)
  (electric-pair-mode 1))

(defun electric-pair/deactivate ()
  (interactive)
  (electric-pair-mode -1))

(add-hook 'activate-mark-hook #'electric-pair/activate)
(add-hook 'deactivate-mark-hook #'electric-pair/deactivate)

(electric-indent-mode 1)

(use-package magit
  :config
  (defun magit/magit-status-no-split ()
    "Don't split window."
    (interactive)
    (let ((magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))
      (magit-status)))
  (global-unset-key (kbd "C-x g"))
  (global-set-key (kbd "C-x g g") #'magit-status)
  (global-set-key (kbd "C-x g c") #'magit-clone)
  (global-set-key (kbd "C-x g s") #'magit/magit-status-no-split))

(use-package forge)

(use-package yasnippet
  :config
  (yas-reload-all)
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package insert-shebang)

(defun eshell/jwt-decode (jwt)
  (interactive "sJWT: ")
  (shell-command-to-string (concat "PATH=~/.npm-packages/bin:$PATH NODE_PATH=~/.npm-packages/lib/node_modules node -e \"const jwt = require('jsonwebtoken'); console.log(jwt.decode('" jwt "', { complete: true }))\"")))

(use-package nodejs-repl
  :config
  (defun nodejs-repl/remove-broken-filter ()
    (remove-hook 'comint-output-filter-functions 'nodejs-repl--delete-prompt t))
  (add-hook 'nodejs-repl-mode-hook #'nodejs-repl/remove-broken-filter))

(use-package typescript-mode
  :mode "\\.ts\\'")

(use-package jest-test-mode 
  :commands jest-test-mode
  :hook (typescript-mode typescript-ts-mode js-mode js-ts-mode typescript-tsx-mode))

(use-package apheleia
  :config
  (setf (alist-get 'prettier apheleia-formatters)
        '(npx "eslint" "--fix" file))
  (add-to-list 'apheleia-mode-alist '(js-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(js-ts-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(typescript-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(typescript-ts-mode . prettier))
  (apheleia-global-mode t))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package lsp-mode
  :defer t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-clients-typescript-server-args '("--stdio"))
  :bind (
         :map lsp-mode-map
         ("C-h ." . lsp-describe-thing-at-point)
         ("C-." . lsp-execute-code-action)
         ("M-." . lsp-find-definition))
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (js-mode . (lambda () 
                      (lsp)))
         (js-ts-mode . (lambda () 
                      (lsp)))
         (typescript-ts-mode . (lambda () 
                                 (lsp)))
         (typescript-mode . (lambda () 
                                 (lsp)))
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (with-eval-after-load 'js
    (define-key js-mode-map (kbd "M-.") nil)
    )
  (setq
   lsp-log-io nil
   lsp-enable-symbol-highlighting nil
   lsp-eldoc-render-all t
   lsp-auto-guess-root t
   lsp-log-io nil
   lsp-restart 'auto-restart
   lsp-enable-on-type-formatting nil
   lsp-eslint-auto-fix-on-save nil
   lsp-signature-auto-activate t
   lsp-signature-render-documentation t
   lsp-signature-function 'lsp-lv-message
   lsp-headerline-breadcrumb-enable nil
   lsp-semantic-tokens-enable nil
   lsp-enable-folding nil
   lsp-enable-snippet nil
   lsp-modeline-code-actions-enable nil
   lsp-idle-delay 0.5
   lsp-completion-provider :none
   lsp-enable-file-watchers nil))

(use-package dap-mode
  :config
  (require 'dap-node)
  (dap-node-setup))

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

(if (eq (shell-command "kubectl --help") 0 )
    (use-package kubel))

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

(use-package csv-mode
  :config
  (add-hook 'csv-mode-hook 'csv-guess-set-separator)
  (setq csv-separators '("," ";" ":")))

(use-package devdocs
  :config
  (global-set-key (kbd "C-h D") #'devdocs-lookup)
  (add-hook 'js-mode-hook
        (lambda () (setq-local devdocs-current-docs '("node~16_lts" "jsdoc" "javascript")))))

(use-package ejc-sql
  :config
  (setq ejc-result-table-impl 'orgtbl-mode)

  (add-hook 'ejc-sql-connected-hook
        (lambda ()
          (ejc-set-fetch-size 200)
          (ejc-set-max-rows 200)
          (ejc-set-show-too-many-rows-message nil)
          (ejc-set-column-width-limit nil)
          (ejc-set-use-unicode nil)))

  (add-hook 'sql-mode-hook
            (lambda ()
              (ejc-sql-mode t)))

  (add-hook 'ejc-sql-minor-mode-hook
            (lambda ()
              (ejc-eldoc-setup)))

  (require 'ejc-company)
  (push 'ejc-company-backend company-backends)
  
  (add-hook 'ejc-sql-minor-mode-hook
            (lambda ()
              (company-mode t))))

(custom-set-faces
 `(ansi-color-black ((t (:foreground "#282a36"))))
 `(ansi-color-red ((t (:foreground "#ff5555"))))
 `(ansi-color-green ((t (:foreground "#50fa7b"))))
 `(ansi-color-yellow ((t (:foreground "#f1fa8c"))))
 `(ansi-color-blue ((t (:foreground "#bd93f9"))))
 `(ansi-color-magenta ((t (:foreground "#ff79c6"))))
 `(ansi-color-cyan ((t (:foreground "#8be9fd"))))
 `(ansi-color-gray ((t (:foreground "#f8f8f2")))))

(setq eshell-banner-message "")

(defun eshell/hook ()
  (require 'eshell)
  (require 'em-smart)
  (define-key eshell-mode-map (kbd "M-m") #'eshell-bol)
  (define-key eshell-hist-mode-map (kbd "M-s") nil)
  (define-key eshell-hist-mode-map (kbd "M-r") #'consult-history)
  (setq 
   eshell-where-to-jump 'begin
   eshell-review-quick-commands nil
   eshell-smart-space-goes-to-end t
   eshell-prompt-function
   (lambda ()
     (concat (format-time-string " %Y-%m-%d %H:%M" (current-time))
             (if (= (user-uid) 0) " # " " $ ")))
   eshell-highlight-prompt t)
  (set-face-attribute 'eshell-prompt nil :weight 'ultra-bold :inherit 'minibuffer-prompt))
(add-hook 'eshell-mode-hook #'eshell/hook)

(defun eshell/rename-with-current-path ()
  (interactive)
  (rename-buffer (concat "Eshell: " (replace-regexp-in-string "^[Directory ]*" "" (pwd))) t))
(add-hook 'eshell-directory-change-hook #'eshell/rename-with-current-path)
(add-hook 'eshell-mode-hook #'eshell/rename-with-current-path)

(defun eshell/get-relevant-buffer ()
  (if (derived-mode-p 'dired-mode)
      (get-buffer (replace-regexp-in-string "^[Directory ]*" "" (pwd)))
    (car (seq-filter (lambda (buf)
                       (string-prefix-p (concat "Eshell: " (replace-regexp-in-string "/$" "" (consult--project-root)))
                                        (buffer-name buf)))
                     (buffer-list)))))

(defun eshell/new-or-current ()
  "Open a new instance of eshell."
  (interactive)
  (let ((eshell-buffer (eshell/get-relevant-buffer))
        (default-directory (if (derived-mode-p 'dired-mode)
                               (replace-regexp-in-string "^[Directory ]*" "" (pwd))
                             (consult--project-root))))
    
    (pop-to-buffer (current-buffer) 'display-buffer-pop-up-frame)
    (if eshell-buffer
       (switch-to-buffer eshell-buffer)
      (eshell 'N))))

(global-set-key (kbd "C-c t") #'eshell/new-or-current)

(use-package eshell
  :elpaca nil
  :bind (:map eshell-mode-map
              ("<tab>" . company-complete)))

(use-package eat
  :config
  (setq
   eat-term-terminfo-directory (concat elpaca-repos-directory "emacs-eat/terminfo")
   )
  (setq eshell-visual-commands '())

  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))

(defun eshell/emacs (file)
  (find-file file))

(use-package multi-term
  :bind (
         :map term-mode-map
         ("s-<escape>" . term-char-mode))
  :config
  (defun term-send-tab ()
    (interactive)
    (term-send-raw-string "\t"))

  (setq multi-term-program "bash")

  (add-to-list 'term-bind-key-alist '("<backtab>" . term-send-up))
  (add-to-list 'term-bind-key-alist '("TAB" . term-send-tab))
  (add-to-list 'term-bind-key-alist '("s-<escape>" . term-line-mode)))

(defun dired/open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "xdg-open" nil 0 nil file)
    (message "Opening %s done" file)))

(defun dired/open-home-dir ()
  "Open the home directory in dired"
  (interactive)
  (dired "~"))

(defun dired/first-file ()
  (interactive)
  (beginning-of-buffer)
  (while (and (not (eobp))
              (or (bolp)
                  (member (dired-get-filename 'no-dir t)
                          '("." ".."))))
    (dired-next-line 1)))

(defun dired/last-file ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(use-package dired
  :elpaca nil
  :bind (
         :map dired-mode-map
         ("C-." . dired-hide-dotfiles-mode)
         ("<C-return>" . dired/open-file)
         ("M-p" . dired-up-directory)
         ("M-n" . dired-find-file)
         ("s-<escape>" . dired-toggle-read-only)
         ("M-<" . dired/first-file)
         ("M->" . dired/last-file)
         ("~" . dired/open-home-dir))
  :hook
  (dired-mode . dired-hide-details-mode)
  :config
  (setq ls-lisp-use-insert-directory-program nil)
  (require 'ls-lisp)
  (setq ls-lisp-dirs-first t)
  (setq wdired-allow-to-change-permissions t)
  (add-hook 'wdired-mode-hook
    (lambda ()
      (define-key wdired-mode-map (kbd "s-<escape>") 'wdired-abort-changes))))

(use-package dired-subtree
  :bind (
         :map dired-mode-map
         ("C-<tab>" . dired-subtree-cycle)
         ("<tab>" . dired-subtree-toggle)
         ("<backtab>" . dired-subtree-remove)))

(use-package dired-hide-dotfiles
  :hook
  (dired-mode . dired-hide-dotfiles-mode))

(use-package shr
  :elpaca nil
  :config
  (setq shr-use-fonts nil)
  (setq shr-use-colors nil)
  (setq shr-max-image-proportion 1)
  (setq shr-width nil)
  (setq shr-folding-mode t))

(defvar eww/input-history nil)
(eval-after-load "savehist"
  '(add-to-list 'savehist-additional-variables 'eww/input-history))

(defun eww/do-start-with-url-or-search ()
  (interactive)
  (if (derived-mode-p 'eww-mode)
      (eww (completing-read "Eww URL or search " eww/input-history nil nil (eww-current-url) 'eww/input-history))
    (eww (completing-read "Eww URL or search " eww/input-history nil nil nil 'eww/input-history))))

(with-eval-after-load 'eww
  (defun eww/rename-buffer ()
    "Rename `eww-mode' buffer so sites open in new page.
  URL `http://xahlee.info/emacs/emacs/emacs_eww_web_browser.html'
  Version 2017-11-10"
    (interactive)
    (let (($title (plist-get eww-data :title)))
      (when (eq major-mode 'eww-mode )
        (if $title
            (rename-buffer (concat "Eww: " $title) t)
          (rename-buffer "Eww" t)))))

  ;;(add-hook 'eww-after-render-hook #'mixed-pitch-mode)
  (add-hook 'eww-after-render-hook 'eww/rename-buffer))

(add-hook 'elpaca-after-init-hook
      #'(lambda ()
  	(let ((local-settings "~/.emacs.d/local.el"))
  	  (when (file-exists-p local-settings)
  	    (load-file local-settings)))
  	(eval-after-load "frames-only-mode"
  	  (frames-only-mode 1))))
