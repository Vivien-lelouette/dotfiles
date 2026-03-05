;;; init.el --- -*- lexical-binding: t; -*-
;; Ensure Emacs loads the most recent byte-compiled files.
(setq load-prefer-newer t)
;; Make Emacs Native-compile .elc files asynchronously by setting
;; `native-comp-jit-compilation' to t.
(setq native-comp-jit-compilation t)
(setq native-comp-deferred-compilation native-comp-jit-compilation)  ; Deprecated

(setq use-package-always-ensure t)
(setq package-install-upgrade-built-in t)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defconst *is-windows* (eq system-type 'windows-nt))
(defconst *is-linux* (eq system-type 'gnu/linux))

;; Ensure compilation of all elisp packages
(use-package compile-angel
  :ensure t
  :demand t
  :config
  ;; Set `compile-angel-verbose' to nil to disable compile-angel messages.
  ;; (When set to nil, compile-angel won't show which file is being compiled.)
  (setq compile-angel-verbose t)

  ;; Uncomment the line below to compile automatically when an Elisp file is saved
  (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)

  ;; The following directive prevents compile-angel from compiling your init
  ;; files. If you choose to remove this push to `compile-angel-excluded-files'
  ;; and compile your pre/post-init files, ensure you understand the
  ;; implications and thoroughly test your code. For example, if you're using
  ;; the `use-package' macro, you'll need to explicitly add:
  ;; (eval-when-compile (require 'use-package))
  ;; at the top of your init file.
  (push "/init.el" compile-angel-excluded-files)
  (push "/local.el" compile-angel-excluded-files)
  (push "/desktop.el" compile-angel-excluded-files)

  ;; A global mode that compiles .el files before they are loaded
  ;; using `load' or `require'.
  (compile-angel-on-load-mode 1))

;; Raise GC threshold during init for faster startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; After init, lower to a reasonable value for interactive use
(defun gc/set-runtime-threshold ()
  "Set GC threshold to a reasonable value after startup."
  (setq gc-cons-threshold (* 64 1024 1024)  ; 64MB
        gc-cons-percentage 0.1))
(add-hook 'emacs-startup-hook #'gc/set-runtime-threshold)

;; GC when Emacs loses focus (debounced, skip during completion)
(defun gc/collect-on-focus-out ()
  "Trigger GC when Emacs loses focus, unless minibuffer or company is active."
  (unless (or (active-minibuffer-window)
              (bound-and-true-p company-candidates))
    (garbage-collect)))
(add-hook 'focus-out-hook #'gc/collect-on-focus-out)

;; redisplay-dont-pause is obsolete since Emacs 24.5 - removed
;; Optimize jit-lock for better fontification performance
(setq jit-lock-stealth-time 1.5
      jit-lock-stealth-nice 0.5
      jit-lock-defer-time 0.25
      jit-lock-chunk-size 1500)

(setq large-file-warning-threshold 100000000)

(setq read-process-output-max (* 5 1024 1024))

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)

(use-package ultra-scroll
  :init
  (setq scroll-conservatively 3 ; or whatever value you prefer, since v0.4
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

(defalias 'yes-or-no-p 'y-or-n-p)
(setq xref-prompt-for-identifier nil
      comint-prompt-read-only t
      use-dialog-box nil)

(define-key minibuffer-local-completion-map " " nil)
(define-key minibuffer-local-must-match-map " " nil)
(define-key minibuffer-local-completion-map "?" nil)
(define-key minibuffer-local-must-match-map "?" nil)
(defun minibuffer/insert-newline ()
  "Insert a literal newline in the minibuffer."
  (interactive)
  (insert "\n"))
(define-key minibuffer-local-map (kbd "S-<return>") #'minibuffer/insert-newline)
(defun minibuffer/escape-spaces ()
  "Escape all spaces in minibuffer input with backslashes."
  (interactive)
  (save-excursion
    (goto-char (minibuffer-prompt-end))
    (while (search-forward " " nil t)
      (replace-match "\\\\ "))))
(define-key minibuffer-local-map (kbd "M-\\") #'minibuffer/escape-spaces)
(add-hook 'minibuffer-setup-hook #'subword-mode)

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))
(setq create-lockfiles nil)
(setq projectile-known-projects-file (expand-file-name "tmp/projectile-bookmarks.eld" user-emacs-directory)
      lsp-session-file (expand-file-name "tmp/.lsp-session-v1" user-emacs-directory))

(use-package no-littering)

(define-key key-translation-map (kbd "<up>") (kbd "C-p"))
(define-key key-translation-map (kbd "<down>") (kbd "C-n"))
(define-key key-translation-map (kbd "<left>") (kbd "C-b"))
(define-key key-translation-map (kbd "<right>") (kbd "C-f"))

(define-key key-translation-map [f1] (kbd "C-1"))
(define-key key-translation-map [f2] (kbd "C-2"))
(define-key key-translation-map [f3] (kbd "C-3"))
(define-key key-translation-map [f4] (kbd "C-4"))
(define-key key-translation-map [f5] (kbd "C-5"))
(define-key key-translation-map [f6] (kbd "C-6"))
(define-key key-translation-map [f7] (kbd "C-7"))
(define-key key-translation-map [f8] (kbd "C-8"))
(define-key key-translation-map [f9] (kbd "C-9"))
(define-key key-translation-map [f10] (kbd "C-0"))

;; Auto-revert with performance optimizations
(setq auto-revert-interval 1           ; Check every 1 second (not continuously)
      auto-revert-check-vc-info nil    ; Don't check VC info (expensive)
      auto-revert-verbose nil          ; Don't show messages
      auto-revert-use-notify t         ; Use file notifications when available
      auto-revert-avoid-polling t)     ; Prefer notifications over polling
(global-auto-revert-mode 1)
(require 'bind-key)
(bind-key* "C-x K" #'kill-buffer-and-window)
(bind-key* "C-x k" #'kill-current-buffer)
(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd (if *is-linux* "C-<iso-lefttab>" "C-S-<tab>")) 'previous-buffer)

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

;; (global-hl-line-mode 1)

(setq hl-line-overlay-priority -50)

(defvar frame-centric nil
  "When non-nil, window rules prefer opening buffers in new frames.
Set via --eval at daemon launch: emacs --daemon --eval '(setq frame-centric t)'")

(defun vv/display-buffer-pop-up-frame-maybe (buffer alist)
  "Display BUFFER in a new frame only when `frame-centric' is non-nil."
  (when frame-centric
    (display-buffer-pop-up-frame buffer alist)))

;; --- Docker ---
(with-eval-after-load 'docker
  (setq docker-pop-to-buffer-action '((vv/display-buffer-pop-up-frame-maybe))))

;; --- gptel ---
(with-eval-after-load 'gptel
  (setq gptel-display-buffer-action '((vv/display-buffer-pop-up-frame-maybe))))

;; --- Agent Shell ---
(with-eval-after-load 'agent-shell
  (setq agent-shell-display-action
        (if (featurep 'ewm)
            '((display-buffer-same-window))
          '((vv/display-buffer-pop-up-frame-maybe display-buffer-in-side-window)
            (side . left)
            (slot . 1)
            (window-width . 100)
            (preserve-size . (t . nil))))))

;; --- Claudemacs ---
(add-to-list 'display-buffer-alist
             '("^\\*claudemacs"
               (display-buffer-reuse-window vv/display-buffer-pop-up-frame-maybe)
               (reusable-frames . visible)))

;; --- Jira ---
(with-eval-after-load 'jira
  (defun jira/display-reuse-window (buffer alist)
    "Display BUFFER in the same window if it already shows a *Jira buffer."
    (when (string-prefix-p "*Jira" (buffer-name (window-buffer (selected-window))))
      (display-buffer-same-window buffer alist)))

  (defun jira/force-display-actions (orig-fn &rest args)
    "Advice to obey display-actions for Jira buffers."
    (let ((switch-to-buffer-obey-display-actions t))
      (apply orig-fn args)))
  (dolist (fn '(jira-issues jira-tempo))
    (advice-add fn :around #'jira/force-display-actions)))

(add-to-list 'display-buffer-alist
             '("\\*Jira"
               (jira/display-reuse-window
                vv/display-buffer-pop-up-frame-maybe)
               (reusable-frames . visible)))

;; --- Jest ---
(add-to-list 'display-buffer-alist
             '("\\*jest-test-compilation\\*"
               (display-buffer-reuse-window vv/display-buffer-pop-up-frame-maybe)
               (reusable-frames . visible)))

(setq bookmark-save-flag 1)

(setq indent-tabs-mode nil
      indent-line-function 'insert-tab)
(setq-default indent-tabs-mode nil)

(defun tab/default-width (width)
  (interactive "nTab default width: ")

  (setq-default tab-width width)
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
        standard-indent tab-width))

(defun tab/width (width)
  (interactive "nTab width: ")
  (setq-local tab-width width
              c-basic-offset tab-width
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
              standard-indent tab-width))

(tab/default-width 4)

(setq
 display-line-numbers-type 'absolute
 mode-line-percent-position nil)
(global-display-line-numbers-mode 1)
(defun completion/disable-line-numbers ()
  "Disable line numbers in completion list buffers."
  (display-line-numbers-mode 0))
(add-hook 'completion-list-mode-hook #'completion/disable-line-numbers)
(line-number-mode 0)
(column-number-mode 0)
(global-hl-line-mode 1)

(setq warning-minimum-level :error)

(repeat-mode 1)

(scroll-bar-mode 1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(cua-mode 1)
(define-key isearch-mode-map (kbd "C-v") 'isearch-yank-kill)
(setq window-divider-default-right-width 8
      window-divider-default-bottom-width 8)

(window-divider-mode 1)

(setq initial-scratch-message nil)

(setq-default fill-column 148)

(defun fonts/set-fonts ()
  (interactive)
  (set-face-attribute 'default nil :family "JetBrains Mono")

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil :family "JetBrains Mono" :height 1.0)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :family "Cantarell" :weight 'regular :height 1.0)
  (fix-char-width-for-spinners))
(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'fonts/set-fonts)
  (add-hook 'window-setup-hook #'fonts/set-fonts))

(use-package textsize
  :vc (:url "https://github.com/WJCFerguson/textsize" :rev :newest)
  :custom
  (textsize-default-points 10)
  (textsize-monitor-size-thresholds '((0 . -3) (280 . 0) (500 . 3) (1000 . 6) (1500 . 9)))
  (textsize-pixel-pitch-thresholds '((0 . 3) (0.12 . 0) (0.18 . -3) (0.22 . -6) (0.40 . 12)))
  :init (textsize-mode))

(setq auth-sources '("~/.authinfo.json.gpg" "~/.authinfo.gpg"))

(setq remote-file-name-inhibit-cache nil
      vc-ignore-dir-regexp
      (format "%s\\|%s"
              vc-ignore-dir-regexp
              tramp-file-name-regexp)
      tramp-verbose 1
      tramp-default-method "ssh"
      tramp-use-ssh-controlmaster-options nil
      tramp-chunksize 500
      tramp-auto-save-directory "~/.emacs.d/var/tramp-autosave/")

(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(defun theme/hide-minibuffer-scrollbar (&optional frame)
  (set-window-scroll-bars (minibuffer-window frame) 0 nil))

(defun theme/apply ()
  (interactive)
  (load-file "~/.emacs.d/custom_packages/dracula-theme.el")
  (load-theme 'dracula t)

  (modify-all-frames-parameters '((internal-border-width . 0)))
  (fringe-mode '(12 . 12))
  (theme/hide-minibuffer-scrollbar))

(add-hook 'after-make-frame-functions #'theme/hide-minibuffer-scrollbar)
(add-hook 'after-init-hook #'theme/apply)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq
   inhibit-compacting-font-caches t
   mode-line-right-align-edge 'right-margin
   doom-modeline-percent-position nil
   doom-modeline-enable-word-count t
   doom-modeline-continuous-word-count-modes nil
   doom-modeline-total-line-number nil
   doom-modeline-total-line-number nil
   doom-modeline-workspace-name nil
   doom-modeline-height 18
   doom-modeline-bar-width 4)
  (line-number-mode 0)
  (column-number-mode 0))

(use-package hide-mode-line
  :config
  (add-hook 'completion-list-mode-hook #'hide-mode-line-mode))

(setq tab-always-indent 'complete)
(setq completions-format 'one-column
      completions-header-format nil
      completion-show-help nil
      completion-show-inline-help nil
      completions-max-height 30
      completion-auto-select nil)

(setq-default isearch-lazy-count t
              isearch-allow-motion t)


(defun my/minibuffer-choose-completion (&optional no-exit no-quit)
  (interactive "P")
  (with-minibuffer-completions-window
    (let ((completion-use-base-affixes nil))
      (choose-completion nil no-exit no-quit))))

(use-package consult
  :bind
  (;; C-x bindings (buffer)
   ("C-x b"   . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x r b" . consult-bookmark)
   ("C-x p b" . consult-project-buffer)
   ;; M-g bindings (goto)
   ("M-g g"   . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g o"   . consult-outline)
   ("M-g m"   . consult-mark)
   ("M-g k"   . consult-global-mark)
   ("M-g i"   . consult-imenu)
   ("M-g I"   . consult-imenu-multi)
   ("M-g f"   . consult-flymake)
   ;; M-s bindings (search)
   ("M-s d"   . consult-find)
   ("M-s g"   . consult-grep)
   ("M-s G"   . consult-git-grep)
   ("M-s r"   . consult-ripgrep)
   ("M-s l"   . consult-line)
   ("M-s L"   . consult-line-multi)
   ("M-s k"   . consult-keep-lines)
   ("M-s u"   . consult-focus-lines)
   ;; Other
   ("M-y"     . consult-yank-pop)
   ("C-x M-:" . consult-complex-command)
   ("M-#"     . consult-register-load)
   ("M-'"     . consult-register-store)
   ("C-M-#"   . consult-register))
  :config
  (setq consult-narrow-key "<"))

(use-package company
  :defer 1  ; Delay loading by 1 second
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 2)  ; Reduced CPU usage
  (company-idle-delay 0.15)          ; 0.01 was too aggressive
  (company-tooltip-idle-delay 0.15)
  :config
  (global-company-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
  completion-category-defaults nil
  completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :init
  (vertico-mode 1)
  (vertico-buffer-mode 1)
  :config
  (setq vertico-buffer-display-action
        '(display-buffer-in-direction
          (direction . below)
          (window-height . 0.2))))

(use-package marginalia
  :init
  (marginalia-mode 1))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim))
  :config
  (setq prefix-help-command #'embark-prefix-help-command)

  (defun embark/add-ctrl-bindings (keymap)
    "Duplicate uppercase letter bindings in KEYMAP as C-<letter>."
    (map-keymap
     (lambda (key cmd)
       (when (and (integerp key) (>= key ?A) (<= key ?Z))
         (define-key keymap (vector (- key 64)) cmd)))
     keymap))

  (embark/add-ctrl-bindings embark-general-map)
  (dolist (entry embark-keymap-alist)
    (dolist (map-sym (ensure-list (cdr entry)))
      (when (and (symbolp map-sym) (boundp map-sym)
                 (keymapp (symbol-value map-sym)))
        (embark/add-ctrl-bindings (symbol-value map-sym))))))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package org
  :config
  (define-key org-mode-map (kbd "C-M-S-<left>") nil)
  (define-key org-mode-map (kbd "C-M-S-<right>") nil)
  (define-key org-mode-map (kbd "C-S-<right>") nil)
  (define-key org-mode-map (kbd "C-S-<left>") nil)

  (setq
   org-confirm-babel-evaluate nil
   org-image-actual-width t
   org-startup-with-inline-images t
   org-support-shift-select t)

  (load-file "~/.emacs.d/custom_packages/org-flyimage.el")
  (with-eval-after-load "org"
    (require 'org-flyimage)
    (add-hook 'org-mode-hook 'org-flyimage-mode))

  (defun org/org-babel-tangle-config ()
    (when (or (string-equal (buffer-file-name)
                            (expand-file-name "~/.dotfiles/README.org"))
              (string-equal (buffer-file-name)
                            (expand-file-name "~/.dotfiles/river/README.org"))
              (string-equal (buffer-file-name)
                            (expand-file-name "~/.dotfiles/fnott/README.org"))
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
  (defun org/enable-auto-tangle ()
    "Enable auto-tangle on save in org-mode buffers."
    (add-hook 'after-save-hook #'org/org-babel-tangle-config nil t))
  (add-hook 'org-mode-hook #'org/enable-auto-tangle))
;; (custom-set-faces
;;  '(org-level-1 ((t (:inherit outline-1 :height 2.5))))
;;  '(org-level-2 ((t (:inherit outline-2 :height 1.8))))
;;  '(org-level-3 ((t (:inherit outline-3 :height 1.4))))
;;  '(org-level-4 ((t (:inherit outline-4 :height 1.2))))
;;  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))))

(use-package org-tree-slide
  :config
  (define-key org-tree-slide-mode-map (kbd "M-p") 'org-tree-slide-move-previous-tree)
  (define-key org-tree-slide-mode-map (kbd "M-n") 'org-tree-slide-move-next-tree))

;; (make-directory "~/RoamNotes")
(use-package org-roam
  :custom
  (org-roam-directory "~/RoamNotes")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(use-package org-roam-ui
  :vc (:url "https://github.com/org-roam/org-roam-ui" :rev :newest)
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package markdown-mode
  :hook (markdown-mode . visual-line-mode))

(use-package time
  :ensure nil
  :commands world-clock
  :config
  (setq display-time-interval 60)
  (setq display-time-mail-directory nil)
  (setq display-time-default-load-average nil))

(use-package avy
  :config
  (require 'bind-key)
  (bind-key "M-j" #'avy-goto-char-timer))

(defun mc/setup-cursor-face ()
  "Set up cursor face for multiple-cursors mode."
  (set-face-attribute 'mc/cursor-bar-face nil :height 1 :background nil :inherit 'cursor))
(use-package multiple-cursors
  :vc (:url "https://github.com/magnars/multiple-cursors.el" :rev :newest)
  :hook
  ((multiple-cursors-mode . mc/setup-cursor-face))
  :config
  (define-key mc/keymap (kbd "<return>") nil)
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C-}") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-{") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-'") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
  (setq mc/black-list-prefer t))

(use-package kmacro-x
  :init (kmacro-x-atomic-undo-mode 1))

(use-package combobulate
  :vc (:url "https://github.com/mickeynp/combobulate" :rev :newest)
  :hook
  ((python-ts-mode . combobulate-mode)
   (js-ts-mode . combobulate-mode)
   (html-ts-mode . combobulate-mode)
   (css-ts-mode . combobulate-mode)
   (yaml-ts-mode . combobulate-mode)
   (typescript-ts-mode . combobulate-mode)
   (json-ts-mode . combobulate-mode)
   (tsx-ts-mode . combobulate-mode))
  :config
  (setq combobulate-flash-node nil
        combobulate-message-node-highlighted nil
        combobulate-javascript-highlight-queries-default nil
        combobulate-typescript-highlight-queries-default nil
        combobulate-tsx-highlight-queries-default nil))

(use-package goto-last-change
  :config
  (global-set-key (kbd "C-z") 'goto-last-change))

(use-package vundo
  :vc (:url "https://github.com/casouri/vundo" :rev :newest)
  :bind (
         :map vundo-mode-map
         ("C-b" . vundo-backward)
         ("C-f" . vundo-forward)
         ("C-p" . vundo-previous)
         ("C-n" . vundo-next)
         ("<home>" . vundo-stem-root)
         ("<end>" . vundo-stem-end))
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  (global-unset-key (kbd "C-?"))
  (global-set-key (kbd "C-?") 'vundo))

(use-package hideshow
  :ensure nil
  :hook
  (prog-mode . hs-minor-mode)
  (yaml-ts-mode . hs-minor-mode)
  :bind (
         :map prog-mode-map
         ("C-M-<tab>" . hs-cycle)
         :map hs-minor-mode-map
         ("C-M-<tab>" . hs-cycle))
  :init
  (with-eval-after-load 'hideshow
    (define-key prog-mode-map (kbd (if *is-linux* "C-M-<iso-lefttab>" "C-M-S-<tab>")) #'hs-global-cycle)
    (define-key hs-minor-mode-map (kbd (if *is-linux* "C-M-<iso-lefttab>" "C-M-S-<tab>")) #'hs-global-cycle))
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
    :ensure nil
    :config
    (setq ediff-window-setup-function 'ediff-setup-windows-plain
          ediff-split-window-function 'split-window-horizontally))

(use-package perfect-margin
  :vc (:url "https://github.com/mpwang/perfect-margin" :rev :newest)
  :defer 2
  :config
  (setq perfect-margin-only-set-left-margin nil
        perfect-margin-ignore-regexps '("*docker-container*" "*Ibuffer*" "* docker")
        perfect-margin-ignore-filters nil
        perfect-margin-visible-width 148
        perfect-margin-disable-in-splittable-check nil)

  (defun perfect-margin-minibuffer-setup ()
    "Apply perfect-margin to minibuffer window."
    (when perfect-margin-mode
      (let* ((win (active-minibuffer-window))
             (width (window-total-width win))
             (margin (max 0 (/ (- width perfect-margin-visible-width) 2))))
        (set-window-margins win margin margin))))
  (add-hook 'minibuffer-setup-hook #'perfect-margin-minibuffer-setup)

  (perfect-margin-mode 0))

(use-package pgmacs
  :vc (:url "https://github.com/emarsden/pgmacs" :rev :newest)
  :defer t
  :custom
  (pgmacs-row-limit 100)
  (pgmacs-large-database-threshold 0)
  :config
  (defun pgmacs-open-sql-connection ()
    "Open PGmacs using a connection from `sql-connection-alist'."
    (interactive)
    (require 'sql)
    (let* ((name (completing-read "SQL connection: "
                                  (mapcar #'car sql-connection-alist) nil t))
           (conn (cdr (assoc-string name sql-connection-alist)))
           (get (lambda (sym) (cadr (assoc sym conn))))
           (host (funcall get 'sql-server))
           (port (funcall get 'sql-port))
           (db   (funcall get 'sql-database))
           (user (funcall get 'sql-user))
           (pass (funcall get 'sql-password))
           (str  (format "host=%s port=%s dbname=%s user=%s password=%s"
                         host (or port 5432) db user pass)))
      (pgmacs-open-string str))))

(use-package emms
  :config
  (emms-all)
  (setq emms-player-list '(emms-player-vlc)
        emms-info-functions '(emms-info-native)))

(use-package string-inflection
  :defer t
  :bind (("C-c C-u C-u" . string-inflection-upcase)
         ("C-c C-u C-k" . string-inflection-kebab-case)
         ("C-c C-u C-c" . string-inflection-lower-camelcase)
         ("C-c C-u C-S-c" . string-inflection-camelcase)
         ("C-c C-u C--" . string-inflection-underscore)
         ("C-c C-u C-_" . string-inflection-capital-underscore)))

(use-package sudo-edit
  :commands (sudo-edit sudo-edit-find-file))

(use-package which-key
  :defer 2  ; Load after 2 seconds idle
  :config
  (defun which-key/popup-dimensions (_)
    "Return custom dimensions for which-key popup."
    (cons (round (* (window-height) 0.25)) (window-width)))
  (defun which-key/show-popup (_act-popup-dim)
    "Show which-key popup below the selected window."
    (when-let* ((buf (get-buffer which-key-buffer-name)))
      (display-buffer buf
                      '((display-buffer-below-selected)
                        (window-height . fit-window-to-buffer)))))
  (defun which-key/hide-popup ()
    "Hide the which-key popup window."
    (when-let* ((buf (get-buffer which-key-buffer-name))
                (win (get-buffer-window buf)))
      (quit-window nil win)))
  (setq which-key-popup-type 'custom
        which-key-custom-popup-max-dimensions-function #'which-key/popup-dimensions
        which-key-custom-show-popup-function #'which-key/show-popup
        which-key-custom-hide-popup-function #'which-key/hide-popup
        which-key-idle-delay 0.5)
  (which-key-mode 1))

(use-package whole-line-or-region
  :defer 1
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
  :vc (:url "https://github.com/emacs-straight/blist" :rev :newest)
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

(use-package wgrep
  :defer t
  :config
  (setq wgrep-auto-save-buffer t)

  (defun wgrep/hook ()
    (wgrep-change-to-wgrep-mode)))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package helpful
  :defer t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command))
  :config
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-funtion #'helpful-variable))

(use-package explain-pause-mode
  :vc (:url "https://github.com/lastquestion/explain-pause-mode" :rev :newest))

(use-package free-keys
  :commands free-keys)

(use-package csv-mode
  :vc (:url "https://github.com/emacsmirror/csv-mode" :rev :newest)
  :config
  (setq csv-comment-start-default nil)
  (customize-set-variable 'csv-separators '("," "	" ";" "~"))
  (customize-set-variable 'csv-header-lines 1)
  (add-hook 'csv-mode-hook 'csv-align-mode)
  (add-hook 'csv-mode-hook 'csv-header-line)

  (defcustom csv+-quoted-newline "\^@"
    "Replace for newlines in quoted fields."
    :group 'sv
    :type 'string)

  (defun csv+-quoted-newlines (&optional b e inv)
    "Replace newlines in quoted fields of region B E by `csv+-quoted-newline'.
B and E default to `point-min' and `point-max', respectively.
If INV is non-nil replace quoted `csv+-quoted-newline' chars by newlines."
    (interactive
     (append (when (region-active-p)
               (list (region-begin)
                     (region-end)))
             prefix-arg))
    (unless b (setq b (point-min)))
    (unless e (setq e (point-max)))
    (save-excursion
      (goto-char b)
      (let ((from (if inv csv+-quoted-newline "\n"))
            (to (if inv "\n" csv+-quoted-newline)))
        (while (search-forward from e t)
          (when (nth 3 (save-excursion (syntax-ppss (1- (point)))))
            (replace-match to))))))

  (defsubst csv+-quoted-newlines-write-contents ()
    "Inverse operation of `csv+-quoted-newlines' for the full buffer."
    (save-excursion
      (save-restriction
        (widen)
        (let ((file (buffer-file-name))
              (contents (buffer-string)))
          (with-temp-buffer
            (insert contents)
            (csv+-quoted-newlines (point-min) (point-max) t)
            (write-region (point-min) (point-max) file)))))
    (set-visited-file-modtime)
    (set-buffer-modified-p nil)
    t ;; File contents has been written (see `write-contents-functions').
    )

  (defun csv+-setup-quoted-newlines ()
    "Hook function for `csv-mode-hook'.
Transform newlines in quoted fields to `csv+-quoted-newlines'
when reading files and the other way around when writing contents."
    (add-hook 'write-contents-functions #'csv+-quoted-newlines-write-contents t t)
    (let ((modified-p (buffer-modified-p)))
      (csv+-quoted-newlines)
      (set-buffer-modified-p modified-p)))

  (remove-hook 'csv-mode-hook #'csv+-setup-quoted-newlines))

(use-package d2-mode)

(use-package ob-d2
  :vc (:url "https://github.com/dmacvicar/ob-d2" :rev :newest)
  :defer t)

(use-package jinx
  :ensure nil
  :if (or *is-linux* (executable-find "enchant-2"))
  :hook ((text-mode org-mode markdown-mode) . jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

(use-package docker
  :config
  (global-set-key (kbd "C-c d") 'docker)
  (setq docker-show-messages nil
        docker-container-default-sort-key '("Names"))

  (defun docker-run-async-with-buffer-shell (program &rest args)
    "Execute \"PROGRAM ARGS\" and display output in a new `shell' buffer."
    (let* ((process (apply #'docker-run-start-file-process-shell-command program args))
           (buffer (process-buffer process)))
      (set-process-query-on-exit-flag process nil)
      (with-current-buffer buffer (shell-mode))
      (set-process-filter process 'comint-output-filter)
      (display-buffer-same-window buffer nil)))

  (defun docker-run-async-with-buffer-vterm (program &rest args)
    "Execute \"PROGRAM ARGS\" and display output in a new `vterm' buffer."
    (defvar vterm-kill-buffer-on-exit)
    (defvar vterm-shell)
    (if (fboundp 'vterm-same-window)
        (let* ((process-args (-remove 's-blank? (-flatten args)))
               (vterm-shell (s-join " " (-insert-at 0 program process-args)))
               (vterm-kill-buffer-on-exit nil))
          (vterm-same-window
           (apply #'docker-utils-generate-new-buffer-name program process-args)))
      (error "The vterm package is not installed")))

  (defun docker-run-async-with-buffer-shell (program &rest args)
    "Execute \"PROGRAM ARGS\" and display output in a new `shell' buffer."
    (let* ((process (apply #'docker-run-start-file-process-shell-command program args))
           (buffer (process-buffer process)))
      (set-process-query-on-exit-flag process nil)
      (with-current-buffer buffer (shell-mode))
      (set-process-filter process 'comint-output-filter)
      (display-buffer-same-window buffer nil)))

  (defun run-with-local-timer (secs repeat function &rest args)
    "Like `run-with-idle-timer', but always runs in the `current-buffer'.

  Cancels itself, if this buffer was killed."
    (let* (;; Chicken and egg problem.
           (fns (make-symbol "local-timer"))
           (timer (apply 'run-with-timer secs repeat fns args))
           (fn `(lambda (&rest args)
                  (if (not (buffer-live-p ,(current-buffer)))
                      (cancel-timer ,timer)
                    (with-current-buffer ,(current-buffer)
                      (apply (function ,function) args))))))
      (fset fns fn)
      fn))

  (defvar-local docker/auto-refresh--timer nil
    "Timer for auto-refreshing docker containers view.")

  (defun docker/auto-refresh ()
    "Auto-refresh docker containers view. Cancels itself, if this buffer was killed."
    (when docker/auto-refresh--timer
      (cancel-timer docker/auto-refresh--timer))
    (setq docker/auto-refresh--timer
          (run-with-local-timer 5 5 'revert-buffer)))

  (add-hook 'docker-container-mode-hook #'docker/auto-refresh)

  (docker-utils-columns-setter 'docker-container-columns '((:name "Names" :width 40 :template "{{ json .Names }}" :sort nil :format nil)
                                                           (:name "Status" :width 40 :template "{{ json .Status }}" :sort nil :format nil)
                                                           (:name "Ports" :width 60 :template "{{ json .Ports }}" :sort nil :format nil)
                                                           (:name "Id" :width 16 :template "{{ json .ID }}" :sort nil :format nil)
                                                           (:name "Image" :width 90 :template "{{ json .Image }}" :sort nil :format nil)
                                                           (:name "Command" :width 30 :template "{{ json .Command }}" :sort nil :format nil)
                                                           (:name "Created" :width 23 :template "{{ json .CreatedAt }}" :sort nil :format
                                                                  (lambda (x) (format-time-string "%F %T" (date-to-time x))))
                                                           )
                               )
  )

(use-package gptel
  :config
  (setq
   gptel-default-mode 'org-mode
   gptel-prompt-prefix-alist '((markdown-mode . "## ") (org-mode . "** ") (text-mode . "## "))))

(use-package agent-shell
  :ensure t
  :config
  (setq agent-shell-show-welcome-message nil
        agent-shell-show-busy-indicator nil
        agent-shell-header-style 'none
        agent-shell-prefer-viewport-interaction nil
        agent-shell-preferred-agent-config 'claude-code
        agent-shell-highlight-blocks nil
        agent-shell-embed-file-size-limit 51200
        markdown-overlays-highlight-blocks nil
        markdown-overlays-render-latex nil)

  (add-to-list 'exec-path "~/.npm-packages/bin")
  (add-hook 'agent-shell-mode-hook (lambda () (display-line-numbers-mode -1)))

  (defun agent/shell ()
    "Start agent shell or reuse existing shell for current project.
If the shell buffer is focused in a side window, return focus to the
main window without closing the side window.
If the shell buffer is visible in another frame, focus that frame.
Preserves context (region, files, etc.) like the default behavior."
    (interactive)
    (let* ((context (agent-shell--context))
           (shell-buffer (agent-shell--shell-buffer :no-error t :no-create t))
           (shell-window (when shell-buffer
                           (get-buffer-window shell-buffer t))))
      (cond
       ;; Focused in side window: return to main window
       ((and shell-window
             (window-parameter shell-window 'window-side)
             (eq (current-buffer) shell-buffer))
        (select-window (get-mru-window nil nil t)))
       ;; Visible: focus it and paste context ourselves
       (shell-window
        (select-frame-set-input-focus (window-frame shell-window))
        (select-window shell-window)
        (when (and context (not shell-maker--busy))
          (goto-char (point-max))
          (insert "\n\n" context)
          (comint-previous-prompt 1)))
       ;; Running but not visible: display it and paste context ourselves
       (shell-buffer
        (agent-shell--display-buffer shell-buffer)
        (when (and context (not shell-maker--busy))
          (with-current-buffer shell-buffer
            (goto-char (point-max))
            (insert "\n\n" context)
            (comint-previous-prompt 1))))
       ;; Not running: create shell and paste context ourselves
       (t
        (let ((new-buffer (agent-shell--shell-buffer)))
          (agent-shell--display-buffer new-buffer)
          (when (and context (not shell-maker--busy))
            (with-current-buffer new-buffer
              (goto-char (point-max))
              (insert "\n\n" context)
              (comint-previous-prompt 1))))))))

  ;; Navigation avec M-n / M-p pour les pages du viewport
  (with-eval-after-load 'agent-shell-viewport
    (define-key agent-shell-viewport-view-mode-map (kbd "M-n") #'agent-shell-viewport-next-page)
    (define-key agent-shell-viewport-view-mode-map (kbd "M-p") #'agent-shell-viewport-previous-page)
    (define-key agent-shell-viewport-edit-mode-map (kbd "M-n") #'agent-shell-viewport-next-page)
    (define-key agent-shell-viewport-edit-mode-map (kbd "M-p") #'agent-shell-viewport-previous-page))

  (defun agent/display-below (buffer alist)
    "Display BUFFER below, reusing existing window if present."
    (let ((below (window-in-direction 'below)))
      (if below
          (progn
            (set-window-dedicated-p below nil)
            (window--display-buffer buffer below 'reuse alist))
        (display-buffer-in-direction buffer
                                     (append '((direction . below)
                                               (window-height . 0.6))
                                             alist)))))

  (add-to-list 'display-buffer-alist
               '("\\*agent-shell-diff\\*"
                 (agent/display-below)))

  (defun agent/diff-then-close (orig-fn &rest args)
    "Call ORIG-FN, then kill the diff buffer and its window."
    (let ((diff-buf (current-buffer))
          (diff-win (selected-window)))
      (apply orig-fn args)
      (when (buffer-live-p diff-buf)
        (kill-buffer diff-buf))
      (when (window-live-p diff-win)
        (delete-window diff-win))))

  (advice-add 'agent-shell-diff-accept-all :around #'agent/diff-then-close)
  (advice-add 'agent-shell-diff-reject-all :around #'agent/diff-then-close)

  (global-set-key (kbd "C-c a") #'agent/shell))

(use-package joplin-mode
  :vc (:url "https://github.com/cinsk/joplin-mode" :rev :newest)
  :commands (joplin joplin-search joplin-create-note joplin-create-notebook
                    joplin-journal joplin-delete-note-dwim joplin-delete-notebook-dwim)
  :bind (("M-s j"   . joplin-find-note)
         ("M-s M-j" . joplin-find-note)
         ("C-c j"   . joplin-transient-menu))
  :config
  (setq joplin-token (auth-source-pick-first-password :host "joplin"))

  ;; --- Async HTTP helper for Joplin local API ---
  (defun joplin--http-get-async-handler (cb _status)
    "Parse JSON response from url-retrieve and call CB with result."
    (goto-char (point-min))
    (when (re-search-forward "\n\n" nil t)
      (condition-case nil
          (let ((data (json-read)))
            (funcall cb data))
        (error nil)))
    (kill-buffer (current-buffer)))

  (defun joplin--http-get-async (url cb &optional context)
    "Async version of joplin--http-get. Calls CB with parsed JSON."
    (let ((url-proxy-services joplin-url-proxy))
      (url-retrieve
       (joplin--build-url url (append context joplin-context))
       (apply-partially #'joplin--http-get-async-handler cb))))

  (defun joplin--find-folder (name &optional parent-id)
    "Find a folder with NAME under PARENT-ID.
  When multiple folders match, return the most recently modified one.
  Returns folder ID or nil."
    (let ((candidates nil))
      (cl-loop for (_ . folder) in joplin-folders
               when (and (string-equal (JFOLDER-title folder) name)
                         (or (null parent-id)
                             (string-equal (JFOLDER-parent_id folder) parent-id)))
               do (push (JFOLDER-id folder) candidates))
      (if (<= (length candidates) 1)
          (car candidates)
        ;; Pre-fetch update times O(N) instead of O(N*logN) in sort comparator
        (let ((times (make-hash-table :test 'equal)))
          (dolist (id candidates)
            (puthash id
                     (or (alist-get 'updated_time
                                    (joplin--http-get (format "/folders/%s" id))) 0)
                     times))
          (car (sort candidates
                     (lambda (a b)
                       (> (gethash a times) (gethash b times)))))))))

  (defun joplin--find-or-create-folder (name &optional parent-id)
    "Find or create a folder with NAME under PARENT-ID. Returns folder ID."
    (or (joplin--find-folder name parent-id)
        (let* ((args `((title . ,name)))
               (args (if parent-id (cons `(parent_id . ,parent-id) args) args))
               (resp (joplin--http-post "/folders" args)))
          (joplin--init-folders)
          (alist-get 'id resp))))

  (defun joplin--find-note-in-folder (title folder-id)
    "Find a note with TITLE in FOLDER-ID. Returns note ID or nil."
    (let* ((notes (joplin--http-get (format "/folders/%s/notes" folder-id)
                                    '((fields . "id,title"))))
           (items (if (vectorp notes) notes
                    (alist-get 'items notes))))
      (cl-loop for note across items
               when (string-equal (alist-get 'title note) title)
               return (alist-get 'id note))))

  (defun joplin--folder-by-id (folder-id)
    "Return the JFOLDER struct for FOLDER-ID, or nil."
    (cl-loop for (_ . folder) in joplin-folders
             when (string-equal (JFOLDER-id folder) folder-id)
             return folder))

  (defun joplin--folder-path (folder-id)
    "Return the full path for FOLDER-ID (e.g. \"Journal/2026/02-Feb\")."
    (let ((path nil) (id folder-id))
      (while id
        (let ((folder (joplin--folder-by-id id)))
          (if folder
              (progn
                (push (JFOLDER-title folder) path)
                (let ((pid (JFOLDER-parent_id folder)))
                  (setq id (if (and pid (not (string-empty-p pid))) pid nil))))
            (setq id nil))))
      (string-join path "/")))

  (defun joplin--resolve-folder-path (segments)
    "Resolve a list of folder name SEGMENTS, creating missing folders.
  Returns the folder ID of the last segment."
    (let ((parent-id nil))
      (dolist (name segments parent-id)
        (setq parent-id (joplin--find-or-create-folder name parent-id)))))

  (defun joplin--context-folder-id ()
    "Return the folder ID from the current joplin-mode context, or nil.
  In a folder browser, returns the folder under cursor.
  In a note list or note buffer, returns the note's parent folder."
    (cond
     ((joplin--folder-at-point)
      (JFOLDER-id (joplin--folder-at-point)))
     ((joplin--search-note-at-point)
      (JNOTE-parent_id (joplin--search-note-at-point)))
     ((bound-and-true-p joplin-note)
      (JNOTE-parent_id joplin-note))))

  (defun joplin--context-folder-path ()
    "Return the folder path from the current Joplin context, with trailing slash."
    (let ((folder-id (joplin--context-folder-id)))
      (if folder-id
          (concat (joplin--folder-path folder-id) "/")
        "")))

  (defun joplin--folder-children (parent-id)
    "Return sorted list of child folder names under PARENT-ID (nil for root)."
    (sort
     (cl-loop for (_ . folder) in joplin-folders
              when (if parent-id
                       (string-equal (JFOLDER-parent_id folder) parent-id)
                     (or (null (JFOLDER-parent_id folder))
                         (string-empty-p (JFOLDER-parent_id folder))))
              collect (JFOLDER-title folder))
     #'string<))

  (defun joplin--folder-notes (folder-id)
    "Return sorted list of note titles in FOLDER-ID."
    (when folder-id
      (let* ((resp (joplin--http-get (format "/folders/%s/notes" folder-id)
                                     '((fields . "id,title") (limit . "100"))))
             (items (if (vectorp resp) resp (alist-get 'items resp))))
        (sort (cl-loop for note across items
                       collect (alist-get 'title note))
              #'string<))))

  (defun joplin--folder-contents (folder-id)
    "Return candidates for FOLDER-ID: subfolders with trailing /, then notes."
    (append (mapcar (lambda (f) (concat f "/")) (joplin--folder-children folder-id))
            (joplin--folder-notes folder-id)))

  (defun joplin--read-path (prompt)
    "Read a Joplin path with iterative folder navigation.
  Each step shows subfolders (suffixed with /) and notes of the
  current folder.  Selecting a subfolder navigates into it;
  selecting a note or typing a new name returns the full path."
    (let* ((init (joplin--context-folder-path))
           (segments (split-string init "/" t))
           (folder-id nil)
           (path ""))
      ;; Resolve initial context path
      (dolist (s segments)
        (let ((id (joplin--find-folder s folder-id)))
          (when id
            (setq folder-id id
                  path (concat path s "/")))))
      ;; Iterative navigation
      (catch 'done
        (while t
          (let* ((candidates (joplin--folder-contents folder-id))
                 (sel (completing-read (concat prompt path) candidates)))
            (cond
             ((and (> (length sel) 0) (string-suffix-p "/" sel))
              (let ((name (substring sel 0 -1)))
                (setq folder-id (joplin--find-folder name folder-id)
                      path (concat path name "/"))))
             ((> (length sel) 0)
              (throw 'done (concat path sel)))
             (t
              (throw 'done (string-trim-right path "/")))))))))

  (defun joplin-create-notebook (path)
    "Create a Joplin notebook at PATH (e.g. \"Parent/Child/New\").
  Missing notebooks in the path are created automatically.
  In a Joplin view, the path is pre-filled from the notebook at point."
    (interactive
     (progn
       (joplin--init)
       (list (joplin--read-path "Notebook path: "))))
    (let ((parts (split-string path "/" t)))
      (joplin--resolve-folder-path parts)
      (message "Notebook ready: %s" path)))

  (defun joplin-create-note (path)
    "Create or open a Joplin note at PATH (e.g. \"Folder/Sub/My Note\").
  The last segment is the note title, everything before is the notebook path.
  Missing notebooks are created automatically.  If the note already exists,
  it is opened directly.
  In a Joplin view, the path is pre-filled from the notebook at point."
    (interactive
     (progn
       (joplin--init)
       (list (joplin--read-path "Note path: "))))
    (let* ((parts (split-string path "/" t))
           (title (car (last parts)))
           (folder-parts (butlast parts))
           (folder-id (when folder-parts
                        (joplin--resolve-folder-path folder-parts)))
           (existing (when folder-id
                       (joplin--find-note-in-folder title folder-id)))
           (note-id (or existing
                        (alist-get 'id (joplin--http-post
                                        "/notes/"
                                        `((title . ,title)
                                          ,@(when folder-id
                                              `((parent_id . ,folder-id)))
                                          (body . ""))))))
           (buf (joplin--note-buffer note-id)))
      (switch-to-buffer buf)))

  (defun joplin--journal-note-id (date-str)
    "Return the note ID for journal entry DATE-STR, creating it if needed.
  Ensures the Journal/YYYY/MM-Xxx notebook hierarchy exists."
    (joplin--init)
    (let* ((date (parse-time-string date-str))
           (month (nth 4 date))
           (year (nth 5 date))
           (month-abbrevs ["" "Jan" "Feb" "Mar" "Apr" "May" "Jun"
                           "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"])
           (year-str (number-to-string year))
           (month-str (format "%02d-%s" month (aref month-abbrevs month)))
           (journal-id (joplin--find-or-create-folder "Journal"))
           (year-id (joplin--find-or-create-folder year-str journal-id))
           (month-id (joplin--find-or-create-folder month-str year-id))
           (existing (joplin--find-note-in-folder date-str month-id)))
      (or existing
          (alist-get 'id (joplin--http-post
                          "/notes/"
                          `((title . ,date-str)
                            (parent_id . ,month-id)
                            (body . "")))))))

  (defun joplin-journal (date-str)
    "Create or open a Joplin journal note for DATE-STR (YYYY-MM-DD).
  Creates the Journal/YYYY/MM-Xxx notebook hierarchy as needed."
    (interactive
     (list (read-from-minibuffer "Journal date (YYYY-MM-DD): "
                                 (format-time-string "%Y-%m-%d"))))
    (joplin--init)
    (switch-to-buffer (joplin--note-buffer (joplin--journal-note-id date-str))))

  (defun joplin-delete-note-dwim ()
    "Delete a Joplin note.  Uses note at point, current note buffer,
  or prompts for a path via completion."
    (interactive)
    (joplin--init)
    (let (title id)
      (cond
       ((joplin--search-note-at-point)
        (let ((note (joplin--search-note-at-point)))
          (setq title (JNOTE-title note)
                id (JNOTE-id note))))
       ((bound-and-true-p joplin-note)
        (setq title (JNOTE-title joplin-note)
              id (JNOTE-id joplin-note)))
       (t
        (let* ((path (joplin--read-path "Delete note: "))
               (parts (split-string path "/" t))
               (note-title (car (last parts)))
               (folder-parts (butlast parts))
               (folder-id (when folder-parts
                            (let ((fid nil))
                              (dolist (s folder-parts fid)
                                (setq fid (joplin--find-folder s fid)))))))
          (setq title note-title
                id (when folder-id
                     (joplin--find-note-in-folder note-title folder-id))))))
      (unless id (user-error "Note not found"))
      (when (yes-or-no-p (format "Delete note \"%s\"? " title))
        (joplin-delete-note id)
        (cond
         ((eq major-mode 'joplin-search-mode)
          (joplin-search-revert))
         ((bound-and-true-p joplin-note-mode)
          (set-buffer-modified-p nil)
          (kill-buffer)))
        (message "Deleted note: %s" title))))

  (defun joplin-delete-notebook-dwim ()
    "Delete a Joplin notebook.  Uses notebook at point in the folder
  browser, or prompts for a path via completion."
    (interactive)
    (joplin--init)
    (let (name id)
      (if-let ((folder (joplin--folder-at-point)))
          (setq name (JFOLDER-title folder)
                id (JFOLDER-id folder))
        (let* ((path (joplin--read-path "Delete notebook: "))
               (parts (split-string path "/" t)))
          (setq name (car (last parts))
                id (let ((fid nil))
                     (dolist (s parts fid)
                       (setq fid (joplin--find-folder s fid)))))))
      (unless id (user-error "Notebook not found"))
      (when (yes-or-no-p (format "Delete notebook \"%s\" and all its contents? " name))
        (joplin--http-del (concat "/folders/" id))
        (joplin--init-folders)
        (when (eq major-mode 'joplin-mode)
          (joplin-sync-folders))
        (message "Deleted notebook: %s" name))))

  (defun joplin/enable-edit-after-revert ()
    "Enable editing after reverting a Joplin note buffer."
    (setq buffer-read-only nil)
    (when view-mode (view-mode -1)))

  (defun joplin/remove-metadata-before-save ()
    "Remove metadata section before saving a Joplin note."
    (setq-local joplin--saving-via-hook t)
    (joplin--remove-metadata-section))

  (defun joplin/restore-metadata-after-save ()
    "Restore metadata section after saving a Joplin note."
    (unwind-protect
        (joplin--insert-metadata-section)
      (setq-local joplin--saving-via-hook nil)))

  ;; Find note buffers by note ID, not by buffer name
  (defun joplin--note-buffer-by-id (id &optional parent _buf-name)
    "Find or create a Joplin note buffer by note ID."
    (let ((buf (cl-loop for b in (buffer-list)
                        when (and (buffer-live-p b)
                                  (with-current-buffer b
                                    (and (bound-and-true-p joplin-note)
                                         (string= (JNOTE-id joplin-note) id))))
                        return b)))
      (unless buf
        (setq buf (joplin--note-fill-buffer
                   id (generate-new-buffer "*JoplinNote*") parent))
        (with-current-buffer buf
          (when view-mode (view-mode -1))
          (joplin--rename-buffer-from-note)
          (setq-local revert-buffer-function #'joplin--revert-note-buffer)
          (add-hook 'after-revert-hook #'joplin/enable-edit-after-revert 90 t)
          (joplin--insert-metadata-section)
          (add-hook 'before-save-hook #'joplin/remove-metadata-before-save -100 t)
          (add-hook 'after-save-hook #'joplin/restore-metadata-after-save 100 t)))
      (with-current-buffer buf
        (setq-local joplin-parent-buffer parent))
      buf))

  (advice-add 'joplin--note-buffer :override #'joplin--note-buffer-by-id)

  (defun joplin--journal-date-p (title)
    "Return non-nil if TITLE matches YYYY-MM-DD format."
    (and (stringp title)
         (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" title)))

  (defun joplin--journal-find-adjacent (date-str direction &optional max-days)
    "Find the nearest journal note in DIRECTION (-1 or +1) from DATE-STR.
  Search up to MAX-DAYS days (default 90).  Returns (TITLE . ID) or nil."
    (let* ((date (parse-time-string date-str))
           (time (encode-time 0 0 12 (nth 3 date) (nth 4 date) (nth 5 date)))
           (max-days (or max-days 90))
           (month-abbrevs ["" "Jan" "Feb" "Mar" "Apr" "May" "Jun"
                           "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"])
           (journal-id (joplin--find-folder "Journal"))
           (cache (make-hash-table :test 'equal))
           result)
      (when journal-id
        (cl-dotimes (i max-days)
          (unless result
            (let* ((offset (* direction (1+ i)))
                   (candidate (format-time-string
                               "%Y-%m-%d"
                               (time-add time (days-to-time offset))))
                   (cdate (parse-time-string candidate))
                   (cyear (number-to-string (nth 5 cdate)))
                   (cmonth (format "%02d-%s" (nth 4 cdate)
                                   (aref month-abbrevs (nth 4 cdate))))
                   (cache-key (concat cyear "/" cmonth))
                   (notes (or (gethash cache-key cache)
                              (let* ((yid (joplin--find-folder cyear journal-id))
                                     (mid (and yid (joplin--find-folder cmonth yid)))
                                     (ns (when mid
                                           (let* ((resp (joplin--http-get
                                                         (format "/folders/%s/notes" mid)
                                                         '((fields . "id,title") (limit . "100"))))
                                                  (items (if (vectorp resp) resp
                                                           (alist-get 'items resp))))
                                             (cl-loop for n across items
                                                      collect (cons (alist-get 'title n)
                                                                    (alist-get 'id n)))))))
                                (puthash cache-key (or ns 'empty) cache)
                                ns))))
              (unless (eq notes 'empty)
                (when-let ((hit (assoc candidate notes)))
                  (setq result hit)))))))
      result))

  (defun joplin--journal-nav-link (label date-str note-id)
    "Return a propertized string for a journal nav link."
    (let ((map (make-sparse-keymap))
          (id note-id))
      (define-key map [header-line mouse-1]
        `(lambda (e)
           (interactive "e")
           (switch-to-buffer (joplin--note-buffer ,id))))
      (propertize label
                  'face '(:foreground "#6272a4")
                  'mouse-face '(:foreground "#bd93f9" :underline t)
                  'help-echo date-str
                  'keymap map)))

  (defun joplin--journal-nav-header (title)
    "Return header-line navigation elements if TITLE is a journal date."
    (if (joplin--journal-date-p title)
        (let ((prev (joplin--journal-find-adjacent title -1))
              (next (joplin--journal-find-adjacent title  1)))
          (list
           "  "
           (propertize "|" 'face '(:foreground "#44475a"))
           "  "
           (if prev
               (joplin--journal-nav-link
                (concat "<< " (car prev))
                (car prev) (cdr prev))
             (propertize "<< ···" 'face '(:foreground "#44475a")))
           "    "
           (if next
               (joplin--journal-nav-link
                (concat (car next) " >>")
                (car next) (cdr next))
             (propertize "··· >>" 'face '(:foreground "#44475a")))))
      ""))

  (defun joplin--rename-buffer-from-note ()
    "Rename current buffer to match the Joplin note title."
    (when (bound-and-true-p joplin-note)
      (rename-buffer
       (format "*Joplin: %s (%s)*"
               (or (JNOTE-title joplin-note) "Untitled")
               (substring (JNOTE-id joplin-note) 0
                          (min 7 (length (JNOTE-id joplin-note)))))
       t)
      ;; Force doom-modeline to use buffer name instead of temp file path
      (setq doom-modeline--buffer-file-name
            (propertize (buffer-name)
                        'face 'doom-modeline-buffer-file
                        'mouse-face 'doom-modeline-highlight
                        'help-echo "Buffer name
mouse-1: Previous buffer\nmouse-3: Next buffer"
                        'local-map mode-line-buffer-identification-keymap))
      ;; Show note title in header-line
      (let* ((title (or (JNOTE-title joplin-note) "Untitled"))
             (folder (joplin--folder-path (JNOTE-parent_id joplin-note))))
        (setq header-line-format
              (list " "
                    (propertize folder 'face 'shadow)
                    (propertize "/" 'face 'shadow)
                    (propertize title 'face '(:weight bold))
                    (joplin--journal-nav-header title))))))

  ;; Linkify YYYY-MM-DD dates to journal notes on save
  (defun joplin--linkify-dates ()
    "Replace bare YYYY-MM-DD dates with Joplin journal links in the note body.
  Stops before the metadata section to avoid modifying read-only text."
    (when (bound-and-true-p joplin-note-mode)
      (save-excursion
        (goto-char (point-min))
        (let ((search-end (when (and (markerp joplin--metadata-start)
                                     (marker-position joplin--metadata-start))
                            joplin--metadata-start)))
          (while (re-search-forward
                  "\\b\\([0-9]\\{4\\}-[01][0-9]-[0-3][0-9]\\)\\b"
                  search-end t)
            (unless (and (> (match-beginning 0) (point-min))
                         (= (char-before (match-beginning 0)) ?\[))
              (let* ((date-str (match-string 1))
                     (mb (match-beginning 0))
                     (me (match-end 0))
                     (note-id (joplin--journal-note-id date-str))
                     (inhibit-modification-hooks t)
                     (link (format "[%s](:/%s)" date-str note-id)))
                (delete-region mb me)
                (goto-char mb)
                (insert link))))))))

  (defun joplin--before-save-note (&rest _)
    "Remove metadata section and linkify dates before saving a Joplin note."
    (joplin--remove-metadata-section)
    (joplin--linkify-dates))

  (advice-add 'joplin-save-note :before #'joplin--before-save-note)

  (defun joplin--after-save-note (&rest _)
    "Rename buffer and restore metadata section after save.
  When called from `write-file-functions' (inside `save-buffer'),
  skip metadata insertion — `after-save-hook' will handle it,
  avoiding read-only errors from premature re-insertion."
    (when (bound-and-true-p joplin-note)
      (let ((title (JNOTE-title joplin-note)))
        (unless title
          (setq-local joplin-note (joplin-get-note (JNOTE-id joplin-note))))))
    (joplin--rename-buffer-from-note)
    (unless joplin--saving-via-hook
      (joplin--insert-metadata-section)))

  (advice-add 'joplin-save-note :after #'joplin--after-save-note)

  (defun joplin--revert-body-callback (buf pos id data)
    "Handle async note body fetch for revert in BUF."
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (or (alist-get 'body data) "")))
        (goto-char (min pos (point-max)))
        (setq-local joplin-note (joplin-get-note id))
        (joplin--rename-buffer-from-note)
        (set-buffer-modified-p nil)
        (setq buffer-read-only nil)
        (when view-mode (view-mode -1))
        (joplin--insert-metadata-section)
        (message "Reverted note from Joplin"))))

  (defun joplin--revert-note-buffer (_ignore-auto _noconfirm)
    "Revert the current buffer from the Joplin API asynchronously."
    (let ((id (JNOTE-id joplin-note))
          (pos (point))
          (buf (current-buffer)))
      (message "Reverting note from Joplin...")
      (joplin--http-get-async
       (concat "/notes/" id)
       (apply-partially #'joplin--revert-body-callback buf pos id)
       '((fields . "body")))))

  (advice-remove 'joplin--note-fill-buffer #'joplin--after-note-fill-buffer)

  (defun joplin-backlinks ()
    "List notes that link to the current Joplin note."
    (interactive)
    (unless (bound-and-true-p joplin-note)
      (user-error "Not in a Joplin note buffer"))
    (joplin-search nil (JNOTE-id joplin-note)))

  (defvar-local joplin--metadata-start nil
    "Marker for the start of the metadata section in a Joplin note buffer.")

  (defvar-local joplin--saving-via-hook nil
    "Non-nil while `save-buffer' is in progress, to prevent the :after
  advice on `joplin-save-note' from re-inserting read-only backlinks
  during `write-file-functions'.")

  (defun joplin--backlinks-async-callback (buf note-id data)
    "Handle async backlinks response. Append backlinks to metadata in BUF."
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let* ((items (alist-get 'items data))
               (backlinks (cl-loop for item across items
                                   unless (string= (alist-get 'id item) note-id)
                                   collect (cons (alist-get 'title item)
                                                 (alist-get 'id item)))))
          (when backlinks
            (save-excursion
              (let ((inhibit-read-only t)
                    (buffer-undo-list t))
                (goto-char (point-max))
                (let ((start (point)))
                  (insert "\n## Backlinks\n")
                  (dolist (bl backlinks)
                    (insert (format "[%s](:/%s)\n" (car bl) (cdr bl))))
                  (add-text-properties start (point-max)
                                       '(read-only t front-sticky (read-only)
                                         joplin-metadata t face shadow))))))
        (set-buffer-modified-p nil)))))

  (defun joplin--fetch-backlinks-async ()
    "Fetch backlinks for the current note asynchronously."
    (when (bound-and-true-p joplin-note)
      (let ((id (JNOTE-id joplin-note))
            (buf (current-buffer)))
        (joplin--http-get-async
         "/search"
         (apply-partially #'joplin--backlinks-async-callback buf id)
         `((query . ,id)
           (type . note)
           (fields . "id,title")
           (limit . "50"))))))

  (defun joplin--remove-metadata-section ()
    "Remove the metadata section from the current buffer."
    (when (and (markerp joplin--metadata-start)
               (marker-position joplin--metadata-start))
      (let ((inhibit-read-only t)
            (buffer-undo-list t))
        (delete-region joplin--metadata-start (point-max))
        (set-marker joplin--metadata-start nil))))

  (defun joplin--format-joplin-time (ms)
    "Format a Joplin timestamp MS (milliseconds since epoch) as a readable string."
    (when (and ms (numberp ms) (> ms 0))
      (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time (/ ms 1000.0)))))

  (defun joplin--insert-metadata-section ()
    "Insert a read-only metadata section at the end of the current buffer.
  Includes note path, author, dates. Backlinks are fetched asynchronously."
    (joplin--remove-metadata-section)
    (when (bound-and-true-p joplin-note)
      (let* ((note joplin-note)
             (folder-path (joplin--folder-path (JNOTE-parent_id note)))
             (full-path (concat folder-path "/" (or (JNOTE-title note) "Untitled")))
             (author (JNOTE-author note))
             (created (joplin--format-joplin-time (JNOTE-user_created_time note)))
             (updated (joplin--format-joplin-time (JNOTE-user_updated_time note)))
             (tags (JNOTE-_tags note)))
        (save-excursion
          (let ((inhibit-read-only t)
                (buffer-undo-list t))
            (goto-char (point-max))
            ;; Ensure there is at least one editable line before metadata
            ;; so the user can type in an empty (new) note.
            (when (= (point-min) (point-max))
              (insert "\n"))
            (setq-local joplin--metadata-start (point-marker))
            (let ((start (point)))
              (insert "\n\n# ──────────── Metadata ────────────\n")
              (insert (format "## %s\n" full-path))
              (when (and author (not (string-empty-p author)))
                (insert (format "- Author: %s\n" author)))
              (when created
                (insert (format "- Created: %s\n" created)))
              (when updated
                (insert (format "- Updated: %s\n" updated)))
              (when tags
                (insert "\n## Tags\n")
                (insert (format "Tags: %s\n"
                                (mapconcat #'JTAG-title tags ", "))))
              (add-text-properties start (point-max)
                                   '(read-only t front-sticky (read-only)
                                     joplin-metadata t face shadow)))))))
    (set-buffer-modified-p nil)
    ;; Fetch backlinks asynchronously — they'll be appended when ready
    (joplin--fetch-backlinks-async))

  (defun joplin--clamp-to-before-metadata (&rest _)
    "If point is inside the metadata section, move it just before it."
    (when (and (bound-and-true-p joplin-note)
               (markerp joplin--metadata-start)
               (marker-position joplin--metadata-start)
               (>= (point) joplin--metadata-start))
      (goto-char joplin--metadata-start)
        (forward-line -1)
        (end-of-line)))

  (advice-add 'end-of-buffer :after #'joplin--clamp-to-before-metadata)

  (defun joplin-mark-whole-buffer ()
    "Select only the editable part of the buffer, excluding metadata."
    (interactive)
    (push-mark (point-min) nil t)
    (if (and (bound-and-true-p joplin-note)
             (markerp joplin--metadata-start)
             (marker-position joplin--metadata-start))
        (goto-char joplin--metadata-start)
      (goto-char (point-max))))

  ;; Metadata section removal/restoration during save:
  ;; - before-save-hook (-100): sets `joplin--saving-via-hook', removes metadata
  ;; - write-file-functions: `joplin-save-note' (with :before/:after advice)
  ;;   The :after advice skips metadata insertion when the flag is set,
  ;;   avoiding read-only text in the buffer during basic-save-buffer processing
  ;; - after-save-hook (100): re-inserts metadata, clears flag
  ;; For direct `joplin-save-note' calls (C-c j s), the flag is nil so
  ;; the :after advice handles metadata insertion directly.

  (defun joplin-follow-link-at-point ()
    "Follow link at point: open Joplin notes in Emacs, others externally."
    (interactive)
    (if-let ((url (markdown-link-url)))
        (if (string-prefix-p ":/" url)
            (switch-to-buffer (joplin--note-buffer (substring url 2)))
          (browse-url url))
      (user-error "No link at point")))

  (defun joplin--all-notes ()
    "Fetch all Joplin notes as an alist of (title . id)."
    (let ((page 0)
          (has-more t)
          all-notes)
      (while has-more
        (cl-incf page)
        (let* ((resp (joplin--http-get "/notes"
                                       `((page . ,page)
                                         (limit . 100)
                                         (fields . "id,title")
                                         (order_by . "user_updated_time")
                                         (order_dir . "desc"))))
               (items (alist-get 'items resp)))
          (cl-loop for item across items
                   do (push (cons (alist-get 'title item)
                                  (alist-get 'id item))
                            all-notes))
          (setq has-more (not (eq (alist-get 'has_more resp) :json-false)))))
      (nreverse all-notes)))

  (defun joplin--read-note (prompt)
    "Select a Joplin note with PROMPT via completing-read, return its id."
    (let* ((candidates (joplin--all-notes))
           (sel (completing-read prompt candidates nil t)))
      (cdr (assoc sel candidates))))

  (defun joplin-insert-link ()
    "Search for a Joplin note and insert a link at point."
    (interactive)
    (unless (bound-and-true-p joplin-note-mode)
      (user-error "Not in a Joplin note buffer"))
    (joplin--init)
    (let ((id (joplin--read-note "Search notes: ")))
      (when id
        (let* ((candidates (joplin--search-notes ""))
               (title (car (rassoc id candidates))))
          (insert (format "[%s](:/%s)" (or title "Untitled") id))))))

  (defun joplin-find-note ()
    "Search for a Joplin note and open it."
    (interactive)
    (joplin--init)
    (let ((id (joplin--read-note "Search notes: ")))
      (when id
        (switch-to-buffer (joplin--note-buffer id)))))

  (transient-define-prefix joplin-transient-menu ()
    "Joplin"
    ["Navigate"
     ("m" "Notebooks"       joplin)
     ("f" "Find note"       joplin-find-note)
     ("s" "Search"          joplin-search)
     ("j" "Journal"         joplin-journal)]
    ["Create"
     ("n" "New note"        joplin-create-note)
     ("b" "New notebook"    joplin-create-notebook)]
    ["Delete"
     ("d" "Delete note"     joplin-delete-note-dwim)
     ("D" "Delete notebook" joplin-delete-notebook-dwim)]
    ["Note actions" :if (lambda () (bound-and-true-p joplin-note-mode))
     ("l" "Insert link"     joplin-insert-link)
     ("B" "Backlinks"       joplin-backlinks)
     ("o" "Follow link"     joplin-follow-link-at-point)])

  ;; Joplin view keybindings
  (define-key joplin-mode-map [?c] #'joplin-create-note)
  (define-key joplin-mode-map [?C] #'joplin-create-notebook)
  (define-key joplin-mode-map [?D] #'joplin-delete-notebook-dwim)
  (define-key joplin-search-mode-map [?c] #'joplin-create-note)
  (define-key joplin-search-mode-map [?C] #'joplin-create-notebook)
  (define-key joplin-search-mode-map [?D] #'joplin-delete-note-dwim)
  (define-key joplin-note-mode-map [(meta ?o)] #'joplin-follow-link-at-point)
  (define-key joplin-note-mode-map (kbd "C-x C-s") #'joplin-save-note)
  (define-key joplin-note-mode-map (kbd "C-x h") #'joplin-mark-whole-buffer)
  (define-key joplin-note-mode-map (kbd "C-c j") #'joplin-transient-menu)
  )

(load-file "~/.emacs.d/custom_packages/structured-log-mode.el")

(use-package stripspace
  :ensure t

  ;; Enable for prog-mode-hook, text-mode-hook, conf-mode-hook
  :hook ((prog-mode . stripspace-local-mode)
         (text-mode . stripspace-local-mode)
         (conf-mode . stripspace-local-mode))

  :custom
  ;; The `stripspace-only-if-initially-clean' option:
  ;; - nil to always delete trailing whitespace.
  ;; - Non-nil to only delete whitespace when the buffer is clean initially.
  ;; (The initial cleanliness check is performed when `stripspace-local-mode'
  ;; is enabled.)
  (stripspace-only-if-initially-clean t)

  ;; Enabling `stripspace-restore-column' preserves the cursor's column position
  ;; even after stripping spaces. This is useful in scenarios where you add
  ;; extra spaces and then save the file. Although the spaces are removed in the
  ;; saved file, the cursor remains in the same position, ensuring a consistent
  ;; editing experience without affecting cursor placement.
  (stripspace-restore-column t))

(use-package jira
    :vc (:url "https://github.com/unmonoqueteclea/jira.el" :rev :newest)
    :config
    (defun jira/detail-find-issue-by-key ()
      (interactive)
      (jira-detail-find-issue-by-key))

    (global-set-key (kbd "M-s J") #'jira/detail-find-issue-by-key)
    (global-set-key (kbd "M-s M-J") #'jira/detail-find-issue-by-key)

    (setq jira-detail-show-announcements nil
          jira-token-is-personal-access-token nil
          jira-api-version 3
          jira-issues-max-results 200
          jira-issues-sort-key (cons "Status" t)
          jira-issues-table-fields '(:key :issue-type-name :status-name :assignee-name :summary)
          jira-issues-fields '((:key (:path key) (:columns . 12) (:name . "Key") (:formatter . jira-fmt-issue-key))
                               (:priority-name (:path fields priority name) (:columns . 10) (:name . "Priority"))
                               (:priority-icon. ((:path fields priority iconUrl) (:columns . 10) (:name . "Priority")))
                               (:labels (:path fields labels) (:columns . 10) (:name . "Labels"))
                               (:original-estimate (:path fields aggregatetimeoriginalestimate) (:columns . 10) (:name . "Estimate") (:formatter . jira-fmt-time-from-secs))
                               (:work-ratio (:path fields workratio) (:columns . 6) (:name . "WR") (:formatter . jira-fmt-issue-progress))
                               (:remaining-time (:path fields timeestimate) (:columns . 10) (:name . "Remaining") (:formatter . jira-fmt-time-from-secs))
                               (:assignee-name (:path fields assignee displayName) (:columns . 18) (:name . "Assignee"))
                               (:reporter-name (:path fields reporter displayName) (:columns . 14) (:name . "Reporter"))
                               (:components (:path fields components) (:columns . 10) (:name . "Components") (:formatter . jira-fmt-issue-components))
                               (:fix-versions (:path fields fixVersions) (:columns . 10) (:name . "Fix Versions") (:formatter . jira-fmt-issue-fix-versions))
                               (:status-name (:path fields status) (:columns . 22) (:name . "Status") (:formatter . jira-fmt-issue-status))
                               (:status-category-name (:path fields status statusCategory name) (:columns . 10) (:name . "Status Category"))
                               (:creator-name (:path (fields creator displayName)) (:columns . 10) (:name . "Creator"))
                               (:progress-percent (:path fields progress percent) (:columns . 10) (:name . "Progress") (:formatter . jira-fmt-issue-progress))
                               (:issue-type-name (:path fields issuetype name) (:columns . 5) (:name . "Type") (:formatter . jira-fmt-issue-type-name))
                               (:issue-type-id (:path fields issuetype id) (:columns . 15) (:name . "Type"))
                               (:issue-type-icon (:path fields issuetype iconUrl) (:columns . 10) (:name . "Type"))
                               (:project-key (:path fields project key) (:columns . 10) (:name . "Project"))
                               (:project-name (:path fields project name) (:columns . 10) (:name . "Project"))
                               (:parent-type-name (:path fields parent fields issuetype name) (:columns . 10) (:name . "Parent Type") (:formatter . jira-fmt-issue-type-name))
                               (:parent-status (:path fields parent fields status) (:columns . 10) (:name . "Parent Status") (:formatter . jira-fmt-issue-status))
                               (:parent-key (:path fields parent key) (:columns . 10) (:name . "Parent Key") (:formatter . jira-fmt-issue-key))
                               (:parent-summary (:path fields parent fields summary) (:columns . 40) (:name . "Parent Summary"))
                               (:created (:path fields created) (:columns . 10) (:name . "Created") (:formatter . jira-fmt-datetime))
                               (:updated (:path fields updated) (:columns . 10) (:name . "Updated") (:formatter . jira-fmt-datetime))
                               (:description (:path fields description) (:columns . 10) (:name . "Description"))
                               (:summary (:path fields summary) (:columns . 10) (:name . "Summary"))
                               (:due-date (:path fields duedate) (:columns . 10) (:name . "Due Date") (:formatter . jira-fmt-date))
                               (:sprints (:path fields (custom "Sprint")) (:columns . 10) (:name . "Sprints") (:formatter . jira-fmt-issue-sprints))
                               (:line (:path fields (custom "Business line")) (:columns . 10) (:name . "Business Line") (:formatter . jira-fmt-business-line))
                               (:cost-center (:path fields (custom "Cost center")) (:columns . 10) (:name . "Const Center") (:formatter . jira-fmt-cost-center))
                               (:resolution (:path fields resolution name) (:columns . 10) (:name . "Resolution"))
                               (:issuelinks (:path fields issuelinks) (:columns . 15) (:name . "Linked Issues") (:formatter . jira-fmt-issuelinks))))

    (defun jira/attachment-id-by-name (issue filename)
      "Find the attachment content ID for FILENAME in ISSUE data."
      (let ((attachments (append (alist-get 'attachment (alist-get 'fields issue)) nil)))
        (cl-loop for att in attachments
                 when (string= (alist-get 'filename att) filename)
                 return (file-name-nondirectory
                         (url-filename
                          (url-generic-parse-url (alist-get 'content att)))))))

    (defvar jira/auto-inline-images t
      "When non-nil, automatically fetch and inline images when opening a Jira issue.")

    (defun jira/inline-image-parser ()
      "Parser for binary image data from Jira API."
      (set-buffer-multibyte nil)
      (buffer-string))

    (defun jira/inline-image-callback (key placeholder fname counter data _response)
      "Handle async image fetch result.
KEY is the issue key, PLACEHOLDER the text to replace, FNAME the
filename, COUNTER a cons cell (done . total) for tracking progress."
      (when-let* ((buf (jira-detail--get-issue-buffer key)))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (save-excursion
              (goto-char (point-min))
              (when (search-forward placeholder nil t)
                (replace-match "")
                (insert-image
                 (create-image data nil t
                               :max-width (min 1000 (- (window-pixel-width) 50)))
                 (format "[%s]" fname))
                (insert "\n")))))))

    (defun jira/inline-images ()
      "Replace <file:...> image placeholders with inline images in jira-detail buffer."
      (interactive)
      (unless jira-detail--current
        (user-error "Not in a jira-detail buffer"))
      (let ((issue jira-detail--current)
            (key jira-detail--current-key)
            (total 0)
            (counter (cons 0 0)))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward
                  "<file:\\(?:[^:>]+:\\)?\\([^>]+\\.\\(?:png\\|jpe?g\\|gif\\|webp\\)\\)>"
                  nil t)
            (let* ((filename (match-string-no-properties 1))
                   (placeholder (match-string-no-properties 0))
                   (id (jira/attachment-id-by-name issue filename)))
              (when id
                (cl-incf total)
                (jira-api-call
                 "GET" (format "attachment/content/%s" id)
                 :parser #'jira/inline-image-parser
                 :callback (apply-partially #'jira/inline-image-callback
                                            key placeholder filename counter))))))
        (setcdr counter total)
        (if (> total 0)
            (message "Fetching %d image(s)..." total)
          (message "No image placeholders found"))))

    (defun jira/toggle-auto-inline-images ()
      "Toggle automatic image inlining for Jira issues."
      (interactive)
      (setq jira/auto-inline-images (not jira/auto-inline-images))
      (message "Jira auto inline images: %s" (if jira/auto-inline-images "ON" "OFF")))

    (defun jira/copy-issue-link ()
      "Copy the Jira URL of the issue at point to the kill ring."
      (interactive)
      (if-let* ((key (jira-utils-marked-item))
                (url (format "%s/browse/%s" (jira-api--get-current-url) key)))
          (progn (kill-new url)
                 (message "Copied: %s" url))
        (user-error "No issue at point")))

    (defun jira/browse-url-at-point-or-issue (issue-key)
      "Open the link at point in a browser, or fall back to opening ISSUE-KEY."
      (if-let* ((btn (button-at (point)))
                (url (or (button-get btn 'href)
                         (let ((data (button-get btn 'button-data)))
                           (and (stringp data) (string-match-p "\\`https?://" data) data)))))
          (browse-url url)
        (jira-actions-open-issue issue-key)))

    (defun jira/inhibit-ret-on-buttons (orig-fun &rest args)
      "In jira modes, make RET run the mode binding instead of following buttons."
      (if (and (memq last-command-event '(13 return))
               (derived-mode-p 'jira-issues-mode 'jira-detail-mode))
          (when-let* ((cmd (lookup-key (current-local-map) (kbd "RET"))))
            (call-interactively cmd))
        (apply orig-fun args)))
    (advice-add 'push-button :around #'jira/inhibit-ret-on-buttons)

    (defun jira/list-jql (jql)
      "Ouvre la vue jira-issues avec le filtre JQL donné."
      (interactive "sJQL: ")
      (oset (get 'jira-issues-menu 'transient--prefix)
            value (list (concat "--jql=" jql)))
      (jira-issues))

    (defun jira/org--fetch-issues-handler (cb data _response)
      "Handler pour le retour API. Appelle CB avec les issues extraites de DATA."
      (funcall cb (append (alist-get 'issues data) nil)))

    (defun jira/org--fetch-issues (jql cb &optional max-results)
      "Récupère les issues Jira correspondant à JQL de manière asynchrone.
    Appelle CB avec la liste des issues une fois reçues."
      (require 'jira-api)
      (let* ((max-results (or max-results 50))
             (params `(("jql" . ,jql)
                       ("maxResults" . ,max-results)
                       ("fields" . "key,status,summary,issuetype,assignee"))))
        (jira-api-search
         :params params
         :callback (apply-partially #'jira/org--fetch-issues-handler cb))))

    (defun jira/org--insert-issues (issues)
      "Insère les ISSUES formatées avec faces au point actuel."
      (require 'jira-fmt)
      ;; Header
      (insert (propertize (format "%-12s" "Id") 'face 'bold))
      (insert "  ")
      (insert (propertize (format "%-22s" "Status") 'face 'bold))
      (insert "  ")
      (insert (propertize "Titre" 'face 'bold))
      (insert "\n")
      ;; Séparateur
      (insert (make-string 80 ?─))
      (insert "\n")
      ;; Issues groupées par status
      (let ((last-status nil))
        (dolist (issue issues)
          (let* ((key (alist-get 'key issue))
                 (fields (alist-get 'fields issue))
                 (status (alist-get 'status fields))
                 (status-name (alist-get 'name status))
                 (status-fmt (jira-fmt-issue-status status))
                 (summary (or (alist-get 'summary fields) "")))
            ;; Ligne vide entre groupes de status différents
            (when (and last-status (not (equal last-status status-name)))
              (insert "\n"))
            (setq last-status status-name)
            (insert key)
            (insert (make-string (max 0 (- 12 (length key))) ? ))
            (insert "  ")
            (insert (substring-no-properties (or status-fmt "")))
            (insert (make-string (max 0 (- 22 (length (substring-no-properties (or status-fmt ""))))) ? ))
            (insert "  ")
            (insert summary)
            (insert "\n"))))
      (when issues (delete-char -1)))

    (defun jira/org--refresh-block-callback (buf content-start content-end issues)
      "Callback pour jira/org-refresh-block.
    Insère ISSUES entre CONTENT-START et CONTENT-END dans BUF."
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (save-excursion
            (delete-region content-start content-end)
            (goto-char content-start)
            (if issues
                (progn
                  (jira/org--insert-issues issues)
                  (insert "\n"))
              (insert "(Aucune issue)\n"))
            (jira/org-fontify-buffer)
            (message "Bloc Jira rafraîchi")))))

    (defun jira/org-refresh-block ()
      "Rafraîchit le bloc Jira sous le curseur ou dans la section courante."
      (interactive)
      (save-excursion
        (let ((case-fold-search t))
          ;; Chercher le marqueur BEGIN dans la section courante
          (unless (looking-at "^#\\+JIRA_BEGIN:")
            (if (re-search-backward "^#\\+JIRA_BEGIN:" nil t)
                (goto-char (match-beginning 0))
              (user-error "Aucun bloc Jira trouvé")))
          (let* ((jql (progn
                        (looking-at "^#\\+JIRA_BEGIN: \\(.*\\)$")
                        (match-string 1)))
                 (content-start (progn (forward-line 1) (point)))
                 (content-end (progn
                                (if (re-search-forward "^#\\+JIRA_END" nil t)
                                    (line-beginning-position)
                                  (user-error "Marqueur JIRA_END manquant")))))
            (message "Chargement Jira...")
            (jira/org--fetch-issues
             jql
             (apply-partially #'jira/org--refresh-block-callback
                              (current-buffer) content-start content-end))))))

    (defun jira/org-refresh-buffer ()
      "Rafraîchit tous les blocs Jira du buffer."
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^#\\+JIRA_BEGIN:" nil t)
          (goto-char (match-beginning 0))
          (jira/org-refresh-block)
          (forward-line 1))))

    (defun jira/org--insert-block-callback (buf content-start issues)
      "Callback pour jira/org-insert-block.
    Remplace le placeholder dans BUF à CONTENT-START avec ISSUES."
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (save-excursion
            (goto-char content-start)
            (delete-region content-start
                          (progn (re-search-forward "^#\\+JIRA_END")
                                 (line-beginning-position)))
            (if issues
                (progn
                  (jira/org--insert-issues issues)
                  (insert "\n"))
              (insert "(Aucune issue)\n"))
            (jira/org-fontify-buffer)))))

    (defun jira/org-insert-block (jql)
      "Insère un nouveau bloc Jira avec JQL."
      (interactive "sJQL: ")
      (insert (format "#+JIRA_BEGIN: %s\n" jql))
      (let ((content-start (point)))
        (insert "(Chargement...)\n#+JIRA_END\n")
        (jira/org--fetch-issues
         jql
         (apply-partially #'jira/org--insert-block-callback
                          (current-buffer) content-start))))

    ;; Liste des statuts connus avec leur catégorie pour la fontification
    (defvar jira/org-status-faces
      '(;; To Do
        ("À analyser" . jira-face-status-todo)
        ("To Do" . jira-face-status-todo)
        ("Backlog" . jira-face-status-todo)
        ("Open" . jira-face-status-todo)
        ("Reopened" . jira-face-status-todo)
        ("READY TO DO" . jira-face-status-todo)
        ;; In Progress
        ("In Progress" . jira-face-status-inprogress)
        ("En cours" . jira-face-status-inprogress)
        ("In Review" . jira-face-status-inprogress)
        ("MERGE REQUEST REVIEW" . jira-face-status-inprogress)
        ("En revue" . jira-face-status-inprogress)
        ("À valider" . jira-face-status-inprogress)
        ("En attente" . jira-face-status-inprogress)
        ;; Done
        ("Done" . jira-face-status-done)
        ("Terminé" . jira-face-status-done)
        ("Closed" . jira-face-status-done)
        ("Resolved" . jira-face-status-done)
        ("DEPLOYED" . jira-face-status-done)
        ("Validé" . jira-face-status-done))
      "Alist mapping status names to their faces.")

    (defun jira/org-clear-overlays ()
      "Supprime tous les overlays Jira du buffer."
      (remove-overlays (point-min) (point-max) 'jira-overlay t))

    (defvar jira/org--status-regex nil
      "Cached regex matching all known Jira statuses.")

    (defun jira/org--status-regex ()
      "Return a regex matching all known statuses, built once and cached."
      (or jira/org--status-regex
          (setq jira/org--status-regex
                (concat "  \\("
                        (mapconcat (lambda (sf) (regexp-quote (car sf)))
                                   jira/org-status-faces "\\|")
                        "\\)  "))))

    (defun jira/org-fontify-block (start end)
      "Fontifie un bloc Jira entre START et END avec des overlays."
      (save-excursion
        ;; Overlay de base : neutralise l'emphasis org dans tout le bloc
        (let ((base-ov (make-overlay start end)))
          (overlay-put base-ov 'jira-overlay t)
          (overlay-put base-ov 'face 'default)
          (overlay-put base-ov 'priority -10))
        (goto-char start)
        ;; Fontifier les IDs Jira (pattern: PROJET-1234)
        (while (re-search-forward "^\\([A-Z][A-Z0-9]+-[0-9]+\\)" end t)
          (let ((ov (make-overlay (match-beginning 1) (match-end 1))))
            (overlay-put ov 'jira-overlay t)
            (overlay-put ov 'face 'jira-face-link)))
        ;; Fontifier les statuts — une seule passe avec une regex combinée
        ;; case-insensitive car jira-fmt-issue-status fait (upcase status-name)
        (goto-char start)
        (let ((case-fold-search t))
          (while (re-search-forward (jira/org--status-regex) end t)
            (let* ((status (match-string 1))
                   (face (cdr (assoc-string status jira/org-status-faces t)))
                   (ov (make-overlay (match-beginning 1) (match-end 1))))
              (overlay-put ov 'jira-overlay t)
              (overlay-put ov 'face face))))))

    (defun jira/org-fontify-buffer ()
      "Fontifie tous les blocs Jira du buffer."
      (interactive)
      (require 'jira-fmt)
      (jira/org-clear-overlays)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^#\\+JIRA_BEGIN:" nil t)
          (let ((block-start (line-beginning-position 2)))
            (when (re-search-forward "^#\\+JIRA_END" nil t)
              (let ((block-end (line-beginning-position)))
                (jira/org-fontify-block block-start block-end)))))))

    (defun jira/org-maybe-fontify ()
      "Fontifie les blocs Jira si jira-fmt est chargé."
      (when (featurep 'jira-fmt)
        (jira/org-fontify-buffer)))

    (defun jira/init-org-buffer ()
      (jira/org-setup-keys)
      (add-hook 'after-save-hook #'jira/org-maybe-fontify nil t)
      ;; Fontifier après un court délai pour laisser le buffer se charger
      (let ((buf (current-buffer)))
        (run-with-idle-timer 0.5 nil
                             (lambda ()
                               (when (buffer-live-p buf)
                                 (with-current-buffer buf
                                   (jira/org-maybe-fontify)))))))

    (defun jira/org-issue-at-point ()
      "Retourne l'ID Jira sous le curseur, ou nil."
      (save-excursion
        (let ((line-start (line-beginning-position))
              (line-end (line-end-position)))
          ;; Chercher un ID Jira sur la ligne courante
          (goto-char line-start)
          (when (re-search-forward "\\([A-Z][A-Z0-9]+-[0-9]+\\)" line-end t)
            (let ((match-start (match-beginning 1))
                  (match-end (match-end 1)))
              ;; Vérifier si le curseur est sur ou avant l'ID
              (when (<= (point) match-end)
                (match-string 1)))))))

    (defun jira/org-open-issue-at-point ()
      "Ouvre l'issue Jira sous le curseur dans jira.el."
      (interactive)
      (if-let ((key (jira/org-issue-at-point)))
          (jira-detail-show-issue key)
        (user-error "Aucun ID Jira trouvé sur cette ligne")))

    (defun jira/org-RET-dwim ()
      "Ouvre l'issue Jira si sur un ID, sinon exécute l'action org par défaut."
      (interactive)
      (if (jira/org-issue-at-point)
          (jira/org-open-issue-at-point)
        (org-return)))

    (defun jira/org-setup-keys ()
      "Configure les raccourcis clavier pour les blocs Jira dans org-mode."
      (local-set-key (kbd "<return>") #'jira/org-RET-dwim))

    ;; Hook pour fontifier à l'ouverture des fichiers Org
    (add-hook 'org-mode-hook 'jira/init-org-buffer)

    (with-eval-after-load 'jira-detail
      (defun jira/remove-section-highlight-bg ()
        "Remove background from magit-section-highlight in Jira detail buffers."
        (face-remap-add-relative 'magit-section-highlight :background (face-background 'default)))
      (add-hook 'jira-detail-mode-hook #'jira/remove-section-highlight-bg)
      (define-key jira-detail-mode-map (kbd "I") #'jira/toggle-auto-inline-images)
      (define-key jira-detail-mode-map (kbd "c") #'jira/copy-issue-link)
      (defun jira/browse-issue-at-point ()
        "Open link at point or issue in browser."
        (interactive)
        (jira/browse-url-at-point-or-issue jira-detail--current-key))
      (define-key jira-detail-mode-map (kbd "M-o") #'jira/browse-issue-at-point)
      (defun jira/auto-inline-images-after (key &rest _)
        "After-advice to auto-inline images in Jira issue buffers."
        (when-let* ((buf (jira-detail--get-issue-buffer key)))
          (with-current-buffer buf
            (when jira/auto-inline-images
              (jira/inline-images)))))
      (advice-add 'jira-detail--issue :after #'jira/auto-inline-images-after)))

    (with-eval-after-load 'jira-issues
      (define-key jira-issues-mode-map (kbd "c") #'jira/copy-issue-link)
      (defun jira/browse-marked-issue ()
        "Open link at point or marked issue in browser."
        (interactive)
        (jira/browse-url-at-point-or-issue (jira-utils-marked-item)))
      (define-key jira-issues-mode-map (kbd "M-o") #'jira/browse-marked-issue))

(use-package nix-mode
  :mode "\\.nix\\'")

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

(electric-indent-mode 0)
(defun electric-indent/inhibit ()
  (interactive)
  (setq-local electric-indent-inhibit t))

(use-package ilist
  :vc (:url "https://github.com/emacs-straight/ilist" :rev :newest)
  :defer t)

(use-package transient
  :ensure t
  :defer t
  :config
  (defun transient/display-buffer (buffer alist)
    "Display transient BUFFER below the selected window.
Falls back to a bottom side-window when splitting is not possible."
    (or (display-buffer-below-selected buffer alist)
        (display-buffer-in-side-window buffer
                                       (append alist '((side . bottom))))))
  (setq transient-display-buffer-action
        '(transient/display-buffer
          (dedicated . t)
          (inhibit-same-window . t)
          (window-height . fit-window-to-buffer)
          (preserve-size . (nil . t))
          (window-parameters (no-other-window . t)))))

(use-package magit
  :defer t
  :commands (magit-status magit-clone magit-blame)
  :bind (("C-x g g" . magit-status)
         ("C-x g c" . magit-clone))
  :config
  (require 'ilist)

  (defun magit/display-buffer (buffer)
    "Display magit BUFFER: status in same window, others in a magit window below."
    (if (with-current-buffer buffer
          (derived-mode-p 'magit-status-mode))
        (display-buffer buffer '(display-buffer-same-window))
      (let ((magit-win (cl-find-if
                        (lambda (w)
                          (with-current-buffer (window-buffer w)
                            (derived-mode-p 'magit-mode)))
                        (cdr (window-list)))))
        (if magit-win
            (window--display-buffer buffer magit-win 'reuse)
          (display-buffer buffer
                          '(display-buffer-in-direction
                            (direction . below)
                            (window-height . 0.6)))))))
  (setq magit-display-buffer-function #'magit/display-buffer)

  (defun magit/jira-key-from-branch ()
    "Extract a Jira issue key (e.g. PROJ-123) from the current branch name."
    (when-let* ((branch (magit-get-current-branch)))
      (when (string-match "\\b\\([A-Z][A-Z0-9]+-[0-9]+\\)" branch)
        (match-string 1 branch))))

  (defvar-local magit/jira-issue-cache nil
    "Cached Jira issue data for the current magit-status buffer.")

  (defvar-local magit/jira-issue-cache-branch nil
    "Branch name associated with `magit/jira-issue-cache'.")

  (defvar-local magit/jira-image-cache nil
    "Hash table mapping filenames to binary image data for the Jira section.")

  (defun magit/jira-fetch-issue-callback (buf branch data _response)
    "Handle async Jira fetch result.
Store DATA in cache for BRANCH in BUF, then refresh magit."
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq magit/jira-issue-cache data)
        (setq magit/jira-issue-cache-branch branch)
        (magit-refresh))))

  (defun magit/jira-fetch-issue-async (key buf branch)
    "Fetch Jira issue KEY asynchronously.
When done, store result in BUF's cache and refresh magit."
    (jira-api-call
     "GET" (concat "issue/" key)
     :callback (apply-partially #'magit/jira-fetch-issue-callback buf branch)))

  (defun magit/jira-inline-image-callback (buf placeholder fname data _response)
    "Handle async image fetch for magit jira section.
Store DATA in `magit/jira-image-cache' and replace PLACEHOLDER in BUF."
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when magit/jira-image-cache
          (puthash fname data magit/jira-image-cache))
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-min))
            (when (search-forward placeholder nil t)
              (replace-match "")
              (insert-image
               (create-image data nil t
                             :max-width (min 600 (- (window-pixel-width) 50)))
               (format "[%s]" fname))
              (insert "\n")))))))

  (defun magit/jira-inline-images (issue buf)
    "Inline image placeholders in magit buffer BUF for ISSUE.
Uses `magit/jira-image-cache' for already-fetched images."
    (with-current-buffer buf
      (unless magit/jira-image-cache
        (setq magit/jira-image-cache (make-hash-table :test 'equal)))
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward
                  "<file:\\(?:[^:>]+:\\)?\\([^>]+\\.\\(?:png\\|jpe?g\\|gif\\|webp\\)\\)>"
                  nil t)
            (let* ((filename (match-string-no-properties 1))
                   (placeholder (match-string-no-properties 0))
                   (cached (gethash filename magit/jira-image-cache)))
              (if cached
                  (progn
                    (replace-match "")
                    (insert-image
                     (create-image cached nil t
                                   :max-width (min 600 (- (window-pixel-width) 50)))
                     (format "[%s]" filename))
                    (insert "\n"))
                (when-let* ((id (jira/attachment-id-by-name issue filename)))
                  (jira-api-call
                   "GET" (format "attachment/content/%s" id)
                   :parser #'jira/inline-image-parser
                   :callback (apply-partially #'magit/jira-inline-image-callback
                                              buf placeholder filename))))))))))

  (defun magit/jira--insert-issue-section (key issue)
    "Insert the magit section content for Jira ISSUE with KEY."
    (let* ((fields (alist-get 'fields issue))
           (summary (alist-get 'summary fields))
           (status (alist-get 'name (alist-get 'status fields)))
           (type (alist-get 'name (alist-get 'issuetype fields)))
           (assignee (or (alist-get 'displayName (alist-get 'assignee fields)) "Unassigned"))
           (priority (alist-get 'name (alist-get 'priority fields)))
           (components (mapcar (lambda (c) (alist-get 'name c))
                               (append (alist-get 'components fields) nil)))
           (description (alist-get 'description fields)))
      (when (and issue summary)
        (magit-insert-section (jira-issue key)
          (magit-insert-heading
            (format "Jira: %s [%s] %s" key status summary))
          (insert (format "  Type:     %s\n" type))
          (insert (format "  Priority: %s\n" (or priority "None")))
          (insert (format "  Assignee: %s\n" assignee))
          (insert (format "  Status:   %s\n" status))
          (when components
            (insert (format "  Components: %s\n" (string-join components ", "))))
          (when description
            (insert "\n")
            (insert (propertize "  Description\n" 'font-lock-face 'magit-section-heading))
            (insert (replace-regexp-in-string
                     "^" "    "
                     (jira-doc-format description))
                    "\n"))
          (insert "\n"))))
    (when jira/auto-inline-images
      (magit/jira-inline-images issue (current-buffer))))

  (defun magit/insert-jira-issue ()
    "Insert a Magit section showing the Jira issue associated with the branch."
    (when-let* ((key (magit/jira-key-from-branch)))
      (require 'jira-api nil t)
      (require 'jira-doc nil t)
      (when (fboundp 'jira-api-call)
        (condition-case nil
            (let* ((current-branch (magit-get-current-branch))
                   (_cache-check (unless (equal current-branch magit/jira-issue-cache-branch)
                                   (setq magit/jira-issue-cache nil
                                         magit/jira-issue-cache-branch current-branch
                                         magit/jira-image-cache nil)))
                   (issue magit/jira-issue-cache))
              (if issue
                  ;; Cache hit: insert section normally
                  (magit/jira--insert-issue-section key issue)
                ;; Cache miss: show placeholder and fetch async
                (magit-insert-section (jira-issue key)
                  (magit-insert-heading
                    (format "Jira: %s [Loading...]" key))
                  (insert "\n"))
                (magit/jira-fetch-issue-async key (current-buffer) current-branch)))
          (error nil)))))

  (defun jira/open-issue-at-section ()
    "Open Jira issue at point in magit-status."
    (interactive)
    (when-let* ((key (magit-section-value-if 'jira-issue)))
      (jira-detail-show-issue key)))
  (defvar-keymap jira-issue-section-map
    :doc "Keymap for the Jira issue section in magit-status."
    "RET" #'jira/open-issue-at-section)

  (defclass jira-issue (magit-section)
    ((keymap :initform 'jira-issue-section-map)))

  (magit-add-section-hook 'magit-status-sections-hook
                          #'magit/insert-jira-issue
                          'forge-insert-issues 'append))

(use-package forge
  :after magit
  :config
  (global-set-key (kbd "M-o") #'forge-browse)
  (defun forge--format-topic-title (topic)
    (with-temp-buffer
      (save-excursion
        (with-slots (title status state) topic
          (insert
           (magit--propertize-face
            title
            `(,@(and (forge-pullreq-p topic)
                     (oref topic draft-p)
                     '(forge-pullreq-draft))
              ,(pcase status
                 ('unread  'forge-topic-unread)
                 ('pending 'forge-topic-pending)
                 ('done    'forge-topic-done))
              ,(pcase (list (eieio-object-class topic) state)
                 (`(forge-issue   open)      'forge-issue-open)
                 (`(forge-issue   completed) 'forge-issue-completed)
                 (`(forge-issue   unplanned) 'forge-issue-unplanned)
                 (`(forge-pullreq open)      'forge-pullreq-open)
                 (`(forge-pullreq merged)    'forge-pullreq-merged)
                 (`(forge-pullreq rejected)  'forge-pullreq-rejected)))))))
      (run-hook-wrapped 'forge-topic-wash-title-hook
                        (lambda (fn) (prog1 nil (save-excursion (funcall fn)))))
      (buffer-string))))

(use-package yasnippet
  :defer 2
  :bind (:map yas-keymap
              ("C-<right>" . yas-next-field)
              ("C-<left>" . yas-prev-field)
              ("<tab>" . nil)
              ("TAB" . nil))
  :config
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'text-mode-hook #'yas-minor-mode))

(with-eval-after-load 'company
  (with-eval-after-load 'yasnippet
    ;; Make RET only complete Company when popup is visible, not exit yas field
    (define-key company-active-map (kbd "RET") nil)
    (define-key company-active-map (kbd "<return>") nil)
    ;; Use C-y as alternative to complete selection without affecting yas
    (define-key company-active-map (kbd "C-y") #'company-complete-selection)

    (defun my/yas-company-complete-and-stay (fn &rest args)
      "Complete company selection and update yasnippet mirrors."
      (let ((field (and (bound-and-true-p yas-minor-mode)
                        (ignore-errors (yas-current-field)))))
        (if field
            (let ((yas--inhibit-overlay-hooks t))
              (apply fn args)
              ;; Force mirror update after Company insertion
              (let ((snippet (yas--field-snippet field)))
                (when snippet
                  (yas--update-mirrors snippet))))
          (apply fn args))))
    (advice-add 'company-complete-selection :around #'my/yas-company-complete-and-stay)))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package jwt
  :vc (:url "https://github.com/joshbax189/jwt-el" :rev :newest))

(use-package nodejs-repl
  :config
  (defun nodejs-repl/remove-broken-filter ()
    (remove-hook 'comint-output-filter-functions 'nodejs-repl--delete-prompt t))
  (add-hook 'nodejs-repl-mode-hook #'nodejs-repl/remove-broken-filter))

(use-package typescript-mode
  :mode "\\.ts\\'")

(use-package vue-mode
  :mode "\\.vue\\'")

(use-package php-mode)

(use-package sql
  :ensure nil
  :config
  ;; Give password to PG sql connection
  (defun sql/inject-pgpassword (orig-fun product options &rest args)
    "Around-advice to inject PGPASSWORD env var for PostgreSQL connections."
    (setenv "PGPASSWORD" sql-password)
    (unwind-protect
        (apply orig-fun product options args)
      (setenv "PGPASSWORD" nil)))
  (advice-add 'sql-comint-postgres :around #'sql/inject-pgpassword))

(use-package sqlup-mode
  :config
  (add-hook 'sql-mode-hook 'sqlup-mode))

(use-package ejc-sql
  :defer t
  :config
  (defun ejc/sql-mode-setup ()
    (ejc-eldoc-setup)
    (setq-local company-backends
                (cons 'ejc-company-backend company-backends)))
  (add-hook 'ejc-sql-minor-mode-hook #'ejc/sql-mode-setup))

(use-package sqlite-mode-extras
  :vc (:url "https://github.com/xenodium/sqlite-mode-extras" :rev :newest)
  :hook ((sqlite-mode . sqlite-extras-minor-mode)))

(use-package jest-test-mode
  :commands jest-test-mode
  :hook (typescript-mode typescript-ts-mode js-mode js-ts-mode typescript-tsx-mode))

(use-package apheleia
  :config
  (push
   '(eslint
     . ("apheleia-npx" "eslint_d" "--fix-to-stdout" "--stdin" "--stdin-filename" file)) apheleia-formatters)
  (add-to-list 'apheleia-mode-alist '(js-mode . eslint))
  (add-to-list 'apheleia-mode-alist '(js-ts-mode . eslint))
  (add-to-list 'apheleia-mode-alist '(typescript-mode . eslint))
  (add-to-list 'apheleia-mode-alist '(typescript-ts-mode . eslint))
  (add-hook 'prog-mode-hook #'apheleia-mode))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package flycheck
  :defer 2
  :config
  (setq flycheck-idle-change-delay 1.0  ; Wait before checking
        flycheck-display-errors-delay 0.5)
  ;; org-lint renvoie des strings propertized au lieu de numéros de ligne,
  ;; ce qui casse flycheck (bug de compatibilité org 9.8 / flycheck)
  (setq-default flycheck-disabled-checkers '(org-lint))
  (add-hook 'prog-mode-hook #'flycheck-mode))

(use-package flycheck-title
  :after flycheck
  :config
  (flycheck-title-mode))

(setenv "LSP_USE_PLISTS" "true")

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ((js-mode js-ts-mode typescript-ts-mode typescript-mode php-mode vue-mode) . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-completion-provider :none
        lsp-log-io nil
        lsp-idle-delay 0.5
        lsp-enable-file-watchers t
        lsp-enable-folding nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-on-type-formatting nil
        lsp-signature-auto-activate nil
        lsp-eldoc-enable-hover nil)

  (defun add-yasnippet-enable-company ()
    (setq-local company-backends '((:separate company-yasnippet company-capf)))
    (company-mode 1))
  (add-hook 'lsp-mode-hook #'add-yasnippet-enable-company)

  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-hover nil)
  (define-key lsp-ui-mode-map (kbd "C-h h") #'lsp-ui-doc-glance)
  (define-key lsp-ui-mode-map (kbd "C-h C-h") #'lsp-ui-doc-glance))

(use-package dap-mode
  :defer t
  :commands (dap-debug dap-debug-edit-template))

(use-package expreg
  :vc (:url "https://github.com/casouri/expreg" :rev :newest)
  :defer t
  :bind (("C-=" . expreg-expand)
         ("C-`" . expreg-contract)))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (run-with-idle-timer 1 nil #'exec-path-from-shell-initialize)))

(defun shell/hook ()
  (display-line-numbers-mode 0)
  (visual-line-mode 1))
(add-hook 'shell-mode-hook #'shell/hook)

(defun utils/get-project-root-if-wanted ()
  (interactive)
  (let ((cur-buffer (window-buffer (selected-window))))
    (with-current-buffer cur-buffer
      (or (and (fboundp 'project-current)
               (project-current)
               (project-root (project-current)))
          (vc-root-dir)
          (when (derived-mode-p 'dired-mode)
            (replace-regexp-in-string "^[Directory ]*" "" (pwd)))
          default-directory))))

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

(defun eshell/prompt ()
  "Custom eshell prompt with timestamp."
  (concat (format-time-string " %Y-%m-%d %H:%M" (current-time))
          (if (= (user-uid) 0) " # " " $ ")))

(defun eshell/hook ()
  (require 'eshell)
  (require 'em-smart)
  (define-key eshell-mode-map (kbd "M-m") #'eshell-bol)
  (define-key eshell-hist-mode-map (kbd "M-s") nil)

  (setq
   eshell-where-to-jump 'begin
   eshell-review-quick-commands nil
   eshell-smart-space-goes-to-end t
   eshell-prompt-function #'eshell/prompt
   eshell-highlight-prompt t)
  (set-face-attribute 'eshell-prompt nil :weight 'ultra-bold :inherit 'minibuffer-prompt)
  (eat-eshell-mode 1)
  (eat-eshell-visual-command-mode 1)
  (display-line-numbers-mode 0)
  (define-key eshell-mode-map (kbd "<tab>") #'company-complete))
(add-hook 'eshell-mode-hook #'eshell/hook)

(defun eshell/rename-with-current-path ()
  (interactive)
  (rename-buffer (concat "Eshell: " (replace-regexp-in-string "^[Directory ]*" "" (pwd))) t))
(add-hook 'eshell-directory-change-hook #'eshell/rename-with-current-path)
(add-hook 'eshell-mode-hook #'eshell/rename-with-current-path)

(defun eshell/get-relevant-buffer (path)
  (message path)
  (get-buffer (concat "Eshell: " (replace-regexp-in-string "/$" "" path))))

(defun eshell/new-or-current ()
  "Open a new instance of eshell.
If in a dired buffer, open eshell in the current directory.
Otherwise, open in the project root.
Reuses an existing eshell buffer for the target directory if one exists.
When not in frame-centric mode and not in a dired buffer, display
in a left side window.
If already focused on an eshell side window, return focus to the
main window without closing the side window."
  (interactive)
  (if (and (derived-mode-p 'eshell-mode)
           (window-parameter (selected-window) 'window-side))
      (select-window (get-mru-window nil nil t))
    (let* ((in-dired (derived-mode-p 'dired-mode))
           (default-directory (replace-regexp-in-string "/$" ""
                                                        (if in-dired
                                                            default-directory
                                                          (utils/get-project-root-if-wanted))))
           (eshell-buffer (eshell/get-relevant-buffer default-directory))
           (use-side-window (and (not frame-centric) (not in-dired) (not (featurep 'ewm))))
           (buf (or eshell-buffer
                    (let ((b (generate-new-buffer eshell-buffer-name)))
                      (with-current-buffer b (eshell-mode)) b))))
      (if use-side-window
          (pop-to-buffer buf
                         '((display-buffer-in-side-window)
                           (side . left)
                           (slot . 0)
                           (window-width . 100)
                           (preserve-size . (t . nil))))
        (switch-to-buffer buf)))))

(global-set-key (kbd "C-s-<return>") #'eshell/new-or-current)
(global-set-key (kbd "C-c <return>") #'eshell/new-or-current)
(global-set-key (kbd "C-c C-<return>") #'eshell/new-or-current)

(use-package eshell
  :ensure nil)

(use-package eat
  :vc (:url "https://codeberg.org/akib/emacs-eat" :rev :newest)
  :config
  (setq eshell-visual-commands '()
        eat-enable-blinking-text nil)

  (defun fix-char-width-for-spinners ()
    "Fix character width for common spinner/animation characters."
    (setq use-default-font-for-symbols nil)

    (set-fontset-font t 'symbol "JetBrains Mono" nil 'prepend)

    (set-fontset-font t 'emoji "JetBrains Mono" nil 'prepend)

    (set-fontset-font t '(#x2800 . #x28FF) "JetBrains Mono")
    (set-fontset-font t '(#x2500 . #x257F) "JetBrains Mono")
    (set-fontset-font t '(#x2580 . #x259F) "JetBrains Mono")
    (set-fontset-font t '(#x2190 . #x21FF) "JetBrains Mono")
    (set-fontset-font t '(#x2700 . #x27BF) "JetBrains Mono")
    (set-fontset-font t #x00B7 "JetBrains Mono")

    (set-char-table-range char-width-table '(#x2800 . #x28FF) 1)  ;; Braille
    (set-char-table-range char-width-table '(#x2500 . #x257F) 1)  ;; Box drawing
    (set-char-table-range char-width-table '(#x2580 . #x259F) 1)  ;; Block elements
    (set-char-table-range char-width-table '(#x25A0 . #x25FF) 1)  ;; Geometric shapes
    (set-char-table-range char-width-table '(#x2190 . #x21FF) 1)  ;; Arrows
    (set-char-table-range char-width-table '(#x2700 . #x27BF) 1)  ;; Dingbats (✢ ✶ ✻ ✽)
    (set-char-table-range char-width-table #x00B7 1))             ;; Middle dot (· ✢ ✶ ✻ ✽)

  (defvar vv/eat-shell (or (executable-find "nu") (getenv "SHELL") "bash"))
  (defvar vv/eat-shell-flag (if (string-match-p "nu\\|bash\\|zsh\\|sh" vv/eat-shell) "-c" "-Command"))

  (defun start-file-process-shell-command-using-eat-exec
      (name buffer command)
    (require 'eat)
    (with-current-buffer (eat-exec buffer name vv/eat-shell nil (list vv/eat-shell-flag command))
      (eat-emacs-mode)
      (setq eat--synchronize-scroll-function #'eat--synchronize-scroll)
      (get-buffer-process (current-buffer))))

  (advice-add #'compilation-start :around
              (defun hijack-start-file-process-shell-command (o &rest args)
                (advice-add #'start-file-process-shell-command :override
                            #'start-file-process-shell-command-using-eat-exec)
                (unwind-protect
                    (apply o args)
                  (advice-remove
                   #'start-file-process-shell-command
                   #'start-file-process-shell-command-using-eat-exec))))

  (add-hook #'compilation-start-hook
            (defun revert-to-eat-setup (proc)
              (set-process-filter proc #'eat--filter)
              (add-function :after (process-sentinel proc) #'eat--sentinel)))

  (advice-add #'kill-compilation :override
              (defun kill-compilation-by-sending-C-c ()
                (interactive)
                (let ((buffer (compilation-find-buffer)))
                  (if (get-buffer-process buffer)
  	                  ;; interrupt-process does not work
                      (process-send-string (get-buffer-process buffer) (kbd "C-c"))
                    (error "The %s process is not running" (downcase mode-name))))))

  (add-hook 'eat-mode-hook #'shell/hook)
  ;; Auto-scroll eat buffers even when window doesn't have focus
  (defun eat/enable-auto-scroll ()
    "Enable auto-scroll for eat terminal buffers."
    (setq-local eat--synchronize-scroll-function #'eat--synchronize-scroll))
  (add-hook 'eat-mode-hook #'eat/enable-auto-scroll)
  (with-eval-after-load 'eat
    ;; Couleurs standard (0-7)
    (set-face-foreground 'eat-term-color-0 "#282a36")  ; black
    (set-face-foreground 'eat-term-color-1 "#ff5555")  ; red
    (set-face-foreground 'eat-term-color-2 "#50fa7b")  ; green
    (set-face-foreground 'eat-term-color-3 "#f1fa8c")  ; yellow
    (set-face-foreground 'eat-term-color-4 "#bd93f9")  ; blue
    (set-face-foreground 'eat-term-color-5 "#ff79c6")  ; magenta
    (set-face-foreground 'eat-term-color-6 "#8be9fd")  ; cyan
    (set-face-foreground 'eat-term-color-7 "#f8f8f2")  ; white

    ;; Couleurs bright (8-15)
    (set-face-foreground 'eat-term-color-8 "#6272a4")   ; bright black (gris)
    (set-face-foreground 'eat-term-color-9 "#ff6e6e")   ; bright red
    (set-face-foreground 'eat-term-color-10 "#69ff94")  ; bright green
    (set-face-foreground 'eat-term-color-11 "#ffffa5")  ; bright yellow
    (set-face-foreground 'eat-term-color-12 "#d6acff")  ; bright blue
    (set-face-foreground 'eat-term-color-13 "#ff92df")  ; bright magenta
    (set-face-foreground 'eat-term-color-14 "#a4ffff")  ; bright cyan
    (set-face-foreground 'eat-term-color-15 "#ffffff")

    (setq eat-term-scrollback-size 400000
          eat--synchronize-scroll-function #'eat--synchronize-scroll)) ; bright white
  )

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

  (setq multi-term-program (if *is-windows* "powershell" "bash"))

  (add-to-list 'term-bind-key-alist '("<backtab>" . term-send-up))
  (add-to-list 'term-bind-key-alist '("TAB" . term-send-tab))
  (add-to-list 'term-bind-key-alist '("s-<escape>" . term-line-mode)))

(use-package coterm
  :defer 3
  :config
  (coterm-mode))

(unless *is-windows*
  (use-package detached
    :init
    (detached-init)
    :bind (;; Replace `async-shell-command' with `detached-shell-command'
           ([remap async-shell-command] . detached-shell-command)
           ;; Replace `compile' with `detached-compile'
           ([remap compile] . detached-compile)
           ([remap recompile] . detached-compile-recompile)
           ;; Replace built in completion of sessions with `consult'
           ([remap detached-open-session] . detached-consult-session))
    :custom ((detached-show-output-on-attach t)
             (detached-terminal-data-command system-type))))

(use-package fancy-compilation
  :commands (fancy-compilation-mode)
  :config
  (setq fancy-compilation-override-colors nil))

(with-eval-after-load 'compile
  (fancy-compilation-mode))

(defvar dired/video-extensions
    '("mp4" "mkv" "avi" "mov" "webm" "flv" "ogv" "mpg" "mpeg" "wmv" "m4v" "divx" "vob" "rmvb")
    "Video file extensions to open with EMMS.")

  (defun dired/find-file ()
    "In dired, open the file named on this line.
Video files are played with EMMS, other files are visited normally."
    (interactive)
    (let* ((file (dired-get-filename nil t))
           (ext (downcase (or (file-name-extension file) ""))))
      (if (member ext dired/video-extensions)
          (progn
            (message "Playing %s with EMMS..." file)
            (emms-play-file file))
        (dired-find-file))))

  (defun dired/open-file ()
    "In dired, open the file externally with xdg-open."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (message "Opening %s..." file)
      (if *is-windows*
          (w32-shell-execute "open" file)
        (call-process "xdg-open" nil 0 nil file))
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
  :ensure nil
  :bind (
         :map dired-mode-map
         ("C-." . dired-hide-dotfiles-mode)
         ("RET" . dired/find-file)
         ("<mouse-2>" . dired/find-file)
         ("<M-return>" . dired/open-file)
         ("M-p" . dired-up-directory)
         ("M-n" . dired/find-file)
         ("s-<escape>" . dired-toggle-read-only)
         ("M-<" . dired/first-file)
         ("M->" . dired/last-file)
         ("~" . dired/open-home-dir))
  :hook
  (dired-mode . dired-hide-details-mode)
  :config
  (setq ls-lisp-use-insert-directory-program nil)
  (require 'ls-lisp)
  (setq ls-lisp-dirs-first t
        wdired-allow-to-change-permissions t
        dired-auto-revert-buffer t)
  (defun wdired/bind-escape-abort ()
    "Bind Super+Escape to abort wdired changes."
    (define-key wdired-mode-map (kbd "s-<escape>") 'wdired-abort-changes))
  (add-hook 'wdired-mode-hook #'wdired/bind-escape-abort))

(use-package dired-hide-dotfiles
  :hook
  (dired-mode . dired-hide-dotfiles-mode))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(defun browse/url-zen-open (url &optional _ignored)
  (interactive (browse-url-interactive-arg "URL: "))
  (call-process "zen" nil 0 nil "--new-window" url))

(defun browse/url-vivaldi-open (url &optional _ignored)
  (interactive (browse-url-interactive-arg "URL: "))
  (call-process "vivaldi" nil 0 nil "--new-window" url))

(setq browse-url-browser-function
      (if *is-windows*
          'browse-url-default-windows-browser
        'browse/url-vivaldi-open))

(use-package shr
  :ensure nil
  :config
  (setq shr-use-fonts t)
  (setq shr-use-colors nil)
  (setq shr-inhibit-images nil)
  (setq shr-max-image-proportion 0.9)
  (setq shr-width nil)
  (setq shr-folding-mode t)
  (setq shr-width -1))

(use-package shr-tag-pre-highlight
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight)))

(global-set-key (kbd "M-s i") 'eww)

(use-package eww
  :ensure nil
  :bind (:map eww-mode-map
              ("M-p" . eww-back-url)
              ("M-p" . eww-next-url)
              ("C-|" . eww-browse-with-external-browser)
              ("C-c g" . eww-reload))
  :config
  (defvar eww/input-history nil)
  (eval-after-load "savehist"
    '(add-to-list 'savehist-additional-variables 'eww/input-history))

  (defun eww/do-start-with-url-or-search ()
    (interactive)
    (if (derived-mode-p 'eww-mode)
        (eww (completing-read "Eww URL or search " eww/input-history nil nil (eww-current-url) 'eww/input-history))
      (eww (completing-read "Eww URL or search " eww/input-history nil nil nil 'eww/input-history))))

  (setq eww-search-prefix "https://html.duckduckgo.com/html/?q=")
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

    (add-hook 'eww-after-render-hook 'eww/rename-buffer)
    (defun eww/enable-visual-line ()
      "Enable visual-line-mode in EWW buffers."
      (visual-line-mode 1))
    (add-hook 'eww-after-render-hook #'eww/enable-visual-line)))

(setq plstore-cache-passphrase-for-symmetric-encryption t
      auth-source-cache t
      epa-file-cache-passphrase-for-symmetric-encryption t)

(use-package auth-source-xoauth2-plugin
  :ensure t
  :custom
  (auth-source-xoauth2-plugin-mode t)
  :config
  (setq oauth2-token-file "~/gnus/oauth2.plstore"))

(setq mail-user-agent 'gnus-user-agent)
(setq read-mail-command 'gnus)
(setq gnus-select-method '(nnnil ""))
(setq gnus-use-full-window nil)
(defun gnus/display-article ()
  "Display article in window below if one exists, otherwise split."
  (let ((article-buf (gnus-get-buffer-create gnus-article-buffer))
        (below (window-in-direction 'below)))
    (when article-buf
      (if below
          (window--display-buffer article-buf below 'reuse)
        (display-buffer article-buf
                        '((display-buffer-below-selected)
                          (window-height . 0.6)))))))
(defun gnus/configure-windows (setting &optional _force)
  "Override for gnus-configure-windows with custom layout."
  (cond
   ((eq setting 'summary)
    (let ((sum-buf (gnus-get-buffer-create gnus-summary-buffer)))
      (when sum-buf
        (switch-to-buffer sum-buf))))
   ((eq setting 'article)
    (gnus/display-article))))
(advice-add 'gnus-configure-windows :override #'gnus/configure-windows)
(keymap-global-set "C-c g" #'gnus)

(setq gnus-message-archive-group nil
      gnus-permanently-visible-groups ":INBOX$"
      gnus-subscribe-newsgroup-method 'gnus-subscribe-killed
      gnus-options-subscribe "^nnimap\\+work:INBOX$")

(add-hook 'gnus-group-mode-hook #'gnus-topic-mode)

(setq gnus-thread-sort-functions '((not gnus-thread-sort-by-date))
        gnus-summary-line-format "%&user-date; %U%R%z %(%[%-23,23f%]%) %s\n"
        gnus-user-date-format-alist '(((gnus-seconds-today) . "%H:%M")
                                      ((+ 86400 (gnus-seconds-today)) . "hier %H:%M")
                                      ((* 7 86400) . "%a %H:%M")
                                      (t . "%d/%m/%Y")))

  (defun gnus/summary-quit ()
    "Kill the article buffer if visible, otherwise exit summary and sync marks."
    (interactive)
    (let ((buf (get-buffer gnus-article-buffer)))
      (if (and buf (get-buffer-window buf))
          (kill-buffer buf)
        (gnus-summary-exit))))

  (defun gnus/summary-next ()
    "Go to next article in summary.
If at the last article, fetch 200 more and then move to the next one."
    (interactive)
    (if (gnus-summary-last-article-p)
        (progn
          (gnus-summary-insert-old-articles 200)
          (gnus-summary-sort-by-date t)
          (gnus-summary-next-subject 1)
          (gnus-summary-select-article))
      (gnus-summary-next-subject 1)
      (gnus-summary-select-article)))

  (defun gnus/summary-prev ()
    "Go to previous article in summary."
    (interactive)
    (gnus-summary-next-subject -1)
    (gnus-summary-select-article))

(setq
 gnus-sorted-header-list
 '("^From:"
   "^X-RT-Originator"
   "^Newsgroups:"
   "^Subject:"
   "^Date:"
   "^Envelope-To:"
   "^Followup-To:"
   "^Reply-To:"
   "^Organization:"
   "^Summary:"
   "^Abstract:"
   "^Keywords:"
   "^To:"
   "^[BGF]?Cc:"
   "^Posted-To:"
   "^Mail-Copies-To:"
   "^Mail-Followup-To:"
   "^Apparently-To:"
   "^Resent-From:"
   "^User-Agent:"
   "^X-detected-operating-system:"
   "^X-Spam_action:"
   "^X-Spam_bar:"
   "^Message-ID:"
   ;; "^References:"
   "^List-Id:"
   "^Gnus-Warning:"))

(setq gnus-gcc-mark-as-read t
      message-confirm-send t
      message-fill-column 120
      message-forward-as-mime t
      message-send-mail-function #'smtpmail-send-it)

(setq gnus-save-newsrc-file nil
      gnus-read-newsrc-file nil
      gnus-home-directory "~/gnus/"
      gnus-directory      "~/gnus/news/"
      message-directory   "~/gnus/mail/"
      nndraft-directory   "~/gnus/drafts/"
      gnus-use-cache nil
      gnus-agent t
      gnus-agent-synchronize-flags t)

(require 'gnus)
(require 'gnus-demon)

(gnus-demon-add-handler 'gnus-demon-scan-news 5 10)

(setq doom-modeline-gnus nil
      doom-modeline-gnus-timer 0)
(defun gnus/setup-on-start ()
  "Initialize Gnus modeline and custom keybindings on start."
  (gnus-demon-init)
  (setq doom-modeline--gnus-started t
        shr-width nil)
  (keymap-set gnus-summary-mode-map "n" #'gnus/summary-next)
  (keymap-set gnus-summary-mode-map "p" #'gnus/summary-prev)
  (keymap-set gnus-summary-mode-map "q" #'gnus/summary-quit)
  (keymap-set message-mode-map "C-c C-s" nil))
(add-hook 'gnus-started-hook #'gnus/setup-on-start)

(use-package bbdb
  :config
  (bbdb-initialize 'gnus 'message)
  (bbdb-mua-auto-update-init 'gnus 'message)
  (setq bbdb-mua-auto-action nil
        bbdb-mua-auto-update-p nil
        bbdb-complete-mail-allow-cycling t
        bbdb-pop-up-window-size 0
        bbdb-mua-pop-up nil
        bbdb-ignore-message-alist
        '(("From" . "notifications@github\\.com")
          ("From" . "noreply@.*\\.github\\.com")
          ("From" . ".*noreply.*@.*")
          ("From" . "info@e\\.atlassian\\.com")))
  (defun message/add-bbdb-backend ()
    "Add BBDB company backend for email composition."
    (setq-local company-backends
                (cons 'company-bbdb company-backends)))
  (add-hook 'message-mode-hook #'message/add-bbdb-backend))

(defvar gcal/ics-url nil
    "Secret ICS URL for Google Calendar.")

  (defvar gcal/org-file "~/org/gcal.org"
    "Org file where Google Calendar events are stored.")

  (defvar gcal/future-months 3
    "Number of months into the future to expand recurring events.")

  (defun gcal/parse-ics-datetime (dt)
    "Parse an ICS datetime string DT into (SEC MIN HOUR DAY MON YEAR).
Handles YYYYMMDDTHHMMSS, YYYYMMDDTHHMMSSZ (UTC), and YYYYMMDD formats.
UTC times (ending with Z) are converted to local time."
    (when (string-match (rx (group (= 4 digit)) (group (= 2 digit)) (group (= 2 digit))
                            (? "T" (group (= 2 digit)) (group (= 2 digit)) (group (= 2 digit)))
                            (? (group "Z")))
                        dt)
      (let ((year (string-to-number (match-string 1 dt)))
            (mon (string-to-number (match-string 2 dt)))
            (day (string-to-number (match-string 3 dt)))
            (hour (if (match-string 4 dt) (string-to-number (match-string 4 dt)) 0))
            (min (if (match-string 5 dt) (string-to-number (match-string 5 dt)) 0))
            (sec (if (match-string 6 dt) (string-to-number (match-string 6 dt)) 0))
            (utc-p (match-string 7 dt)))
        (if utc-p
            (let* ((utc-time (encode-time sec min hour day mon year t))
                   (local (decode-time utc-time)))
              (list (nth 0 local) (nth 1 local) (nth 2 local)
                    (nth 3 local) (nth 4 local) (nth 5 local)))
          (list sec min hour day mon year)))))

  (defun gcal/time-to-absolute (parsed)
    "Convert PARSED time list to Emacs time."
    (encode-time (nth 0 parsed) (nth 1 parsed) (nth 2 parsed)
                 (nth 3 parsed) (nth 4 parsed) (nth 5 parsed)))

  (defun gcal/add-months (parsed n)
    "Add N months to PARSED time, adjusting day if needed."
    (let* ((year (nth 5 parsed))
           (mon (+ (nth 4 parsed) n))
           (day (nth 3 parsed)))
      (while (> mon 12) (setq mon (- mon 12) year (1+ year)))
      (while (< mon 1) (setq mon (+ mon 12) year (1- year)))
      ;; Clamp day to valid range for target month
      (let ((max-day (calendar-last-day-of-month mon year)))
        (when (> day max-day) (setq day max-day)))
      (list (nth 0 parsed) (nth 1 parsed) (nth 2 parsed) day mon year)))

  (defun gcal/add-days (parsed n)
    "Add N days to PARSED time."
    (let ((time (gcal/time-to-absolute parsed)))
      (let ((new-time (time-add time (days-to-time n))))
        (let ((decoded (decode-time new-time)))
          (list (nth 0 decoded) (nth 1 decoded) (nth 2 decoded)
                (nth 3 decoded) (nth 4 decoded) (nth 5 decoded))))))

  (defun gcal/add-weeks (parsed n)
    "Add N weeks to PARSED time."
    (gcal/add-days parsed (* n 7)))

  (defun gcal/add-years (parsed n)
    "Add N years to PARSED time."
    (list (nth 0 parsed) (nth 1 parsed) (nth 2 parsed)
          (nth 3 parsed) (nth 4 parsed) (+ (nth 5 parsed) n)))

  (defun gcal/day-name-to-number (day-name)
    "Convert ICS day abbreviation DAY-NAME to `calendar' day number (0=Sun, 1=Mon, ..., 6=Sat)."
    (pcase day-name
      ("SU" 0) ("MO" 1) ("TU" 2) ("WE" 3) ("TH" 4) ("FR" 5) ("SA" 6)))

  (defun gcal/expand-rrule (dtstart duration-secs rrule)
    "Expand RRULE starting from DTSTART, returning list of (start-parsed . end-parsed).
DURATION-SECS is the event duration in seconds."
    (let* ((freq (and (string-match "FREQ=\\([A-Z]+\\)" rrule) (match-string 1 rrule)))
           (interval (if (string-match "INTERVAL=\\([0-9]+\\)" rrule)
                         (string-to-number (match-string 1 rrule)) 1))
           (count (and (string-match "COUNT=\\([0-9]+\\)" rrule)
                       (string-to-number (match-string 1 rrule))))
           (until (and (string-match "UNTIL=\\([0-9T]+Z?\\)" rrule)
                       (gcal/parse-ics-datetime (match-string 1 rrule))))
           (byday (when (string-match "BYDAY=\\([A-Z,]+\\)" rrule)
                    (mapcar #'gcal/day-name-to-number
                            (split-string (match-string 1 rrule) ","))))
           (now (decode-time))
           (today (list 0 0 0 (nth 3 now) (nth 4 now) (nth 5 now)))
           (horizon (gcal/add-months today gcal/future-months))
           (horizon-time (gcal/time-to-absolute horizon))
           (until-time (when until (gcal/time-to-absolute until)))
           (advance-fn (pcase freq
                         ("DAILY" (lambda (d n) (gcal/add-days d (* n interval))))
                         ("WEEKLY" (lambda (d n) (gcal/add-weeks d (* n interval))))
                         ("MONTHLY" (lambda (d n) (gcal/add-months d (* n interval))))
                         ("YEARLY" (lambda (d n) (gcal/add-years d (* n interval))))))
           (occurrences nil)
           (i 0)
           (max-iter (or count 1000)))
      (when advance-fn
        (if (and byday (string= freq "WEEKLY"))
            ;; WEEKLY with BYDAY: iterate week by week, emit each matching day
            (let ((emitted 0))
              (while (< i max-iter)
                (let ((week-start (funcall advance-fn dtstart i)))
                  ;; Find the Monday of this week (or Sunday, depending on start)
                  ;; We use dtstart's weekday as the anchor for the week
                  (let* ((anchor-time (gcal/time-to-absolute week-start))
                         (anchor-dow (nth 6 (decode-time anchor-time))))
                    (dolist (target-dow byday)
                      (when (< emitted max-iter)
                        (let* ((day-offset (- target-dow anchor-dow))
                               (occ (gcal/add-days week-start day-offset))
                               (occ-time (gcal/time-to-absolute occ))
                               (past-until (and until-time (time-less-p until-time occ-time)))
                               (past-horizon (time-less-p horizon-time occ-time)))
                          (unless (or past-until past-horizon
                                      (time-less-p occ-time (gcal/time-to-absolute dtstart)))
                            (let* ((end-time (time-add occ-time duration-secs))
                                   (end-decoded (decode-time end-time))
                                   (end-parsed (list (nth 0 end-decoded) (nth 1 end-decoded)
                                                     (nth 2 end-decoded) (nth 3 end-decoded)
                                                     (nth 4 end-decoded) (nth 5 end-decoded))))
                              (push (cons occ end-parsed) occurrences)
                              (setq emitted (1+ emitted))))
                          (when (or past-until past-horizon)
                            (setq i max-iter)))))))
                (setq i (1+ i))))
          ;; No BYDAY or non-WEEKLY: simple iteration
          (while (< i max-iter)
            (let* ((occ (funcall advance-fn dtstart i))
                   (occ-time (gcal/time-to-absolute occ)))
              (when (and until-time (time-less-p until-time occ-time))
                (setq i max-iter))
              (when (time-less-p horizon-time occ-time)
                (setq i max-iter))
              (when (< i max-iter)
                (let* ((end-time (time-add occ-time duration-secs))
                       (end-decoded (decode-time end-time))
                       (end-parsed (list (nth 0 end-decoded) (nth 1 end-decoded)
                                         (nth 2 end-decoded) (nth 3 end-decoded)
                                         (nth 4 end-decoded) (nth 5 end-decoded))))
                  (push (cons occ end-parsed) occurrences))
                (setq i (1+ i)))))))
      (nreverse occurrences)))

  (defun gcal/unfold-ics (text)
    "Unfold ICS line continuations (RFC 5545: CRLF + space/tab)."
    (replace-regexp-in-string "\r?\n[ \t]" "" text))

  (defun gcal/extract-attendees (event)
    "Extract attendee display names from EVENT string."
    (let ((pos 0) attendees)
      (while (string-match "^ATTENDEE\\([^:]*\\):\\(.+\\)$" event pos)
        (let ((params (match-string 1 event))
              (value (string-trim (match-string 2 event))))
          (setq pos (match-end 0))
          (if (string-match "CN=\\([^;:]+\\)" params)
              (push (string-trim (match-string 1 params)) attendees)
            (when (string-match "mailto:\\(.+\\)" value)
              (push (string-trim (match-string 1 value)) attendees)))))
      (nreverse attendees)))

  (defun gcal/ics-to-org (ics-content)
    "Parse ICS-CONTENT and return org entries for events up to `gcal/future-months' ahead."
    (let ((entries nil)
          (unfolded (gcal/unfold-ics ics-content))
          (events nil))
      (setq events (split-string unfolded "BEGIN:VEVENT" t))
      (dolist (event events)
        (when (string-match "END:VEVENT" event)
          (let* ((field (lambda (name)
                          (when (string-match (concat "^" name "\\(?:;[^:]*\\)?:\\(.+\\)$")
                                              event)
                            (string-trim (match-string 1 event)))))
                 (dtstart-raw (or (funcall field "DTSTART") ""))
                 (dtend-raw (funcall field "DTEND"))
                 (summary (or (funcall field "SUMMARY") "No title"))
                 (description (funcall field "DESCRIPTION"))
                 (location (funcall field "LOCATION"))
                 (rrule (funcall field "RRULE"))
                 (attendees (gcal/extract-attendees event))
                 (dtstart (gcal/parse-ics-datetime dtstart-raw))
                 (dtend (and dtend-raw (gcal/parse-ics-datetime dtend-raw)))
                 (all-day (not (string-match-p "T" dtstart-raw))))
            (when dtstart
              (let* ((duration-secs (if (and dtstart dtend)
                                        (float-time
                                         (time-subtract (gcal/time-to-absolute dtend)
                                                        (gcal/time-to-absolute dtstart)))
                                      3600))
                     (occurrences (if rrule
                                      (gcal/expand-rrule dtstart duration-secs rrule)
                                    (list (cons dtstart (or dtend dtstart)))))
                     ;; Unescape ICS description
                     (desc (when description
                             (replace-regexp-in-string
                              "\\\\n" "\n"
                              (replace-regexp-in-string "\\\\," "," description))))
                     ;; Separate Meet info from description
                     (meet-info (when desc
                                  (let ((meet-lines nil)
                                        (lines (split-string desc "\n")))
                                    (dolist (line lines)
                                      (when (string-match-p
                                             "\\(?:Google Meet\\|meet\\.google\\.com\\|tel\\.meet/\\|composez le\\|numéros de téléphone\\)"
                                             line)
                                        (push line meet-lines)))
                                    (nreverse meet-lines))))
                     (clean-desc (when desc
                                   (let ((lines (split-string desc "\n"))
                                         (kept nil))
                                     (dolist (line lines)
                                       (unless (or (string-match-p
                                                    "\\(?:Google Meet\\|meet\\.google\\.com\\|tel\\.meet/\\|composez le\\|numéros de téléphone\\)"
                                                    line)
                                                   (string-empty-p (string-trim line)))
                                         (push line kept)))
                                     (nreverse kept))))
                     ;; Build body with metadata
                     (body-parts nil))
                (when (and attendees (> (length attendees) 0))
                  (push (concat "** Participants\n"
                                (mapconcat (lambda (a) (concat "- " a)) attendees "\n"))
                        body-parts))
                (when (and meet-info (> (length meet-info) 0))
                  (push (concat "** Meet\n"
                                (mapconcat #'identity meet-info "\n"))
                        body-parts))
                (when (and location (not (string-empty-p location)))
                  (push (concat "** Lieu\n"
                                (replace-regexp-in-string "\\\\," "," location))
                        body-parts))
                (when (and clean-desc (> (length clean-desc) 0))
                  (push (concat "** Description\n"
                                (mapconcat #'identity clean-desc "\n"))
                        body-parts))
                (let ((body (string-join (nreverse body-parts) "\n")))
                  (dolist (occ occurrences)
                    (let* ((start (car occ))
                           (end (cdr occ))
                           (year (nth 5 start)) (mon (nth 4 start)) (day (nth 3 start))
                           (sort-key (format "%04d%02d%02d%02d%02d" year mon day
                                             (nth 2 start) (nth 1 start)))
                           (ts (if all-day
                                   (format "<%04d-%02d-%02d>" year mon day)
                                 (format "<%04d-%02d-%02d %02d:%02d-%02d:%02d>"
                                         year mon day (nth 2 start) (nth 1 start)
                                         (nth 2 end) (nth 1 end)))))
                      (push (cons sort-key
                                  (if (string-empty-p body)
                                      (concat "* " summary "\n" ts)
                                    (concat "* " summary "\n" ts "\n" body)))
                            entries)))))))))
      (mapconcat #'cdr (sort entries (lambda (a b) (string< (car a) (car b)))) "\n")))

  (defun gcal/fetch-handler (cb _status)
    "Handle async calendar fetch response. Call CB when done."
    (goto-char (point-min))
    (re-search-forward "\n\n" nil t)
    (delete-region (point-min) (point))
    (set-buffer-multibyte t)
    (decode-coding-region (point-min) (point-max) 'utf-8)
    (let* ((ical-buf (current-buffer))
           (ics-content (buffer-string))
           (content (concat "#+TITLE: Google Calendar\n\n"
                            (gcal/ics-to-org ics-content))))
      (write-region content nil gcal/org-file nil 'silent)
      ;; Update buffer if already visiting the file
      (when-let ((org-buf (find-buffer-visiting gcal/org-file)))
        (with-current-buffer org-buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert content)
            (set-buffer-modified-p nil)
            (when (fboundp 'org-element-cache-reset)
              (org-element-cache-reset)))))
      (kill-buffer ical-buf)
      (when (boundp 'org-timeblock-cache)
        (setq org-timeblock-cache nil
              org-timeblock-buffers nil)))
    (message "Calendar updated.")
    (when cb (funcall cb)))

  (defun gcal/fetch (&optional callback)
    "Fetch Google Calendar ICS and convert to org. Call CALLBACK when done."
    (interactive)
    (when gcal/ics-url
      (message "Fetching calendar...")
      (url-retrieve gcal/ics-url
                    (apply-partially #'gcal/fetch-handler callback)
                    nil t)))

  (with-eval-after-load 'org
    (add-to-list 'org-agenda-files gcal/org-file))

(use-package org-timeblock
  :ensure t
  :bind ("C-c c" . gcal/fetch-and-timeblock)
  :custom
  (org-timeblock-span 1)
  :config
  (defun gcal/fetch-and-timeblock ()
    "Fetch Google Calendar then open org-timeblock."
    (interactive)
    (if (not gcal/ics-url)
        (org-timeblock)
      (gcal/fetch #'org-timeblock)))

  (defun gcal/timeblock-show-entry ()
    "Display the selected timeblock event in a dedicated read-only org buffer."
    (interactive)
    (let* ((node (org-timeblock-selected-block))
           (id (and node (dom-attr node 'id)))
           (cache-entry (and id
                             (seq-find
                              (lambda (x)
                                (string= (get-text-property 0 'id x) (car (split-string (format "%s" id) "_"))))
                              org-timeblock-cache))))
      (unless cache-entry
        (user-error "No event selected"))
      (let* ((title (get-text-property 0 'title cache-entry))
             (marker (get-text-property 0 'marker cache-entry))
             (buf (and marker (marker-buffer marker)))
             (content
              (if buf
                  ;; Marker is alive, use it directly
                  (with-current-buffer buf
                    (save-excursion
                      (widen)
                      (goto-char (marker-position marker))
                      (org-back-to-heading-or-point-min t)
                      (buffer-substring-no-properties
                       (point)
                       (org-end-of-subtree t t))))
                ;; Marker dead, search by title in org files
                (catch 'found
                  (dolist (file (org-timeblock-files))
                    (with-current-buffer (find-file-noselect file)
                      (save-excursion
                        (widen)
                        (goto-char (point-min))
                        (when (re-search-forward
                               (concat "^\\*+ .*" (regexp-quote title)) nil t)
                          (org-back-to-heading-or-point-min t)
                          (throw 'found
                                 (buffer-substring-no-properties
                                  (point)
                                  (org-end-of-subtree t t)))))))))))
        (unless content
          (user-error "Cannot find entry for: %s" title))
        (with-current-buffer (get-buffer-create "*Org Timeblock Event*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert content)
            (org-mode)
            (goto-char (point-min))
            (org-fold-show-all)
            (setq buffer-read-only t)
            (local-set-key (kbd "q") #'kill-buffer-and-window))
          (pop-to-buffer (current-buffer))))))

  (defun gcal/open-google-calendar ()
    "Open Google Calendar day view in the default browser."
    (interactive)
    (browse-url "https://calendar.google.com/calendar/u/0/r/day"))

  (define-key org-timeblock-mode-map (kbd "RET") #'gcal/timeblock-show-entry)
  (define-key org-timeblock-mode-map (kbd "<double-mouse-1>") #'gcal/timeblock-show-entry)
  (define-key org-timeblock-mode-map (kbd "M-o") #'gcal/open-google-calendar)
  (define-key org-timeblock-list-mode-map (kbd "M-o") #'gcal/open-google-calendar))

(defun init/load-local-settings ()
  "Load machine-specific local settings after initialization."
  (let ((local-settings "~/.emacs.d/local.el"))
    (when (file-exists-p local-settings)
      (load-file local-settings)))
  (lsp)
  (when (display-graphic-p)
    (fix-char-width-for-spinners)))
(add-hook 'after-init-hook #'init/load-local-settings)
