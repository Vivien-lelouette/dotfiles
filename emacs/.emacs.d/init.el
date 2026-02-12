;;; init.el --- -*- lexical-binding: t; -*-
;; Ensure Emacs loads the most recent byte-compiled files.
(setq load-prefer-newer t)
;; Make Emacs Native-compile .elc files asynchronously by setting
;; `native-comp-jit-compilation' to t.
(setq native-comp-jit-compilation t)
(setq native-comp-deferred-compilation native-comp-jit-compilation)  ; Deprecated

(setq use-package-always-ensure t)
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
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)  ; 16MB
                  gc-cons-percentage 0.1)))

;; GC when Emacs loses focus (debounced, skip during completion)
(add-hook 'focus-out-hook
          (lambda ()
            (unless (or (active-minibuffer-window)
                        (bound-and-true-p company-candidates))
              (garbage-collect))))

;; redisplay-dont-pause is obsolete since Emacs 24.5 - removed
;; Optimize jit-lock for better fontification performance
(setq jit-lock-stealth-time 1.25
      jit-lock-stealth-nice 0.5
      jit-lock-defer-time 0.05  ; Lower defer for snappier highlighting
      jit-lock-chunk-size 1000) ; Smaller chunks = more responsive

(setq large-file-warning-threshold 100000000)

(setq read-process-output-max (* 5 1024 1024)
    process-adaptive-read-buffering nil)

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)

(defun wl/setup-clipboard ()
  "Set up Wayland clipboard integration on first graphical frame."
  (when (and (display-graphic-p)
             (getenv "WAYLAND_DISPLAY"))
    (defvar wl-copy-process nil)
    (defun wl-copy (text)
      (setq wl-copy-process
            (make-process :name "wl-copy"
                          :buffer nil
                          :command '("wl-copy" "-f" "-n")
                          :connection-type 'pipe
                          :noquery t))
      (process-send-string wl-copy-process text)
      (process-send-eof wl-copy-process))
    (defun wl-paste ()
      (if (and wl-copy-process (process-live-p wl-copy-process))
          nil
        (shell-command-to-string "wl-paste -n | tr -d \r")))
    (setq interprogram-cut-function #'wl-copy
          interprogram-paste-function #'wl-paste))
  (remove-hook 'server-after-make-frame-hook #'wl/setup-clipboard))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'wl/setup-clipboard)
  (wl/setup-clipboard))

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
(define-key minibuffer-local-map (kbd "S-<return>") (lambda () (interactive) (insert "\n")))
(define-key minibuffer-local-map (kbd "M-\\")
  (lambda () (interactive)
    (save-excursion
      (goto-char (minibuffer-prompt-end))
      (while (search-forward " " nil t)
        (replace-match "\\\\ ")))))
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
(bind-key* "C-x k" #'kill-buffer-and-window)
(bind-key* "C-x K" #'kill-buffer)
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

(global-hl-line-mode 1)

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

  (dolist (fn '(jira-issues jira-tempo))
    (advice-add fn :around
                (lambda (orig-fn &rest args)
                  (let ((switch-to-buffer-obey-display-actions t))
                    (apply orig-fn args))))))

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
 display-line-numbers-type 'relative
 mode-line-percent-position nil)
(global-display-line-numbers-mode 1)
(add-hook 'completion-list-mode-hook (lambda () (display-line-numbers-mode 0)))
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
(setq window-divider-default-right-width 22
      window-divider-default-bottom-width 22)

(window-divider-mode 1)

(use-package spacious-padding
  :config
  (spacious-padding-mode 1))

(setq initial-scratch-message nil)

(setq-default fill-column 148)

(defun fonts/set-fonts ()
  (interactive)
  (set-face-attribute 'default nil :font "SauceCodePro NF-11")

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil :font "SauceCodePro NF-11")

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font "Cantarell-11" :weight 'regular)
  (dolist (face '(default fixed-pitch))
    (set-face-attribute `,face nil :font "SauceCodePro NF-11"))
  (fix-char-width-for-spinners))
(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'fonts/set-fonts)
  (add-hook 'window-setup-hook #'fonts/set-fonts))

(setq auth-sources '("~/.authinfo.gpg"))

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

(add-hook 'after-init-hook
          #'(lambda ()
              (load-file "~/.emacs.d/custom_packages/dracula-theme.el")
              (load-theme 'dracula t)

              (fringe-mode '(24 . 12))

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
                    (face-remap-add-relative 'fringe :background "#282a36"))))))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq
   inhibit-compacting-font-caches t
   mode-line-right-align-edge 'window
   doom-modeline-percent-position nil
   doom-modeline-enable-word-count t
   doom-modeline-continuous-word-count-modes nil
   doom-modeline-total-line-number t
   doom-modeline-position-line-format '("" " " "%l")
   doom-modeline-position-column-line-format '("" " " "%cC %lL"))
  (line-number-mode 1)
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

(use-package helm
  :config
  (setq helm-input-idle-delay                     0.1
        helm-reuse-last-window-split-state        nil
        helm-always-two-windows                   nil
        helm-split-window-default-side            'below
        helm-commands-using-frame                 '()
        helm-actions-inherit-frame-settings       t
        helm-use-frame-when-more-than-two-windows nil
        helm-use-frame-when-no-suitable-window    nil
        helm-persistent-action-display-window     t
        helm-show-action-window-other-window      'right
        helm-allow-mouse                          t
        helm-move-to-line-cycle-in-source         nil
        helm-echo-input-in-header-line            nil
        helm-autoresize-max-height                30   ; it is %.
        helm-autoresize-min-height                5   ; it is %.
        helm-follow-mode-persistent               t
        helm-candidate-number-limit               300
        helm-visible-mark-prefix                  "✓"
        helm-kill-real-or-display-selection       'real
        helm-truncate-lines                       t
        helm-net-prefer-curl                      t
        helm-split-window-inside-p                t
        helm-grep-git-grep-command "git --no-pager grep -n%cH --color=always --full-name -e \"%p\" -- %f"
        helm-buffers-show-icons nil
        helm-buffer-max-length 35)

  (defun helm--show-action-window-other-window-p ()
    helm-show-action-window-other-window)

  (defun helm/hook ()
    (display-line-numbers-mode 0))
  (add-hook 'helm-major-mode-hook 'helm/hook)

  (defun helm/grep-do-git-grep ()
    (interactive)
    (let* ((helm-candidate-number-limit nil))
      (call-interactively 'helm-grep-do-git-grep)))

  (defun helm/grep-do-git-grep-literal ()
    "Git grep with literal string matching (handles spaces and parentheses)."
    (interactive)
    (let* ((helm-candidate-number-limit nil)
           (pattern (read-string "Git grep literal: " (thing-at-point 'symbol)))
           (default-directory (or (helm-browse-project-get--root-dir (helm-current-directory))
                                  default-directory)))
      (helm-grep-git-1 default-directory nil nil (list "-F" "-e" pattern))))

  (defun helm/do-grep-ag ()
    (interactive)
    (let* ((helm-candidate-number-limit nil))
      (call-interactively 'helm-do-grep-ag)
      ))

  (defun helm/do-grep-ag-project ()
    (interactive)
    (let* ((helm-candidate-number-limit nil))
      (call-interactively 'helm-do-grep-ag-project)
      ))

  (defadvice helm-display-mode-line (after undisplay-header activate)
    (setq header-line-format nil))

  (global-set-key (kbd "C-h r")                        'helm-info-emacs)
  (global-set-key (kbd "M-x")                          'undefined)
  (global-set-key (kbd "M-x")                          'helm-M-x)
  (global-set-key (kbd "M-y")                          'helm-show-kill-ring)
  (global-set-key (kbd "C-x C-f")                      'helm-find-files)
  (global-set-key (kbd "C-x b")                        'helm-buffers-list)
  (global-set-key (kbd "C-c <SPC>")                    'helm-mark-ring)
  (global-set-key [remap bookmark-jump]                'helm-filtered-bookmarks)
  (global-set-key (kbd "C-c i")                        'helm-imenu)
  (global-set-key (kbd "C-c I")                        'helm-imenu-in-all-buffers)
  (global-set-key (kbd "C-:")                          'helm-eval-expression-with-eldoc)
  (global-set-key (kbd "C-,")                          'helm-calcul-expression)
  (global-set-key (kbd "C-h d")                        'helm-info-at-point)
  (global-set-key (kbd "C-h i")                        'helm-info)
  (global-set-key (kbd "C-x C-d")                      'helm-browse-project)
  (global-set-key (kbd "<f1>")                         'helm-resume)
  (global-set-key (kbd "C-h C-f")                      'helm-apropos)
  (global-set-key (kbd "C-h a")                        'helm-apropos)
  (global-set-key (kbd "C-h C-d")                      'helm-debug-open-last-log)
  (global-set-key (kbd "S-<f4>")                       'helm-execute-kmacro)
  (global-set-key (kbd "M-s")                          nil)
  (global-set-key (kbd "M-s M-s")                      'helm-occur-visible-buffers)
  (global-set-key (kbd "M-s M-d")                      'helm-find)
  (global-set-key (kbd "M-s M-o")                      'helm-org-agenda-files-headings)
  (global-set-key (kbd "M-s M-i")                      'helm-google-suggest)
  (global-set-key (kbd "C-x C-b")                      'helm-semantic-or-imenu)

  (define-key global-map (kbd "M-s M-g")               'helm/grep-do-git-grep)
  (define-key global-map (kbd "M-s M-G")               'helm/grep-do-git-grep-literal)
  (define-key global-map [remap bookmark-bmenu-list]   'helm-register)
  (define-key global-map [remap list-buffers]          'helm-buffers-list)
  (define-key global-map [remap dabbrev-expand]        'helm-dabbrev)
  (define-key global-map (kbd "M-g a")                 'helm/do-grep-ag)
  (define-key global-map (kbd "M-s M-p")               'helm/do-grep-ag-project)
  (define-key global-map (kbd "M-g l")                 'goto-line)
  (define-key global-map (kbd "M-g M-g")               'helm-revert-next-error-last-buffer)
  (define-key global-map (kbd "M-g i")                 'helm-gid)
  (define-key global-map (kbd "C-x r p")               'helm-projects-history)
  (define-key global-map (kbd "C-x r c")               'helm-addressbook-bookmarks)
  (define-key global-map (kbd "C-c t r")               'helm-dictionary)

  (helm-mode 1)
  (helm-autoresize-mode 1))

(use-package helm-xref)

(use-package helm-tramp)

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
  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'org/org-babel-tangle-config))))
  ;; (custom-set-faces
  ;;  '(org-level-1 ((t (:inherit outline-1 :height 2.5))))
  ;;  '(org-level-2 ((t (:inherit outline-2 :height 1.8))))
  ;;  '(org-level-3 ((t (:inherit outline-3 :height 1.4))))
  ;;  '(org-level-4 ((t (:inherit outline-4 :height 1.2))))
  ;;  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))))

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

(autoload 'exwm-enable "~/.emacs.d/desktop.el")

(use-package avy
  :config
  (require 'bind-key)
  (bind-key "M-j" #'avy-goto-char-timer))

(use-package multiple-cursors
  :vc (:url "https://github.com/magnars/multiple-cursors.el" :rev :newest)
  :hook
  ((multiple-cursors-mode . (lambda ()
                              (set-face-attribute 'mc/cursor-bar-face nil :height 1 :background nil :inherit 'cursor))))
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
  (setq combobulate-flash-node nil))

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
  (setq which-key-popup-type 'minibuffer
        which-key-idle-delay 0.5)  ; Show after 0.5s
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
    (wgrep-change-to-wgrep-mode))

  (add-hook 'helm-occur-mode-hook 'wgrep/hook)
  (add-hook 'helm-grep-mode-hook 'wgrep/hook))

(use-package wgrep-helm
  :after (wgrep helm))

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
  :hook (emacs-startup . global-jinx-mode)
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

  (defun docker/auto-refresh ()
    "Auto-refresh docker containers view. Cancels itself, if this buffer was killed."
    (run-with-local-timer 5 5 'revert-buffer))

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

(use-package claudemacs
  :vc (:url "https://github.com/cpoile/claudemacs"
            :rev :newest
            :branch "main")
  :config
  (setq claudemacs-use-shell-env t
        claudemacs-switch-to-buffer-on-create nil
        claudemacs-switch-to-buffer-on-toggle nil
        claudemacs-switch-to-buffer-on-file-add nil
        claudemacs-switch-to-buffer-on-send-error nil
        claudemacs-switch-to-buffer-on-add-context nil)

  (defvar claudemacs--scroll-timer nil)

  (defun claudemacs/scroll-to-bottom ()
    "Scroll all claudemacs windows to the bottom."
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (string-prefix-p "*claudemacs" (buffer-name buf)))
        (dolist (window (get-buffer-window-list buf nil t))
          (with-selected-window window
            (goto-char (point-max))
            (recenter -1))))))

  (defun claudemacs/start-scroll-timer ()
    "Start auto-scroll timer for claudemacs."
    (unless claudemacs--scroll-timer
      (setq claudemacs--scroll-timer
            (run-with-timer 0 0.5 #'claudemacs/scroll-to-bottom))))

  (defun claudemacs/stop-scroll-timer ()
    "Stop auto-scroll timer."
    (when claudemacs--scroll-timer
      (cancel-timer claudemacs--scroll-timer)
      (setq claudemacs--scroll-timer nil)))

  (defun claudemacs/toggle-follow ()
    "Toggle auto-scroll for claudemacs buffers."
    (interactive)
    (if claudemacs--scroll-timer
        (progn
          (claudemacs/stop-scroll-timer)
          (message "Claudemacs follow: OFF"))
      (claudemacs/start-scroll-timer)
      (message "Claudemacs follow: ON")))

  (transient-append-suffix 'claudemacs-transient-menu '(-1)
    ["Quick"
     ("c" "Claude" transient:claudemacs-start-menu::0)
     ("f" "Toggle follow" claudemacs/toggle-follow)])

  (defun claudemacs/startup-hook ()
    (claudemacs/start-scroll-timer)
    (run-with-timer 0.5 nil #'eat-emacs-mode))

  (add-hook 'claudemacs-startup-hook #'claudemacs/startup-hook)

  (global-set-key (kbd "C-c c") #'claudemacs-transient-menu))

(use-package joplin-mode
  :vc (:url "https://github.com/cinsk/joplin-mode" :rev :newest)
  :commands (joplin joplin-search joplin-create-note joplin-create-notebook
                    joplin-journal joplin-delete-note-dwim joplin-delete-notebook-dwim)
  :bind (("M-s m"   . joplin-find-note)
         ("M-s M-m" . joplin-find-note)
         ("C-c m"   . joplin-transient-menu))
  :config
  (setq joplin-token (auth-source-pick-first-password :host "joplin"))

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
        (car (sort candidates
                   (lambda (a b)
                     (> (or (alist-get 'updated_time
                                       (joplin--http-get (format "/folders/%s" a))) 0)
                        (or (alist-get 'updated_time
                                       (joplin--http-get (format "/folders/%s" b))) 0))))))))

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
    "Read a Joplin path with iterative Helm folder navigation.
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
                 (sel (helm-comp-read (concat prompt path) candidates
                                      :must-match nil)))
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
       (unless joplin-folders (joplin--init-folders))
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
       (unless joplin-folders (joplin--init-folders))
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
    (unless joplin-folders (joplin--init-folders))
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
    (switch-to-buffer (joplin--note-buffer (joplin--journal-note-id date-str))))

  (defun joplin-delete-note-dwim ()
    "Delete a Joplin note.  Uses note at point, current note buffer,
  or prompts for a path via Helm navigation."
    (interactive)
    (unless joplin-folders (joplin--init-folders))
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
  browser, or prompts for a path via Helm navigation."
    (interactive)
    (unless joplin-folders (joplin--init-folders))
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
          (add-hook 'after-revert-hook
                    (lambda ()
                      (setq buffer-read-only nil)
                      (when view-mode (view-mode -1)))
                    90 t)
          (joplin--insert-backlinks-section)
          (add-hook 'before-save-hook
                    (lambda ()
                      (setq-local joplin--saving-via-hook t)
                      (joplin--remove-backlinks-section))
                    -100 t)
          (add-hook 'after-save-hook
                    (lambda ()
                      (unwind-protect
                          (joplin--insert-backlinks-section)
                        (setq-local joplin--saving-via-hook nil)))
                    100 t)))
      (with-current-buffer buf
        (setq-local joplin-parent-buffer parent))
      buf))

  (advice-add 'joplin--note-buffer :override #'joplin--note-buffer-by-id)

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
                        'local-map mode-line-buffer-identification-keymap))))

  ;; Linkify YYYY-MM-DD dates to journal notes on save
  (defun joplin--linkify-dates ()
    "Replace bare YYYY-MM-DD dates with Joplin journal links in the note body.
  Stops before the backlinks section to avoid modifying read-only text."
    (when (bound-and-true-p joplin-note-mode)
      (save-excursion
        (goto-char (point-min))
        (let ((search-end (when (and (markerp joplin--backlinks-start)
                                     (marker-position joplin--backlinks-start))
                            joplin--backlinks-start)))
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
    "Remove backlinks and linkify dates before saving a Joplin note."
    (joplin--remove-backlinks-section)
    (joplin--linkify-dates))

  (advice-add 'joplin-save-note :before #'joplin--before-save-note)

  (defun joplin--after-save-note (&rest _)
    "Rename buffer and restore backlinks after save.
  When called from `write-file-functions' (inside `save-buffer'),
  skip backlinks insertion — `after-save-hook' will handle it,
  avoiding read-only errors from premature re-insertion."
    (when (bound-and-true-p joplin-note)
      (let ((title (JNOTE-title joplin-note)))
        (unless title
          (setq-local joplin-note (joplin-get-note (JNOTE-id joplin-note))))))
    (joplin--rename-buffer-from-note)
    (unless joplin--saving-via-hook
      (joplin--insert-backlinks-section)))

  (advice-add 'joplin-save-note :after #'joplin--after-save-note)

  (defun joplin--revert-note-buffer (_ignore-auto _noconfirm)
    "Revert the current buffer from the Joplin API."
    (let ((id (JNOTE-id joplin-note))
          (pos (point))
          (buf (current-buffer)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (alist-get 'body
                           (joplin--http-get
                            (concat "/notes/" id)
                            '((fields . "body")))))
        (goto-char (min pos (point-max))))
      (setq-local joplin-note (joplin-get-note id))
      (joplin--rename-buffer-from-note)
      (set-buffer-modified-p nil)
      (setq buffer-read-only nil)
      (when view-mode (view-mode -1))
      ;; Ensure edit mode after all revert hooks complete
      (run-with-timer 0 nil
                      (lambda (b)
                        (when (buffer-live-p b)
                          (with-current-buffer b
                            (setq buffer-read-only nil)
                            (when view-mode (view-mode -1)))))
                      buf)
      (joplin--insert-backlinks-section)
      (message "Reverted note from Joplin")))

  (advice-remove 'joplin--note-fill-buffer #'joplin--after-note-fill-buffer)

  (defun joplin-backlinks ()
    "List notes that link to the current Joplin note."
    (interactive)
    (unless (bound-and-true-p joplin-note)
      (user-error "Not in a Joplin note buffer"))
    (joplin-search nil (JNOTE-id joplin-note)))

  (defvar-local joplin--backlinks-start nil
    "Marker for the start of the backlinks section in a Joplin note buffer.")

  (defvar-local joplin--saving-via-hook nil
    "Non-nil while `save-buffer' is in progress, to prevent the :after
  advice on `joplin-save-note' from re-inserting read-only backlinks
  during `write-file-functions'.")

  (defun joplin--fetch-backlinks-for-note ()
    "Return a list of (TITLE . ID) for notes that link to the current note."
    (condition-case err
        (when (bound-and-true-p joplin-note)
          (let* ((id (JNOTE-id joplin-note))
                 (resp (joplin--http-get "/search"
                                         `((query . ,id)
                                           (type . note)
                                           (fields . "id,title")
                                           (limit . "50"))))
                 (items (alist-get 'items resp)))
            (cl-loop for item across items
                     unless (string= (alist-get 'id item) id)
                     collect (cons (alist-get 'title item)
                                   (alist-get 'id item)))))
      (error (message "Joplin backlinks: %s" (error-message-string err))
             nil)))

  (defun joplin--remove-backlinks-section ()
    "Remove the backlinks section from the current buffer."
    (when (and (markerp joplin--backlinks-start)
               (marker-position joplin--backlinks-start))
      (let ((inhibit-read-only t)
            (buffer-undo-list t))
        (delete-region joplin--backlinks-start (point-max))
        (set-marker joplin--backlinks-start nil))))

  (defun joplin--insert-backlinks-section ()
    "Insert a read-only backlinks section at the end of the current buffer."
    (joplin--remove-backlinks-section)
    (let ((backlinks (joplin--fetch-backlinks-for-note)))
      (when backlinks
        (save-excursion
          (let ((inhibit-read-only t)
                (buffer-undo-list t))
            (goto-char (point-max))
            (setq-local joplin--backlinks-start (point-marker))
            (let ((start (point)))
              (insert "\n\n# ──────────── Backlinks ────────────\n")
              (dolist (bl backlinks)
                (insert (format "[%s](:/%s)\n" (car bl) (cdr bl))))
              (add-text-properties start (point-max)
                                   '(read-only t joplin-backlinks t face shadow)))))))
    (set-buffer-modified-p nil))

  ;; Backlinks removal/restoration during save:
  ;; - before-save-hook (-100): sets `joplin--saving-via-hook', removes backlinks
  ;; - write-file-functions: `joplin-save-note' (with :before/:after advice)
  ;;   The :after advice skips backlinks insertion when the flag is set,
  ;;   avoiding read-only text in the buffer during basic-save-buffer processing
  ;; - after-save-hook (100): re-inserts backlinks, clears flag
  ;; For direct `joplin-save-note' calls (C-c j s), the flag is nil so
  ;; the :after advice handles backlinks insertion directly.

  (defun joplin-follow-link-at-point ()
    "Follow link at point: open Joplin notes in Emacs, others externally."
    (interactive)
    (if-let ((url (markdown-link-url)))
        (if (string-prefix-p ":/" url)
            (switch-to-buffer (joplin--note-buffer (substring url 2)))
          (browse-url url))
      (user-error "No link at point")))

  (defun joplin--search-notes-candidates ()
    "Fetch Joplin notes matching current `helm-pattern'."
    (when (>= (length helm-pattern) 2)
      (let* ((resp (joplin--http-get "/search"
                                     `((query . ,(url-hexify-string helm-pattern))
                                       (type . note)
                                       (fields . "id,title")
                                       (limit . "30"))))
             (items (alist-get 'items resp)))
        (cl-loop for item across items
                 collect (cons (alist-get 'title item)
                               (alist-get 'id item))))))

  (defvar joplin--insert-link-candidates nil
    "Last candidates from `joplin--search-notes-candidates'.")

  (defun joplin-insert-link ()
    "Search for a Joplin note with live Helm and insert a link at point."
    (interactive)
    (unless (bound-and-true-p joplin-note-mode)
      (user-error "Not in a Joplin note buffer"))
    (joplin--init)
    (let ((id (helm :sources
                    (helm-build-sync-source "Joplin notes"
                      :candidates (lambda ()
                                    (setq joplin--insert-link-candidates
                                          (joplin--search-notes-candidates))
                                    joplin--insert-link-candidates)
                      :volatile t
                      :match #'identity
                      :requires-pattern 2)
                    :buffer "*helm joplin link*"
                    :prompt "Note: ")))
      (when id
        (let ((title (car (rassoc id joplin--insert-link-candidates))))
          (insert (format "[%s](:/%s)" (or title "Untitled") id))))))

  (defun joplin-find-note ()
    "Search for a Joplin note with live Helm and open it."
    (interactive)
    (joplin--init)
    (let ((id (helm :sources
                    (helm-build-sync-source "Joplin notes"
                      :candidates #'joplin--search-notes-candidates
                      :volatile t
                      :match #'identity
                      :requires-pattern 2)
                    :buffer "*helm joplin find*"
                    :prompt "Note: ")))
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

(use-package easysession
  :vc (:url "https://github.com/jamescherti/easysession.el" :rev :newest)
  :ensure t
  :custom
  (easysession-save-interval (* 10 60))  ; Save every 10 minutes

  ;; Display the active session name in the mode-line lighter.
  (easysession-save-mode-lighter-show-session-name t)

  ;; Optionally, the session name can be shown in the modeline info area:
  ;; (easysession-mode-line-misc-info t)

  :config
  ;; Key mappings
  (global-set-key (kbd "C-c sl") #'easysession-switch-to) ; Load session
  (global-set-key (kbd "C-c ss") #'easysession-save) ; Save session
  (global-set-key (kbd "C-c sL") #'easysession-switch-to-and-restore-geometry)
  (global-set-key (kbd "C-c sr") #'easysession-rename)
  (global-set-key (kbd "C-c sR") #'easysession-reset)
  (global-set-key (kbd "C-c sd") #'easysession-delete)

  ;; Non-nil: `easysession-setup' loads the session automatically.
  ;; Nil: session is not loaded automatically; the user can load it manually.
  (setq easysession-setup-load-session t)

  ;; Priority depth used when `easysession-setup' adds `easysession' hooks.
  ;; 102 ensures that the session is loaded after all other packages.
  (setq easysession-setup-add-hook-depth 102)

  ;; The `easysession-setup' function adds hooks:
  ;; - To automatically load the session during `emacs-startup-hook'.
  ;; - To automatically save the session at regular intervals, and when Emacs
  ;; exits.
  (easysession-setup))

(use-package jira
  :vc (:url "https://github.com/unmonoqueteclea/jira.el" :rev :newest)
  :config
  (defun jira/detail-find-issue-by-key ()
    (interactive)
    (jira-detail-find-issue-by-key))

  (global-set-key (kbd "M-s j") #'jira/detail-find-issue-by-key)
  (global-set-key (kbd "M-s M-j") #'jira/detail-find-issue-by-key)

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

  (defun jira/inline-images ()
    "Replace <file:...> image placeholders with inline images in jira-detail buffer."
    (interactive)
    (unless jira-detail--current
      (user-error "Not in a jira-detail buffer"))
    (let ((issue jira-detail--current)
          (count 0)
          (done 0))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward
                "<file:\\(?:[^:>]+:\\)?\\([^>]+\\.\\(?:png\\|jpe?g\\|gif\\|webp\\)\\)>"
                nil t)
          (let* ((filename (match-string-no-properties 1))
                 (placeholder (match-string-no-properties 0))
                 (id (jira/attachment-id-by-name issue filename)))
            (when id
              (cl-incf count)
              ;; capture loop variables for the async callback
              ;; key must be a lexical binding (not defvar-local) so the
              ;; closure sees the value captured here, not a dynamic lookup
              ;; in whatever buffer request.el uses for the callback.
              (let ((ph placeholder)
                    (fname filename)
                    (key jira-detail--current-key))
                (jira-api-call
                 "GET" (format "attachment/content/%s" id)
                 :parser (lambda ()
                          (set-buffer-multibyte nil)
                          (buffer-string))
                 :callback
                 (lambda (data _response)
                   (when-let* ((buf (jira-detail--get-issue-buffer key)))
                     (with-current-buffer buf
                       (let ((inhibit-read-only t))
                         (save-excursion
                           (goto-char (point-min))
                           (when (search-forward ph nil t)
                             (replace-match "")
                             (insert-image
                              (create-image data nil t
                                            :max-width (min 1000 (- (window-pixel-width) 50)))
                              (format "[%s]" fname))
                             (insert "\n"))))
                      (cl-incf done)
                      (when (= done count)
                        (goto-char (point-min)))))))))))))
      (if (> count 0)
          (message "Fetching %d image(s)..." count)
        (message "No image placeholders found")))

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

  (with-eval-after-load 'jira-detail
    (define-key jira-detail-mode-map (kbd "I") #'jira/toggle-auto-inline-images)
    (define-key jira-detail-mode-map (kbd "c") #'jira/copy-issue-link)
    (define-key jira-detail-mode-map (kbd "M-o")
      (lambda () "Open link at point or issue in browser" (interactive)
        (jira/browse-url-at-point-or-issue jira-detail--current-key)))
    (advice-add 'jira-detail--issue :after
                (lambda (key &rest _)
                  (when-let* ((buf (jira-detail--get-issue-buffer key)))
                    (with-current-buffer buf
                      (when jira/auto-inline-images
                        (jira/inline-images)))))))

  (with-eval-after-load 'jira-issues
    (define-key jira-issues-mode-map (kbd "c") #'jira/copy-issue-link)
    (define-key jira-issues-mode-map (kbd "M-o")
      (lambda () "Open link at point or issue in browser" (interactive)
        (jira/browse-url-at-point-or-issue (jira-utils-marked-item)))))

  )

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

(electric-indent-mode 1)
(defun electric-indent/inhibit ()
  (interactive)
  (setq-local electric-indent-inhibit t))

(use-package ilist
  :vc (:url "https://github.com/emacs-straight/ilist" :rev :newest)
  :defer t)

(use-package transient
  :defer t)

(use-package magit
  :defer t
  :commands (magit-status magit-clone magit-blame)
  :bind (("C-x g g" . magit-status)
         ("C-x g c" . magit-clone)
         ("C-x g s" . magit/magit-status-no-split))
  :config
  (require 'ilist)
  (defun magit/magit-status-no-split ()
    "Don't split window."
    (interactive)
    (let ((magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))
      (magit-status)))
  (setq magit-bury-buffer-function 'magit-mode-quit-window))

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

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
      (buffer-string)))
  )

(use-package yasnippet
  :defer 2
  :bind (:map yas-keymap
              ("C-<right>" . yas-next-field)
              ("C-<left>" . yas-prev-field)
              ("<tab>" . nil)
              ("TAB" . nil))
  :config
  (yas-global-mode 1))

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
  (advice-add 'sql-comint-postgres :around
              (lambda (orig-fun product options &rest args)
                (setenv "PGPASSWORD" sql-password)
                (unwind-protect
                    (apply orig-fun product options args)
                  (setenv "PGPASSWORD" nil)))))

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
  (apheleia-global-mode t))

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
  (global-flycheck-mode 1))

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
        lsp-enable-symbol-highlighting t
        lsp-enable-on-type-formatting nil)

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

;; (use-package lsp-ui :commands lsp-ui-mode)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)

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
    (exec-path-from-shell-initialize)))

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

  (defun eshell/hook ()
    (require 'eshell)
    (require 'em-smart)
    (define-key eshell-mode-map (kbd "M-m") #'eshell-bol)
    (define-key eshell-hist-mode-map (kbd "M-s") nil)
    (define-key eshell-hist-mode-map (kbd "M-r") #'helm-eshell-history)
    (setq
     eshell-where-to-jump 'begin
     eshell-review-quick-commands nil
     eshell-smart-space-goes-to-end t
     eshell-prompt-function
     (lambda ()
       (concat (format-time-string " %Y-%m-%d %H:%M" (current-time))
               (if (= (user-uid) 0) " # " " $ ")))
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
Reuses an existing eshell buffer for the target directory if one exists."
    (interactive)
    (let* ((default-directory (replace-regexp-in-string "/$" ""
                                (if (derived-mode-p 'dired-mode)
                                    default-directory
                                  (utils/get-project-root-if-wanted))))
           (eshell-buffer (eshell/get-relevant-buffer default-directory)))
      (if eshell-buffer
          (switch-to-buffer eshell-buffer)
        (eshell 'N))))

  (global-set-key (kbd "C-s-<return>") #'eshell/new-or-current)

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

    (set-fontset-font t 'symbol "DejaVu Sans Mono" nil 'prepend)

    (set-fontset-font t 'emoji "DejaVuSans" nil 'prepend)

    (set-fontset-font t '(#x2800 . #x28FF) "DejaVu Sans Mono")
    (set-fontset-font t '(#x2500 . #x257F) "DejaVu Sans Mono")
    (set-fontset-font t '(#x2580 . #x259F) "DejaVu Sans Mono")
    (set-fontset-font t '(#x2190 . #x21FF) "DejaVu Sans Mono")
    (set-fontset-font t '(#x2700 . #x27BF) "DejaVu Sans Mono")
    (set-fontset-font t #x00B7 "DejaVu Sans Mono")

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
  (add-hook 'eat-mode-hook
            (lambda ()
              (setq-local eat--synchronize-scroll-function #'eat--synchronize-scroll)))
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

(defun dired/open-file ()
  "In dired, open the file named on this line."
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
  (setq ls-lisp-dirs-first t
        wdired-allow-to-change-permissions t
        dired-auto-revert-buffer t)
  (add-hook 'wdired-mode-hook
            (lambda ()
              (define-key wdired-mode-map (kbd "s-<escape>") 'wdired-abort-changes))))

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
    (add-hook 'eww-after-render-hook (lambda () (visual-line-mode 1)))))

(add-hook 'after-init-hook
    #'(lambda ()
	(let ((local-settings "~/.emacs.d/local.el"))
	  (when (file-exists-p local-settings)
	    (load-file local-settings)))
    (lsp)
    (when (display-graphic-p)
      (fix-char-width-for-spinners))))
