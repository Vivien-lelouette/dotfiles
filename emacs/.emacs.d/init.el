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
        ;; Enable :ensure use-package keyword.
        (elpaca-use-package-mode)
        ;; Assume :ensure t unless otherwise specified.
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

(defalias 'yes-or-no-p 'y-or-n-p)
(setq xref-prompt-for-identifier nil
      comint-prompt-read-only t
      use-dialog-box nil)

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

(global-auto-revert-mode 1)
(require 'bind-key)
(bind-key* "C-x k" #'kill-current-buffer)
(bind-key* "C-x K" #'kill-buffer)
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)

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

(setq indent-tabs-mode nil
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
 display-line-numbers-type 'relative
 mode-line-percent-position nil)
(global-display-line-numbers-mode 0)
(add-hook 'completion-list-mode-hook (lambda () (display-line-numbers-mode 0)))
(line-number-mode 0)
(column-number-mode 0)
(global-hl-line-mode 0)

(setq warning-minimum-level :error)

(repeat-mode 1)

(scroll-bar-mode 1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(setq window-divider-default-right-width 22
      window-divider-default-bottom-width 22)

(window-divider-mode 1)

(setq-default fill-column 120)

(defun fonts/set-fonts ()
  (interactive)
  (set-face-attribute 'default nil :font "SauceCodePro NF-11")

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil :font "SauceCodePro NF-11")

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font "Cantarell-11" :weight 'regular))
(add-hook 'server-after-make-frame-hook #'fonts/set-fonts)

(defun disable-mixed-pitch ()
  (interactive)
  (mixed-pitch-mode -1))

(use-package mixed-pitch
  :hook
  (org-mode . mixed-pitch-mode))

(setq auth-sources '("~/.authinfo.gpg"))

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

(setq tab-always-indent 'complete)
;; (setq completions-format 'one-column
;;       completions-header-format nil
;;       completion-show-help t
;;       completion-show-inline-help t
;;       completions-max-height 30
;;       completion-auto-select nil)

(setq-default isearch-lazy-count t
              isearch-allow-motion t)

(use-package vertico
  :config
  (load-file "~/.emacs.d/elpaca/repos/vertico/extensions/vertico-multiform.el")
  (load-file "~/.emacs.d/elpaca/repos/vertico/extensions/vertico-flat.el")
  (load-file "~/.emacs.d/elpaca/repos/vertico/extensions/vertico-buffer.el")
  
  (setq vertico-cycle t
        vertico-buffer-mode nil
        vertico-buffer-display-action 'display-buffer-in-child-frame
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

(use-package corfu
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto nil
        ;; corfu-auto-prefix 1
        corfu-echo-documentation t
        corfu-quit-no-match 'separator
        corfu-preselect 'valid)
  
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-auto nil)
              (corfu-mode)))
  
  (defun corfu-send-shell (&rest _)
    "Send completion candidate when inside comint/eshell."
    (cond
     ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
      (eshell-send-input))
     ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
      (comint-send-input))))
  (advice-add #'corfu-insert :after #'corfu-send-shell)
  
  ;; Enable Corfu more generally for every minibuffer, as long as no other
  ;; completion UI is active. If you use Mct or Vertico as your main minibuffer
  ;; completion UI. From
  ;; https://github.com/minad/corfu#completing-with-corfu-in-the-minibuffer
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active) ; Useful if I ever use MCT
                (bound-and-true-p vertico--input))
      (setq-local corfu-auto nil)       ; Ensure auto completion is disabled
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1))

(use-package corfu-terminal
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p :" . cape-emoji)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'tempel-complete)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)

  ;; NOTE 2022-02-05: `kind-icon' depends `svg-lib' which creates a cache
  ;; directory that defaults to the `user-emacs-directory'. Here, I change that
  ;; directory to a location appropriate to `no-littering' conventions, a
  ;; package which moves directories of other packages to sane locations.
  (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/")) ; Change cache dir
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'
  )

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
         ("M-s M-d" . consult-find)
         ("M-s d" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s M-g" . consult-git-grep)
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
  (setq consult-preview-key "M-."
        register-preview-function #'consult-register-format
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref
        consult-buffer-sources '(consult--source-hidden-buffer consult--source-modified-buffer consult--source-buffer consult--source-recent-file consult--source-file-register consult--source-project-buffer-hidden consult--source-project-recent-file-hidden))

  (advice-add #'register-preview :override #'consult-register-window)
  :config
  (setq consult-narrow-key "<"))

(use-package embark-consult)

(use-package orderless
  :init
  (setq completion-styles '(orderless)
  completion-category-defaults nil
  completion-category-overrides '((file (styles partial-completion)))))

(use-package org
  :config
  (define-key org-mode-map (kbd "C-M-S-<left>") nil)
  (define-key org-mode-map (kbd "C-M-S-<right>") nil)

  (setq
   org-confirm-babel-evaluate nil
   org-image-actual-width t
   org-startup-with-inline-images t
   org-support-shift-select t)

  (load-file "~/.emacs.d/custom_packages/org-flyimage.el")
  (with-eval-after-load "org"
    (require 'org-flyimage)
    (add-hook 'org-mode-hook 'org-flyimage-mode)
    (require 'org-indent)
    (add-hook 'org-mode-hook 'org-indent-mode))

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
  :ensure
  (:host github :repo "org-roam/org-roam-ui")
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

(use-package ox-jira)

(use-package time
  :ensure nil
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

(use-package multiple-cursors
  :ensure (:host github :repo "magnars/multiple-cursors.el")
  :hook
  ((multiple-cursors-mode . (lambda ()
                              (set-face-attribute 'mc/cursor-bar-face nil :height 1 :background nil :inherit 'cursor))))
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C-}") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-{") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-;") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
  (setq mc/black-list-prefer t))

(use-package kmacro-x
  :init (kmacro-x-atomic-undo-mode 1))

(use-package combobulate
  :ensure (:host github :repo "mickeynp/combobulate")
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

(use-package flymake-margin
  :ensure (:host github :repo "LionyxML/flymake-margin")
  :after flymake
  :config
  (flymake-margin-mode ))

(use-package perfect-margin
  :config
  (setq perfect-margin-only-set-left-margin nil
        perfect-margin-ignore-regexps nil
        perfect-margin-ignore-filters nil)
  (perfect-margin-mode 0))

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
  (setq which-key-popup-type 'minibuffer)
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
  :ensure nil
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

(elpaca (explain-pause-mode :host github :repo "lastquestion/explain-pause-mode"))

(use-package free-keys)

(use-package csv-mode
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
  :ensure (:host github :repo "dmacvicar/ob-d2")
  :defer t)

(use-package jinx
  :ensure nil
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package lingva)

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
(defun electric-indent/inhibit ()
  (interactive)
  (setq-local electric-indent-inhibit t))

(use-package transient)
(use-package magit
  :ensure (:host github :repo "magit/magit" :ref "g7f472995")
  :config
  (defun magit/magit-status-no-split ()
    "Don't split window."
    (interactive)
    (let ((magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))
      (magit-status)))
  (global-unset-key (kbd "C-x g"))
  (global-set-key (kbd "C-x g g") #'magit-status)
  (global-set-key (kbd "C-x g c") #'magit-clone)
  (global-set-key (kbd "C-x g s") #'magit/magit-status-no-split)
  (setq magit-bury-buffer-function 'magit-mode-quit-window))

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

(use-package forge
  :ensure (:host github :repo "magit/forge" :ref "fca33c6")
  :after magit
  :config
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

(use-package browse-at-remote
  :config
  (global-set-key (kbd "C-x g b") 'browse-at-remote))

(use-package why-this)

(use-package tempel
  :bind (("M-TAB" . tempel-complete) ;; Alternative tempel-expand
         :map tempel-map
         ("M-TAB" . tempel-next)
         ("M-S-TAB" . tempel-previous)))

(use-package tempel-collection
  :ensure t
  :after tempel
  )

(use-package eglot-tempel
  :ensure t
  :after tempel
  :config
  (add-hook 'tempel-mode-hook 'eglot-tempel-mode))

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

(use-package php-mode)

(use-package sqlup-mode
  :config
  (add-hook 'sql-mode-hook 'sqlup-mode))

(use-package sqlite-mode-extras
  :ensure (:host github :repo "xenodium/sqlite-mode-extras")
  :hook ((sqlite-mode . sqlite-extras-minor-mode)))

(use-package jest-test-mode
  :commands jest-test-mode
  :hook (typescript-mode typescript-ts-mode js-mode js-ts-mode typescript-tsx-mode)
  :config
  (setq display-buffer-alist '())
  (add-to-list
   'display-buffer-alist
   '("\\*jest-test-compilation\\*"
     (display-buffer-reuse-window display-buffer-pop-up-frame)
     (reusable-frames . visible))))

(use-package apheleia
  :config
  (add-to-list 'apheleia-mode-alist '(js-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(js-ts-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(typescript-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(typescript-ts-mode . prettier))
  (apheleia-global-mode t))

;; (use-package treesit-auto
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   ;; (treesit-auto-add-to-auto-mode-alist 'all)
;;   (treesit-auto-add-to-auto-mode-alist (list "awk" "bash" "c" "c-sharp" "css" "dockerfile" "html" "java" "json" "kotlin" "make" "markdown" "nix" "nu" "org" "python" "rust" "sql" "vue" "yaml" "typescript" "tsx"))
;;   (global-treesit-auto-mode))

(use-package eglot
  :ensure nil
  :ensure t
  :hook ((( js-mode js-ts-mode typescript-ts-mode typescript-mode php-mode)
          . eglot-ensure))
  :bind (:map eglot-mode-map
              ("C-." . eglot-code-actions))
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)

  :config
  (add-to-list 'eglot-server-programs '(php-mode . ("phpactor" "language-server")))
  
  (defclass eglot-sqls (eglot-lsp-server) ())
  (add-to-list 'eglot-server-programs '(sql-mode . (eglot-sqls "sqls")))

  (cl-defmethod eglot-execute
    :around
    ((server eglot-sqls) action)

    (pcase (plist-get action :command)
      ("executeQuery"
       (if (use-region-p)
           (let* ((begin (region-beginning))
                  (end (region-end))
                  (begin-lsp (eglot--pos-to-lsp-position begin))
                  (end-lsp (eglot--pos-to-lsp-position end))
                  (action (plist-put action :range `(:start ,begin-lsp :end ,end-lsp)))
                  (result (cl-call-next-method server action)))
             (eglot/sqls-show-results result))
         (message "No region")))

      ((or
        "showConnections"
        "showDatabases"
        "showSchemas"
        "showTables")
       (eglot/sqls-show-results (cl-call-next-method)))

      ("switchConnections"
       (let* ((connections (eglot--request server :workspace/executeCommand
                                           '(:command "showConnections")))
              (collection (split-string connections "\n"))
              (connection (completing-read "Switch to connection: " collection nil t))
              (index (number-to-string (string-to-number connection)))
              (action (plist-put action :arguments (vector index))))
         (cl-call-next-method server action)))

      ("switchDatabase"
       (let* ((databases (eglot--request server :workspace/executeCommand
                                         '(:command "showDatabases")))
              (collection (split-string databases "\n"))
              (database (completing-read "Switch to database: " collection nil t))
              (action (plist-put action :arguments (vector database))))
         (cl-call-next-method server action)))

      (_
       (cl-call-next-method))))

  (defun eglot/sqls-show-results (result)
    (with-current-buffer (get-buffer-create "*sqls result*")
      (erase-buffer)
      (insert result)
      (display-buffer (current-buffer))))

  (defun eglot/sqls-execute-command ()
    (interactive)
    (let* ((server (eglot-current-server))
           (command "executeQuery")
           (arguments (concat "file://" (buffer-file-name)))
           (beg (eglot--pos-to-lsp-position (if (use-region-p) (region-beginning) (point-min))))
           (end (eglot--pos-to-lsp-position (if (use-region-p) (region-end) (point-max)))))
      (eglot/sqls-show-results
       (jsonrpc-request
        server
        :workspace/executeCommand
        `(
          :command ,(format "%s" command)
          :arguments [,arguments]
          :timeout 0.5
          :range (:start ,beg :end ,end))))))

  (defun eglot/sqls-select-and-execute-command ()
    (interactive)
    (call-interactively 'sql-beginning-of-statement)
    (call-interactively 'set-mark-command)
    (call-interactively 'sql-end-of-statement)
    (eglot/sqls-execute-command)
    (deactivate-mark))
  
  (defun sql/hook ()
    (interactive)
    (eglot-ensure)
    (define-key sql-mode-map (kbd "C-c C-c") 'eglot/sqls-select-and-execute-command))
  (add-hook 'sql-mode-hook 'sql/hook))

(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode)
  (setq eglot-booster-no-remote-boost t))

(use-package dape
  :ensure (:host github :repo "svaante/dape")
  :hook
  ((kill-emacs . dape-breakpoint-save)
   (after-init . dape-breakpoint-load))

  :init
  (setq
   dape-buffer-window-arrangement 'left
   dape-info-hide-mode-line nil
   dape-info-buffer-window-groups nil)

  :config
  ;; Global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode))

(use-package expreg
  :config
  (global-set-key (kbd "C-=") 'expreg-expand)
  (global-set-key (kbd "C-`") 'expreg-contract))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(defun utils/get-project-root-if-wanted ()
  (interactive)
  (let ((cur-buffer (window-buffer (selected-window))))
    (with-current-buffer cur-buffer
      (if (derived-mode-p 'dired-mode)
          (replace-regexp-in-string "^[Directory ]*" "" (pwd))
        (let ((project-root (consult--project-root)))
          (if project-root
              project-root
            (let ((current-file-name (buffer-file-name cur-buffer)))
              (if current-file-name
                  current-file-name
                ""))))))))

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
  (set-face-attribute 'eshell-prompt nil :weight 'ultra-bold :inherit 'minibuffer-prompt)
  (eat-eshell-mode 1)
  (eat-eshell-visual-command-mode 1)
  (display-line-numbers-mode 0))
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
  "Open a new instance of eshell."
  (interactive)
  (let* ((default-directory (replace-regexp-in-string "/$" "" (utils/get-project-root-if-wanted)))
         (eshell-buffer (eshell/get-relevant-buffer default-directory)))
    (if eshell-buffer
        (switch-to-buffer eshell-buffer)
      (eshell 'N))))

(global-set-key (kbd "C-c t") #'eshell/new-or-current)

(use-package eshell
  :ensure nil)

(use-package eat
  :config
  (setq eshell-visual-commands '())

  (defun start-file-process-shell-command-using-eat-exec
      (name buffer command)
    (require 'eat)
    (with-current-buffer (eat-exec buffer name "bash" nil (list "-ilc" command))
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
                    (error "The %s process is not running" (downcase mode-name)))))))

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

(use-package dired-subtree
  :bind (
         :map dired-mode-map
         ("C-<tab>" . dired-subtree-cycle)
         ("<tab>" . dired-subtree-toggle)
         ("<backtab>" . dired-subtree-remove)))

(use-package dired-hide-dotfiles
  :hook
  (dired-mode . dired-hide-dotfiles-mode))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

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
  :ensure t
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight)))

(global-set-key (kbd "M-s M-i") 'eww)
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

  (setq eww-search-prefix "https://www.google.com/search?ie=utf-8&oe=utf-8&q=")
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

    (add-hook 'eww-after-render-hook #'mixed-pitch-mode)
    (add-hook 'eww-after-render-hook 'eww/rename-buffer)
    (add-hook 'eww-after-render-hook (lambda () (visual-line-mode 1)))))

(add-hook 'elpaca-after-init-hook
      #'(lambda ()
  	(let ((local-settings "~/.emacs.d/local.el"))
  	  (when (file-exists-p local-settings)
  	    (load-file local-settings)))))
