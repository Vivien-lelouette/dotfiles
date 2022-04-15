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

(use-package gcmh
  :config
  (gcmh-mode 1))

(setq large-file-warning-threshold 100000000)

(setq read-process-output-max (* 5 (* 1024 1024)))

(setq tab-always-indent 'complete)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq xref-prompt-for-identifier nil)

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

(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)
(require 'bind-key)
(bind-key* "C-x k" #'kill-current-buffer)
(bind-key* "C-x K" #'kill-buffer)
(global-set-key (kbd "C-z") 'delete-frame)

(setq bookmark-save-flag 1)

(setq display-buffer-base-action
  '((display-buffer-below-selected
     display-buffer-reuse-window
     display-buffer-reuse-mode-window
     display-buffer-same-window
     display-buffer-in-previous-window)))

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

(require 'iso-transl)

(use-package ace-window
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (setq
   aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0)
   aw-background nil
   aw-dispatch-always t
   aw-display-mode-overlay nil)
  (setq aw-dispatch-alist
        '((?x aw-delete-window "Delete Window")
          (?M aw-swap-window "Swap Windows")
          (?m aw-move-window "Move Window")
          (?c aw-copy-window "Copy Window")
          (?j aw-switch-buffer-in-window "Select Buffer")
          (?n aw-flip-window)
          (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
          (?c aw-split-window-fair "Split Fair Window")
          (?v aw-split-window-vert "Split Vert Window")
          (?b aw-split-window-horz "Split Horz Window")
          (?o delete-other-windows "Delete Other Windows")
          (?? aw-show-dispatch-help)))
  (ace-window-display-mode 1))

(use-package avy
  :config
  (require 'bind-key)
  (bind-key* "C-j" #'avy-goto-char-timer))

(use-package multiple-cursors
    :config
    (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-;") 'mc/mark-all-like-this)
    (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click))

(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region)
  (global-set-key (kbd "C--") 'er/contract-region))

(use-package undo-fu
  :config
  (global-unset-key (kbd "C-/"))
  (global-unset-key (kbd "C-?"))
  (global-set-key (kbd "C-/")   'undo-fu-only-undo)
  (global-set-key (kbd "C-?") 'undo-fu-only-redo))

(defvar-local hidden-mode-line-mode nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

(scroll-bar-mode 1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(setq
 window-divider-default-places t
 window-divider-default-right-width 1
 window-divider-default-bottom-width 1)
(window-divider-mode 1)

(set-face-attribute 'default nil :font "SauceCodePro NF")

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "SauceCodePro NF")

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :weight 'regular)

(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode))

(use-package textsize
  :commands textsize-mode
  :init (textsize-mode)
  :config
  (setq textsize-default-points 11))

(defun generate-colors-file ()
  "Function to generate my colors file."
  (interactive)
  (delete-file "~/.colors")
  (append-to-file
   (concat
    "background="
    (face-background 'default)

    "\nbackground_alt="
    (face-background 'mode-line-inactive)

    "\nforeground="
    (face-foreground 'default)

    "\nforeground_alt="
    (face-foreground 'diff-context)

    "\nselected="
    (face-background 'region)

    "\nhighlight="
    (face-background 'cursor)

    "\nalert="
    (face-background 'trailing-whitespace)

    "\n"
    )

   nil

   "~/.colors"
   )
  )

(use-package theme-magic)

(defun custom/load-theme ()
  "Load a theme, generate my colors file and refresh my window manager."
  (interactive)
  (call-interactively 'load-theme)
  (run-with-timer 0.2 nil (lambda ()
                            (theme-magic-from-emacs)
                            (generate-colors-file)
                            (shell-command "wpg -i .wallpaper ~/.cache/wal/colors.json" nil nil)
                            (shell-command "wpg -s .wallpaper" nil nil)
                            (exwm/refresh-setup-and-monitors))))

(use-package doom-themes
  ;;:custom-face
  ;; (org-block ((t (:background "#272C36"))))
  ;; (org-block-begin-line ((t (:background "#272C36"))))
  ;; (org-block-end-line ((t (:background "#272C36"))))
  ;; (window-divider ((t (:foreground "#2e3440"))))
  ;; (window-divider-first-pixel ((t (:foreground "#2e3440"))))
  ;; (window-divider-last-pixel ((t (:foreground "#2e3440"))))
  ;; (hl-line ((t (:background "#434C5E"))))
  ;; :hook (server-after-make-frame . (lambda () (load-theme
  ;;                                            'doom-nord t)))
  :config
  (defun doom-themes-hide-modeline ())
  (doom-themes-org-config))

  ;;(defun darken-buffer ()
  ;;  (setq buffer-face-mode-face `(:background "#272C36"))
  ;;  (face-remap-add-relative 'hl-line `(:background "#2e3440"))
  ;;  (face-remap-add-relative 'fringe `(:background "#272C36"))
  ;;  (buffer-face-mode 1))

  ;;(add-hook 'help-mode-hook #'darken-buffer)
  ;;(add-hook 'helpful-mode-hook #'darken-buffer)

(use-package modus-themes
  :init
  (setq modus-themes-mode-line '(borderless accented))
  (modus-themes-load-themes))

(defun custom/doom-modeline-start ()
  (interactive)
  (doom-modeline-mode 0)
  (setq doom-modeline-height 20
        doom-modeline-major-mode-icon nil
        doom-modeline-major-mode-color-icon nil)
  (set-face-attribute 'doom-modeline-bar nil :background (face-background 'mode-line))
  (set-face-attribute 'doom-modeline-bar-inactive nil :background (face-background 'mode-line-inactive))
  (set-face-attribute 'mode-line nil :height 100)
  (set-face-attribute 'mode-line-inactive nil :height 100)
  (column-number-mode 1)
  (doom-modeline-mode 1))

(use-package doom-modeline
  :hook (server-after-make-frame . custom/doom-modeline-start)
  :config
  (defun fw/s-truncate (len s &optional ellipsis)
    "Like `s-truncate' but
          - return S when LEN is nil
          - return empty string when len is shorter than ELLIPSIS"
    (declare (pure t) (side-effect-free t))
    (let ((ellipsis (or ellipsis "...")))
      (cond
       ((null len) s)
       ((< len (length ellipsis)) "")
       (t (s-truncate len s ellipsis)))))

  (defun fw/doom-modeline-segment--buffer-info (orig-fn &rest args)
    "`doom-modeline-segment--buffer-info' but truncate for EXWM buffers."
    (fw/s-truncate (max 15 (- (window-width) 50))
                   (format-mode-line (apply orig-fn args))
                   "..."))
  (advice-add #'doom-modeline-segment--buffer-info :around #'fw/doom-modeline-segment--buffer-info))

(use-package olivetti
  :config
  (setq olivetti-margin-width 120
        olivetti-minimum-body-width 120
        olivetti-body-width 120))

(use-package hideshow
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

(use-package pulsar
  :straight (pulsar :type git :host gitlab :repo "protesilaos/pulsar")
  :config
  (setq pulse-flag t)
  (pulsar-global-mode 1))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
  :after all-the-icons)

(defun custom/coding-faces ()
  (interactive)
  (set-face-attribute 'font-lock-keyword-face nil :weight 'ultra-bold)
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic :weight 'semi-light)
  (set-face-attribute 'font-lock-function-name-face nil :slant 'italic :weight 'semi-bold)
  (set-face-attribute 'font-lock-string-face nil :weight 'normal :slant 'italic))

(add-hook 'prog-mode-hook #'custom/coding-faces)

(use-package prism
  :defer t
  :config
  (setq prism-num-faces 16)

  (prism-set-colors
    :desaturations '(0) ; do not change---may lower the contrast ratio
    :lightens '(0)      ; same
    :colors (modus-themes-with-colors
              (list blue
                    fg-main
                    magenta
                    green
                    red-alt
                    cyan
                    cyan-alt
                    red-alt-other
                    magenta-alt
                    green-alt
                    cyan
                    blue-alt-other
                    blue-alt
                    yellow
                    green-alt-other
                    fg-special-warm))))

(use-package ediff
    :straight (:type built-in)
    :custom
    ((ediff-window-setup-function 'ediff-setup-windows-plain)
     (ediff-diff-options "-w")
     (ediff-split-window-function 'split-window-horizontally)))

(use-package sudo-edit)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1
        which-key-popup-type 'side-window)
  ;; TODO Pretty damn ugly, must understand the correct way to customize
  (defun which-key--side-window-max-dimensions ()
    (cons
     ;; height
     5
     ;; width
     (window-width)))

  (defun which-key--show-buffer-side-window (act-popup-dim)
    "Show which-key buffer when popup type is side-window."
    (when (and which-key-preserve-window-configuration
               (not which-key--saved-window-configuration))
      (setq which-key--saved-window-configuration (current-window-configuration)))
    (let* ((height (car act-popup-dim))
           (alist
            `((window-height . 6)
              )))
      (display-buffer-below-selected which-key--buffer alist))))

(use-package ibuffer-vc)

(use-package zoom
  :custom
  (zoom-size '(0.618 . 0.618)))

(use-package vertico
    :straight (vertico :type git :host github :repo "minad/vertico")
    :config
    (load-file "~/.emacs.d/straight/build/vertico/extensions/vertico-buffer.el")
    (setq
     vertico-cycle t
     vertico-buffer-display-action '(display-buffer-below-selected (window-height . 10)))
    (add-hook 'minibuffer-setup-hook 'hidden-mode-line-mode)
    (vertico-mode)
    (vertico-buffer-mode))

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; You may want to enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  :init
  (corfu-global-mode))

(use-package embark
  :bind (
         :map minibuffer-local-map
         ("C-c e" . embark-act)))

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
  (setq consult-narrow-key "<")) ;; (kbd "C-+")

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
;;(setq completion-in-region-function
;;  (lambda (&rest args)
;;    (apply (if vertico-mode
;;               #'consult-completion-in-region
;;             #'completion--in-region)
;;           args))))

(use-package embark-consult)

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
  :bind (
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
  (add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'comnpletion-at-point-functions #'cape-dict)
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

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-parentheses
  :config
  (global-highlight-parentheses-mode 1))

(setq electric-pair-pairs
  '(
    (?\' . ?\')
    (?\" . ?\")
    (?\[ . ?\])
    (?\{ . ?\})))
(electric-pair-mode 1)

(use-package aggressive-indent
    :config
    (add-to-list 'aggressive-indent-dont-indent-if
                 '(and (eq (char-before) ?\s) (looking-at-p "$")))
    (add-to-list 'aggressive-indent-dont-indent-if
                 '(minibufferp))
    (add-to-list 'aggressive-indent-excluded-modes 'yaml-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'eshell-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'comint-mode)
    (global-aggressive-indent-mode 1))

(use-package magit
  :config
  (setq transient-display-buffer-action 'display-buffer-below-selected)
  (defun magit/magit-status-no-split ()
    "Don't split window."
    (interactive)
    (let ((magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))
      (magit-status)))
  (global-set-key (kbd "C-x G") 'magit/magit-status-no-split))

(use-package forge)

(use-package code-review
  :bind (
         :map forge-topic-mode-map
         ("C-c r" . code-review-forge-pr-at-point)
         ("C-c C-n" . code-review-comment-jump-next)
         ("C-c C-p" . code-review-comment-jump-previous)))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package consult-yasnippet)

(use-package nodejs-repl
  :config
  (add-hook 'js-mode-hook
    (lambda ()
      (define-key js-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
      (define-key js-mode-map (kbd "C-c C-j") 'nodejs-repl-send-line)
      (define-key js-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
      (define-key js-mode-map (kbd "C-c C-c") 'nodejs-repl-send-buffer)
      (define-key js-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
      (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl))))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (add-hook 'typescript-mode-hook #'lsp))

(use-package lsp-mode
  :straight (lsp-mode :type git :host github :repo "emacs-lsp/lsp-mode")
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/dev/stderr"))
  :bind (
         :map lsp-mode-map
         ("C-h ." . lsp-describe-thing-at-point)
         ("C-." . lsp-execute-code-action)
         ("M-." . lsp-find-definition))
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (js-mode . lsp)
         ;; if you want which-key integration
         ;;(lsp-mode . (lambda () (add-hook 'before-save-hook #'lsp-format-buffer)))
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (with-eval-after-load 'js
    (define-key js-mode-map (kbd "M-.") nil)
    )
  (setq
   lsp-log-io nil
   lsp-completion-provide :none
   lsp-eldoc-render-all nil
   lsp-eslint-auto-fix-on-save t
   lsp-auto-guess-root t
   lsp-log-io nil
   lsp-restart 'auto-restart
   lsp-enable-symbol-highlighting t
   lsp-enable-on-type-formatting nil
   lsp-signature-auto-activate nil
   lsp-signature-render-documentation nil
   lsp-eldoc-hook nil
   lsp-headerline-breadcrumb-enable nil
   lsp-semantic-tokens-enable nil
   lsp-enable-folding nil
   lsp-enable-snippet nil
   lsp-idle-delay 0.5)
  (defun lsp--eslint-before-save (orig-fun)  
    "Run lsp-eslint-apply-all-fixes and then run the original lsp--before-save."  
    (when lsp-eslint-auto-fix-on-save (lsp-eslint-fix-all))  
    (funcall orig-fun))
  (advice-add 'lsp--before-save :around #'lsp--eslint-before-save))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-doc-header t
        lsp-ui-doc-include-signature t
        lsp-ui-doc-border (face-foreground 'default)
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-delay 0.05))

(use-package lsp-ltex
  :hook (text-mode . (lambda ()
                       (require 'lsp-ltex)
                       (lsp))))

(use-package dap-mode
  :straight (dap-mode :type git :host github :repo "emacs-lsp/dap-mode"))

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

(use-package aweshell
  :straight (aweshell :type git :host github :repo "manateelazycat/aweshell"))

(defun custom/vterm-auto-copy-mode (buffer description)
  (with-current-buffer buffer
    (vterm-copy-mode nil)))

(use-package vterm
  :config
  (setq
   vterm-shell "/bin/zsh"
   vterm-buffer-name-string "vterm: %s")
  (add-hook 'vterm-exit-functions #'custom/vterm-auto-copy-mode))

(use-package eshell-vterm
  :config
  (require 'vterm)
  (defalias 'eshell/v 'eshell-exec-visual)
  (eshell-vterm-mode))

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
         :map dired-mode-map
         ("C-." . dired-hide-dotfiles-mode)
         ("<C-return>" . dired-open-file)
         ("M-p" . dired-up-directory)
         ("M-n" . dired-find-file))
  :hook
  (dired-mode . dired-hide-details-mode)
  :config
  (setq ls-lisp-use-insert-directory-program nil)
  (require 'ls-lisp)
  (setq ls-lisp-dirs-first t))

(use-package dired-subtree
  :bind (
         :map dired-mode-map
         ("C-<tab>" . dired-subtree-cycle)
         ("<tab>" . dired-subtree-toggle)
         ("<backtab>" . dired-subtree-remove)))

(use-package dired-hide-dotfiles
  :hook
  (dired-mode . dired-hide-dotfiles-mode))

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
      (expand-file-name "~/dotfiles/kmonad/README.org"))
    (string-equal (buffer-file-name)
      (expand-file-name "~/dotfiles/emacs/local.org")))
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

(use-package shr
  :straight (:type built-in)
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
        (insert (propertize "#+END_SRC" 'face 'org-block-end-line ))
        (shr-ensure-newline)
        (insert "\n"))))

(use-package eww
  :straight (:type built-in)
  :bind (
         :map eww-mode-map
         ("M-r" . eww/open-in-eaf))
  :config
  (require 'shrface)
  (defun eww/rename-buffer ()
    "Rename `eww-mode' buffer so sites open in new page.
URL `http://xahlee.info/emacs/emacs/emacs_eww_web_browser.html'
Version 2017-11-10"
    (let (($title (plist-get eww-data :title)))
      (when (eq major-mode 'eww-mode )
        (if $title
            (rename-buffer $title t)
          (rename-buffer "eww" t)))))

  (add-hook 'eww-after-render-hook 'eww/rename-buffer)
  (add-hook 'eww-after-render-hook #'shrface-mode)
  (add-hook 'eww-after-render-hook #'mixed-pitch-mode)
  (add-hook 'eww-after-render-hook #'olivetti-mode))

(setq gnus-use-full-window nil)

(when window-system
  (setq gnus-sum-thread-tree-indent "  ")
  (setq gnus-sum-thread-tree-root "")
  (setq gnus-sum-thread-tree-false-root "")
  (setq gnus-sum-thread-tree-single-indent "")
  (setq gnus-sum-thread-tree-vertical        "│")
  (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
  (setq gnus-sum-thread-tree-single-leaf     "╰─► "))
(setq gnus-summary-line-format
      (concat
       "%0{%U%R%z%}"
       "%3{│%}" "%1{%d%}" "%3{│%}" ;; date
       "  "
       "%4{%-20,20f%}"               ;; name
       "  "
       "%3{│%}"
       " "
       "%1{%B%}"
       "%s\n"))
(setq gnus-summary-display-arrow t)

(use-package bbdb
  :config
  (require 'bbdb-autoloads)
  (setq bbdb-file "~/.bbdb"
        bbdb-offer-save 'auto
        bbdb-notice-auto-save-file t
        bbdb-expand-mail-aliases t
        bbdb-canonicalize-redundant-nets-p t
        bbdb-always-add-addresses t
        bbdb-complete-name-allow-cycling t
        bbdb-mua-pop-up nil
        bbdb-mua-auto-update-p 'create
        bbdb-message-all-addresses t)
  (bbdb-initialize 'gnus 'message)
  (bbdb-mua-auto-update-init 'gnus 'message))

(let ((local-settings "~/.emacs.d/local.el"))
    (when (file-exists-p local-settings)
  (load-file local-settings)))

(autoload 'exwm-enable "~/.emacs.d/desktop.el")
