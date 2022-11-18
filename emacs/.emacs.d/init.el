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

(add-hook 'after-init-hook
          #'(lambda ()
              (setq gc-cons-threshold (* 100 1000 1000))))

(setq large-file-warning-threshold 100000000)

(setq read-process-output-max (* 5 (* 1024 1024)))
(setq process-adaptive-read-buffering nil)

(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-use-momentum t)

(setq tab-always-indent t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq xref-prompt-for-identifier nil)
(setq comint-prompt-read-only t)

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
(delete-selection-mode 1)

(setq bookmark-save-flag 1)

(setq indent-tabs-mode nil)
(setq indent-line-function 'insert-tab)
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

(setq warning-minimum-level :error)

(repeat-mode 1)

(require 'iso-transl)

(use-package god-mode
  :config
  (setq god-mode-alist '((nil . "C-") ("z" . "M-") ("Z" . "C-M-")))
  (setq god-exempt-predicates '(god-exempt-mode-p))
  (add-to-list 'god-exempt-major-modes 'magit-mode)
  (add-to-list 'god-exempt-major-modes 'magit-status-mode)
  (add-to-list 'god-exempt-major-modes 'magit-diff-mode)
  (add-to-list 'god-exempt-major-modes 'bookmark-bmenu-mode)
  (setq god-exempt-major-modes (remove 'compilation-mode god-exempt-major-modes))

  (define-key god-local-mode-map (kbd ".") #'repeat)
  ;; (global-set-key (kbd "<escape>") #'god-mode-all)
  (define-key god-local-mode-map (kbd "[") #'backward-paragraph)
  (define-key god-local-mode-map (kbd "]") #'forward-paragraph)
  (require 'god-mode-isearch)
  (define-key isearch-mode-map (kbd "<escape>") #'god-mode-isearch-activate)
  (define-key god-mode-isearch-map (kbd "<escape>") #'god-mode-isearch-disable)

  (defun my-god-mode-update-cursor-type ()
    (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))
  (add-hook 'post-command-hook #'my-god-mode-update-cursor-type))

(use-package ace-window
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (setq
   aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0)
   aw-background nil
   aw-dispatch-always t
   aw-display-mode-overlay nil
   aw-minibuffer-flag t)
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
  (defun aw-update ()
    "Update ace-window-path window parameter for all windows.

Ensure all windows are labeled so the user can select a specific
one, even from the set of windows typically ignored when making a
window list."
    (let ((aw-ignore-on)
          (aw-ignore-current)
          (ignore-window-parameters t))
      (avy-traverse
       (avy-tree (aw-window-list) aw-keys)
       (lambda (path leaf)
         (set-window-parameter
          leaf 'ace-window-path
          (propertize
           (concat " " (apply #'string (reverse path)))
           'face 'aw-mode-line-face))))))
  (ace-window-display-mode 1))

(use-package avy
  :config
  (require 'bind-key)
  (bind-key "M-j" #'avy-goto-char-timer))

(use-package multiple-cursors
    :config
    (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-;") 'mc/mark-all-like-this)
    (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
    (setq mc/black-list-prefer t))

(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region)
  (global-set-key (kbd "C--") 'er/contract-region))

(use-package vundo
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  (global-unset-key (kbd "C-?"))
  (global-set-key (kbd "C-?") 'vundo))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(setq
 window-divider-default-places t
 window-divider-default-right-width 1
 window-divider-default-bottom-width 1)
(window-divider-mode -1)

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
  (text-mode . mixed-pitch-mode)
  (yaml-mode . disable-mixed-pitch))

(use-package dracula-theme
  :config
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
  (load-theme 'dracula t)
  (setq dracula-use-24-bit-colors-on-256-colors-terms t))

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

(add-hook 'prog-mode-hook #'subword-mode)
(defun custom/coding-faces ()
  (interactive)
  (set-face-attribute 'font-lock-keyword-face nil :weight 'ultra-bold)
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic :weight 'normal)
  (set-face-attribute 'font-lock-function-name-face nil :slant 'italic :weight 'semi-bold)
  (set-face-attribute 'font-lock-string-face nil :weight 'normal :slant 'italic))

(add-hook 'prog-mode-hook #'custom/coding-faces)

(use-package prism
  :straight (prism :type git :host github :repo "alphapapa/prism.el")
  :defer t
  :config
  (setq prism-num-faces 8
        prism-desaturations '(0 1 2)
        prism-lightens '(0 1 2)))

(use-package ediff
    :straight (:type built-in)
    :custom
    ((ediff-window-setup-function 'ediff-setup-windows-plain)
     (ediff-diff-options "-w")
     (ediff-split-window-function 'split-window-horizontally)))

(use-package sudo-edit)

(use-package explain-pause-mode
  :straight (explain-pause-mode :type git :host github :repo "lastquestion/explain-pause-mode")
  :config
  (explain-pause-mode))

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

(use-package whole-line-or-region
  :config
  (whole-line-or-region-global-mode 1))

(use-package ibuffer-vc)

(use-package zoom
  :custom
  (zoom-size '(0.55 . 0.55)))

(defun window/4k-layout ()
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (split-window-right)
  (other-window 1)
  (split-window)
  (zoom))

(defun window/toggle-pin ()
  (interactive)
  (if (window-parameter (selected-window) 'split-window)
      (progn 
        (set-window-parameter nil 'split-window nil)
        (setq-local window-size-fixed nil)
        (set-window-dedicated-p (selected-window) nil)
        (rename-buffer (string-trim-left (buffer-name)))
        (message "Window unpined"))
    (progn
      (set-window-parameter nil 'split-window #'ignore)
        (setq-local window-size-fixed 'width)
      (set-window-dedicated-p (selected-window) t)
      (rename-buffer (concat " " (buffer-name)))
      (message "Window pined"))))

(global-set-key (kbd "C-c w p") #'window/toggle-pin)

(global-set-key (kbd "C-c w l 4") #'window/4k-layout)

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

(setq tab-always-indent 'complete)
(setq completions-format 'one-column)
(setq completions-header-format nil)
(setq completion-show-help nil)
(setq completions-max-height 10)
(setq completion-auto-select nil)
(setq completion-show-inline-help nil)

(defun my/minibuffer-choose-completion (&optional repeat-key no-exit no-quit)
  (interactive)
  (let ((inhibit-message t)
        (message-log-max nil))
    (if (minibufferp)
        (if (get-buffer-window "*Completions*")
            (progn (minibuffer-previous-completion)
                   (let ((minibuffer-completion-auto-choose t))
                     (minibuffer-next-completion)))
          (call-interactively 'minibuffer-force-complete-and-exit))
      (if (get-buffer-window "*Completions*")
          (with-minibuffer-completions-window
            (let ((completion-use-base-affixes nil))
              (choose-completion nil no-exit no-quit)))
        (progn 
          (completion-in-region-mode -1)
          (if (not (null repeat-key))
              (execute-kbd-macro (kbd repeat-key))))))))

(define-key completion-in-region-mode-map (kbd "C-s") (lambda ()
                                                        (interactive)
                                                        (if (get-buffer-window "*Completions*")
                                                            (minibuffer-next-completion)
                                                          (progn 
                                                            (completion-in-region-mode -1)
                                                            (call-interactively 'isearch-forward)))))

(define-key completion-in-region-mode-map (kbd "C-r") (lambda ()
                                                        (interactive)
                                                        (if (get-buffer-window "*Completions*")
                                                            (minibuffer-previous-completion)
                                                          (progn
                                                            (completion-in-region-mode -1)
                                                            (call-interactively 'isearch-backward)))))

(define-key completion-in-region-mode-map (kbd "RET") (lambda () (interactive) (my/minibuffer-choose-completion "RET")))
(define-key completion-in-region-mode-map (kbd "M-RET") (lambda () (interactive) (completion-in-region-mode -1) (execute-kbd-macro (kbd "RET"))))

(add-hook 'completion-list-mode-hook (lambda () (setq truncate-lines t)))

(defun utils/advice-silence-messages (orig-fun &rest args)
  "Advice function that silences all messages in ORIG-FUN."
  (let ((inhibit-message t)      ;Don't show the messages in Echo area
        (message-log-max nil))   ;Don't show the messages in the *Messages* buffer
    (apply orig-fun args)))

(defun complete/thing-at-point ()
  (if (minibufferp)
      (thing-at-point 'line 'no-properties)
    (with-syntax-table (make-syntax-table (syntax-table)) (modify-syntax-entry ?. "_")  (thing-at-point 'symbol 'no-properties))))

(defun complete/update-in-region ()
  (let ((current-word (complete/thing-at-point))
        (inhibit-message t)
        (message-log-max nil))
    (if (and
         (boundp 'complete/need-completion)
         complete/need-completion)
        (while-no-input
          (redisplay)
          (unless (memq this-command '(minibuffer-complete-and-exit
                                       minibuffer-force-complete-and-exit
                                       completion-at-point
                                       choose-completion))

            (if (minibufferp)
                (minibuffer-completion-help)
              (if (and current-word
                       (>= (string-width current-word) 2))
                  (completion-help-at-point)
                (completion-in-region-mode -1)))
            (minibuffer-previous-completion)
            (minibuffer-next-completion)
            (setq-local complete/need-completion nil))))))

(defun complete/buffer-has-changed (&rest _args)
  (setq-local complete/need-completion t))

(defun complete/start ()
  (interactive)
  (let ((inhibit-message t)
        (message-log-max nil))
    (if (minibufferp)
        (progn (define-key minibuffer-mode-map (kbd "C-s") 'minibuffer-next-completion)
               (define-key minibuffer-mode-map (kbd "C-r") 'minibuffer-previous-completion)
               (define-key minibuffer-mode-map (kbd "C-<tab>") 'my/minibuffer-choose-completion)
               (define-key minibuffer-mode-map (kbd "M-<tab>") 'my/minibuffer-choose-completion)
               (define-key minibuffer-mode-map (kbd "C-<return>") 'minibuffer-complete-and-exit)
               (define-key minibuffer-mode-map (kbd "M-<return>") 'minibuffer-complete-and-exit)
               (define-key minibuffer-mode-map (kbd "<return>") (lambda () (interactive) (my/minibuffer-choose-completion) (minibuffer-complete-and-exit)))
               (minibuffer-completion-help)))
    (remove-hook 'after-change-functions #'complete/buffer-has-changed t)
    (add-hook 'after-change-functions #'complete/buffer-has-changed nil t)))

(setq minibuffer-completion-auto-choose nil)
(run-with-idle-timer 0.2 t #'complete/update-in-region)
(add-hook 'minibuffer-setup-hook #'complete/start)

(use-package vcomplete
  :config
  (add-hook 'completion-list-mode-hook (lambda () (font-lock-mode -1)))
  (defun vcomplete--highlight-completion-at-point ())
  ;; Fix pointer position for consult groups
  (defun vcomplete--update-minibuffer (&rest _args)
    "Update the completion list when completing in a minibuffer."
    (while-no-input
      (redisplay)
      (unless (memq this-command vcomplete-no-update-commands)
        (minibuffer-completion-help)))
    (vcomplete-next-completion 2)
    (vcomplete-prev-completion))

  (define-key vcomplete-command-map (kbd "C-<return>") 'vcomplete-choose-completion))

(use-package embark
  :bind (
         :map minibuffer-local-map
         ("C-c e" . embark-act)))

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
  ;; (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

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
         ("M-A" . marginalia-cycle)))

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
  ;; Silence the pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package wgrep)

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

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package flymake-eslint)

(setq electric-pair-pairs
  '(
    (?\' . ?\')
    (?\" . ?\")
    (?\[ . ?\])
    (?\{ . ?\})))
(electric-pair-mode 1)

(electric-indent-mode 0)
(use-package aggressive-indent
    :config
    (add-to-list 'aggressive-indent-dont-indent-if
                 '(and (eq (char-before) ?\s) (looking-at-p "$")))
    (add-to-list 'aggressive-indent-dont-indent-if
                 '(minibufferp))
    (add-to-list 'aggressive-indent-excluded-modes 'yaml-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'eshell-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'comint-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'authinfo-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'term-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'ansi-term-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'sql-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'helm-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'helm-occur-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'helm-epa-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'helm-major-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'completion-list-mode)
    (global-aggressive-indent-mode 1))

(use-package magit
  :config
  (setq transient-display-buffer-action '(display-buffer-below-selected))
  (defun magit/magit-status-no-split ()
    "Don't split window."
    (interactive)
    (let ((magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))
      (magit-status)))
  (global-unset-key (kbd "C-x g"))
  (global-set-key (kbd "C-x g s") #'magit-status)
  (global-set-key (kbd "C-x g c") #'magit-clone)
  (global-set-key (kbd "C-x g g") #'magit/magit-status-no-split))

(use-package forge)

(use-package code-review
  :bind (
         :map forge-topic-mode-map
         ("C-c r" . code-review-forge-pr-at-point)
         ("C-c C-n" . code-review-comment-jump-next)
         ("C-c C-p" . code-review-comment-jump-previous)))

(use-package tempel
  :bind (("C-<tab>" . tempel-complete))
  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

(use-package nodejs-repl)

(use-package typescript-mode
  :mode "\\.ts\\'")

(use-package jest-test-mode 
  :commands jest-test-mode
  :hook (typescript-mode js-mode typescript-tsx-mode))

(use-package apheleia
  :straight (apheleia :host github :repo "raxod502/apheleia")
  :config
  (setf (alist-get 'prettier apheleia-formatters)
        '(npx "eslint" "--fix" file))
  (add-to-list 'apheleia-mode-alist '(js-mode . prettier))
  (apheleia-global-mode t))

(use-package tree-sitter
  :ensure t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package combobulate
  :straight (combobulate :type git :host github :repo "mickeynp/combobulate")
  :hook ((python-mode . combobulate-mode)
         (js-mode . combobulate-mode)
         (typescript-mode . combobulate-mode))
  :load-path "~/.emacs.d/straight/repos/combobulate/combobulate.el"
  :config
  (setq combobulate-flash-node nil))

(use-package eglot
  :defer t
  :straight (:type built-in)
  :bind (:map eglot-mode-map
              ("C-." . eglot-code-actions))
  :config

  (defun js/hook ()
    (interactive)
    (eglot-ensure)
    (define-key js-mode-map (kbd "M-.") 'xref-find-definitions)
    (define-key js-mode-map (kbd "M-?") 'xref-find-references)
    (define-key js-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
    (define-key js-mode-map (kbd "C-c C-j") 'nodejs-repl-send-line)
    (define-key js-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
    (define-key js-mode-map (kbd "C-c C-c") 'nodejs-repl-send-buffer)
    (define-key js-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
    (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl))

  (defun sql/hook ()
    (interactive)
    (eglot-ensure)
    (define-key sql-mode-map (kbd "C-x C-e") 'eglot/sqls-select-and-execute-command))

  (defun eglot/after-connect ()
    (interactive)
    (complete/start)
    (setq-local completion-at-point-functions (list (cape-super-capf #'tempel-complete #'eglot-completion-at-point) t))
    (if (or (derived-mode-p 'js-mode) (derived-mode-p 'typescript-mode))
                                   (flymake-eslint-enable)))

  (add-hook 'eglot-managed-mode-hook 'eglot/after-connect)
  (add-hook 'typescript-mode-hook 'js/hook)
  (add-hook 'js-mode-hook 'js/hook)
  (add-hook 'sql-mode-hook 'sql/hook)
  (setq 
   eglot-events-buffer-size 0
   eldoc-echo-area-use-multiline-p nil
   eglot-ignored-server-capabilities '(:documentHighlightProvider))

  (delete '((js-mode typescript-mode)
            "typescript-language-server" "--stdio") eglot-server-programs)
  (add-to-list 'eglot-server-programs
               '((js-mode typescript-mode)
                 "typescript-language-server" "--stdio" "--tsserver-path" "/nix/store/0jshaillr4zq0ml575jjnj1xabmlf3m9-typescript-4.8.4/lib/node_modules/typescript"))

  (defclass eglot-sqls (eglot-lsp-server) () :documentation "SQL's Language Server")
  (add-to-list 'eglot-server-programs '(sql-mode . (eglot-sqls "sqls")))

  (defun eglot/sqls-select-and-execute-command ()
    (interactive)
    (call-interactively 'sql-beginning-of-statement)
    (call-interactively 'set-mark-command)
    (call-interactively 'sql-end-of-statement)
    (eglot/sqls-execute-command)
    (deactivate-mark))

  (defun eglot/sqls-execute-command ()
    (interactive)
    (let* ((server (eglot-current-server))
           (command "executeQuery")
           (arguments (concat "file://" (buffer-file-name)))
           (beg (eglot--pos-to-lsp-position (if (use-region-p) (region-beginning) (point-min))))
           (end (eglot--pos-to-lsp-position (if (use-region-p) (region-end) (point-max))))
           (res (jsonrpc-request server :workspace/executeCommand
                                 `(:command ,(format "%s" command) :arguments [,arguments]
                                            :timeout 0.5 :range (:start ,beg :end ,end))))
           (buffer (get-buffer-create "*sqls*")))
      (with-current-buffer buffer
        (erase-buffer)
        (eglot--apply-text-edits `[
                                   (:range
                                    (:start
                                     (:line 0 :character 0)
                                     :end
                                     (:line 0 :character 0))
                                    :newText ,res)
                                   ]
                                 )
        (beginning-of-buffer)
        (replace-regexp "+$" "|")
        (beginning-of-buffer)
        (replace-regexp "^+" "|")
        (beginning-of-buffer)
        (org-mode)
        (org-toggle-pretty-entities)
        (mixed-pitch-mode -1)
        (toggle-truncate-lines 1)
        (god-local-mode 1))
      (display-buffer-below-selected buffer '())
      ))

  (cl-defmethod eglot-execute-command
    ((server eglot-sqls) (command (eql executeQuery)) arguments)
    "For executeQuery."
    (eglot/sqls-execute-command))

  (cl-defmethod eglot-execute-command
    ((server eglot-sqls) (_cmd (eql switchDatabase)) arguments)
    "For switchDatabase."
    (let* ((res (jsonrpc-request server :workspace/executeCommand
                                 `(:command "showDatabases" :arguments ,arguments :timeout 0.5)))
           (menu-items (split-string res "\n"))
           (menu `("Eglot code actions:" ("dummy" ,@menu-items)))
           (db (if (listp last-nonmenu-event)
                   (x-popup-menu last-nonmenu-event menu)
                 (completing-read "[eglot] Pick an database: "
                                  menu-items nil t
                                  nil nil (car menu-items))
                 ))
           )
      (jsonrpc-request server :workspace/executeCommand
                       `(:command "switchDatabase" :arguments [,db] :timeout 0.5))
      ))

  (cl-defmethod eglot-execute-command
    ((server eglot-sqls) (_cmd (eql switchConnections)) arguments)
    "For switchConnection."
    (let* ((res (jsonrpc-request server :workspace/executeCommand
                                 `(:command "showConnections" :arguments ,arguments :timeout 0.5)))
           (menu-items (split-string res "\n"))
           (menu `("Eglot code actions:" ("dummy" ,@menu-items)))
           (conn (if (listp last-nonmenu-event)
                     (x-popup-menu last-nonmenu-event menu)
                   (completing-read "[eglot] Pick a connection: "
                                    menu-items nil t
                                    nil nil (car menu-items))
                   ))
           )
      (jsonrpc-request server :workspace/executeCommand
                       `(:command "switchConnections" :arguments [,(nth 0 (split-string conn))] :timeout 0.5))
      )))

(use-package realgud)

(use-package realgud-trepan-ni)

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

(use-package aweshell
  :straight (aweshell :type git :host github :repo "manateelazycat/aweshell")
  :config
  (define-key eshell-mode-map (kbd "M-m") #'eshell-bol)
  (require 'eshell)
  (require 'em-smart)
  (setq 
   eshell-where-to-jump 'begin
   eshell-banner-message ""
   eshell-review-quick-commands nil
   eshell-smart-space-goes-to-end t)
   (defun eshell/hook ()
     (aweshell-sync-dir-buffer-name)
     (complete/start))
  ;;   (eshell/alias "ll" "ls --group-directories-first --color -l $*")
  ;;   (eshell/alias "docker-all-stop" "docker ps -aq | xargs docker stop")
  ;;   (eshell/alias "da-stop" "docker ps -aq | xargs docker stop"))
  (add-hook 'eshell-mode-hook #'eshell/hook)
  (setq eshell-prompt-function
        (lambda ()
          (concat (format-time-string "%Y-%m-%d %H:%M" (current-time))
                  (if (= (user-uid) 0) " # " " $ ")))))

(defun eshell/emacs (file)
  (find-file file))

(use-package multi-term
  :bind (
         :map term-mode-map
         ("s-I" . term-char-mode))
  :config
  (defun term-send-tab ()
    (interactive)
    (term-send-raw-string "\t"))

  (setq multi-term-program "bash")

  (add-to-list 'term-bind-key-alist '("<backtab>" . term-send-up))
  (add-to-list 'term-bind-key-alist '("TAB" . term-send-tab))
  (add-to-list 'term-bind-key-alist '("s-i" . term-line-mode)))

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
         ("M-n" . dired-find-file)
         ("s-i" . dired-toggle-read-only))
  :hook
  (dired-mode . dired-hide-details-mode)
  :config
  (setq ls-lisp-use-insert-directory-program nil)
  (require 'ls-lisp)
  (setq ls-lisp-dirs-first t)
  (setq wdired-allow-to-change-permissions t)
  (add-hook 'wdired-mode-hook
    (lambda ()
      (define-key wdired-mode-map (kbd "s-I") 'wdired-abort-changes))))

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
                            (expand-file-name "~/.dotfiles/README.org"))
              (string-equal (buffer-file-name)
                            (expand-file-name "~/.dotfiles/qutebrowser/README.org"))
              (string-equal (buffer-file-name)
                            (expand-file-name "~/.dotfiles/emacs/README.org"))
              (string-equal (buffer-file-name)
                            (expand-file-name "~/.dotfiles/emacs/desktop.org"))
              (string-equal (buffer-file-name)
                            (expand-file-name "~/.dotfiles/herbstluftwm/README.org"))
              (string-equal (buffer-file-name)
                            (expand-file-name "~/.dotfiles/rofi/README.org"))
              (string-equal (buffer-file-name)
                            (expand-file-name "~/.dotfiles/polybar/README.org"))
              (string-equal (buffer-file-name)
                            (expand-file-name "~/.dotfiles/kmonad/README.org"))
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

(use-package shr
  :straight (:type built-in)
  :config
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

(defun utils/window-with-buffer-prefix (prefix)
  "Returns the first window displaying a buffer starting with prefix"
  (seq-find (lambda (win) (string-prefix-p prefix (buffer-name (window-buffer win)))) (window-list)))

(setq gnus-use-full-window nil
      gnus-inhibit-images nil)

(add-hook 'gnus-startup-hook
          '(lambda ()
             (gnus-demon-init)
             (doom-modeline-start-gnus-listener)
             (setq gnus-demon-timestep 60)  ;; each timestep is 60 seconds
             ;; Check for new mail every 1 timestep (1 minute)
             (gnus-demon-add-handler 'gnus-demon-scan-news 1 t)
             (defun gnus-configure-windows (setting &optional force)
               (pcase setting
                 ('summary (let ((win (utils/window-with-buffer-prefix "*Summary")))
                             (if win
                                 (set-window-buffer win gnus-summary-buffer)
                               (set-window-buffer (selected-window) gnus-summary-buffer))
                             (select-window (get-buffer-window gnus-summary-buffer))))))

             ;; Don't crash gnus if disconnected
             (defadvice gnus-demon-scan-news (around gnus-demon-timeout activate)
               "Timeout for Gnus."
               (with-timeout
                   (120 (message "Gnus timed out."))
                 ad-do-it))))

(when window-system
  (setq )
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
       "%3{│%}" "%1{%-8,8d%}" "%3{│%}" ;; date
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

(autoload 'exwm-enable "~/.emacs.d/desktop.el")

(let ((local-settings "~/.emacs.d/local.el"))
    (when (file-exists-p local-settings)
  (load-file local-settings)))
