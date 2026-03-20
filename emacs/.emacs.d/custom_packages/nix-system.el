;;; nix-system.el --- Manage NixOS system from Emacs  -*- lexical-binding: t; -*-

;; Author: Vivien
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (transient "0.4"))
;; Keywords: tools nix nixos

;;; Commentary:

;; Transient-based interface for managing a NixOS system:
;; rebuild (switch/boot/test), flake update, generation management,
;; and package search.

;;; Code:

(require 'transient)

(defgroup nix-system nil
  "Manage NixOS system."
  :prefix "nix-system-"
  :group 'tools)

(defcustom nix-system-flake-dir "~/.nixos-configuration"
  "Path to the NixOS flake directory."
  :type 'directory
  :group 'nix-system)

(defcustom nix-system-hostname (system-name)
  "NixOS flake configuration name.
Derived from hostname, stripped of common prefixes."
  :type 'string
  :group 'nix-system)

(defun nix-system--hostname ()
  "Return the NixOS configuration name from the hostname.
Strips 'nixos-' prefix and '-<user>' suffix if present."
  (let ((name nix-system-hostname))
    (when (string-match "\\`nixos-\\(.*\\)" name)
      (setq name (match-string 1 name)))
    (when (string-match "\\(.*\\)-[^-]+\\'" name)
      (setq name (match-string 1 name)))
    name))

(defun nix-system--flake-ref ()
  "Return the flake reference for the current host."
  (format "%s#%s" (expand-file-name nix-system-flake-dir)
          (nix-system--hostname)))

(defun nix-system--run (name &rest args)
  "Run a nix command in a compilation buffer.
NAME is the buffer name suffix, ARGS is the command list."
  (let ((buf-name (format "*nix-system: %s*" name))
        (default-directory (expand-file-name nix-system-flake-dir))
        (cmd (string-join args " ")))
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (cd default-directory)
      (make-comint-in-buffer name buf-name shell-file-name nil "-c" cmd)
      (compilation-shell-minor-mode 1)
      (pop-to-buffer (current-buffer)))))

;;; Rebuild commands

(defun nix-system-rebuild-switch ()
  "Rebuild NixOS and switch to the new configuration."
  (interactive)
  (nix-system--run "rebuild switch"
                   "sudo" "nixos-rebuild" "switch" "--flake" (nix-system--flake-ref)))

(defun nix-system-rebuild-test ()
  "Rebuild NixOS and activate without adding a boot entry."
  (interactive)
  (nix-system--run "rebuild test"
                   "sudo" "nixos-rebuild" "test" "--flake" (nix-system--flake-ref)))

(defun nix-system-rebuild-boot ()
  "Rebuild NixOS for next boot without activating now."
  (interactive)
  (nix-system--run "rebuild boot"
                   "sudo" "nixos-rebuild" "boot" "--flake" (nix-system--flake-ref)))

(defun nix-system-rebuild-dry ()
  "Dry-run NixOS rebuild (build only, don't activate)."
  (interactive)
  (nix-system--run "rebuild dry"
                   "nixos-rebuild" "dry-build" "--flake" (nix-system--flake-ref)))

;;; Flake commands

(defun nix-system-flake-update ()
  "Update all flake inputs."
  (interactive)
  (nix-system--run "flake update" "nix" "flake" "update"))

(defun nix-system-flake-update-input (input)
  "Update a single flake INPUT."
  (interactive
   (list (completing-read "Input: " (nix-system--flake-inputs) nil t)))
  (nix-system--run (format "flake update %s" input)
                   "nix" "flake" "update" input))

(defun nix-system--flake-inputs ()
  "Return list of flake input names."
  (let ((flake-file (expand-file-name "flake.nix" nix-system-flake-dir)))
    (when (file-exists-p flake-file)
      (with-temp-buffer
        (insert-file-contents flake-file)
        (let (inputs)
          (while (re-search-forward "inputs\\.\\([a-zA-Z0-9_-]+\\)" nil t)
            (cl-pushnew (match-string 1) inputs :test #'string=))
          (nreverse inputs))))))

;;; Generation management

(defconst nix-system--profiles-dir "/nix/var/nix/profiles"
  "Path to NixOS system profiles.")

(defun nix-system--current-generation ()
  "Return the current generation number."
  (let ((link (file-symlink-p (expand-file-name "system" nix-system--profiles-dir))))
    (when (and link (string-match "system-\\([0-9]+\\)-link" link))
      (string-to-number (match-string 1 link)))))

(defun nix-system--generation-entries ()
  "Return tabulated-list entries for NixOS generations."
  (let ((current (nix-system--current-generation))
        entries)
    (dolist (path (directory-files nix-system--profiles-dir t "\\`system-[0-9]+-link\\'"))
      (when (string-match "system-\\([0-9]+\\)-link\\'" path)
        (let* ((num (string-to-number (match-string 1 path)))
               (attrs (file-attributes path))
               (date (format-time-string "%Y-%m-%d %H:%M" (file-attribute-modification-time attrs)))
               (version-file (expand-file-name "nixos-version" path))
               (version (if (file-exists-p version-file)
                            (string-trim (with-temp-buffer
                                           (insert-file-contents version-file)
                                           (buffer-string)))
                          "?"))
               (current-p (if (= num current) "*" "")))
          (push (list num (vector (number-to-string num) date version current-p)) entries))))
    (sort entries (lambda (a b) (> (car a) (car b))))))

(defvar-keymap nix-system-generations-mode-map
  :parent tabulated-list-mode-map
  "d" #'nix-system-generation-delete)

(define-derived-mode nix-system-generations-mode tabulated-list-mode "NixOS Generations"
  "Major mode for listing NixOS generations."
  (setq tabulated-list-format [("Gen" 6 (lambda (a b) (< (car a) (car b))) :right-align t)
                                ("Date" 18 t)
                                ("NixOS Version" 35 t)
                                ("Current" 7 nil)]
        tabulated-list-padding 2
        tabulated-list-sort-key '("Gen" . t))
  (add-hook 'tabulated-list-revert-hook #'nix-system--generations-refresh nil t)
  (tabulated-list-init-header)
  (hl-line-mode 1))

(defun nix-system--generations-refresh ()
  "Refresh the generations list."
  (setq tabulated-list-entries (nix-system--generation-entries)))

(defun nix-system-list-generations ()
  "List NixOS generations in a tabulated buffer."
  (interactive)
  (let ((buf (get-buffer-create "*nix-system: generations*")))
    (with-current-buffer buf
      (nix-system-generations-mode)
      (nix-system--generations-refresh)
      (tabulated-list-print t))
    (pop-to-buffer buf)))

(defun nix-system-generation-delete ()
  "Delete the generation at point."
  (interactive)
  (let ((gen (tabulated-list-get-id)))
    (when (and gen (y-or-n-p (format "Delete generation %d? " gen)))
      (nix-system--run (format "delete gen %d" gen)
                       "sudo" "nix-env" "--delete-generations" (number-to-string gen)
                       "-p" (expand-file-name "system" nix-system--profiles-dir)))))

(defun nix-system-gc ()
  "Run garbage collection on the Nix store."
  (interactive)
  (when (y-or-n-p "Run Nix garbage collection? ")
    (nix-system--run "gc" "sudo" "nix-collect-garbage" "-d")))

;;; Package search

(defvar-local nix-system-search--query nil
  "Current search query for the package search buffer.")

(defvar-keymap nix-system-search-mode-map
  :parent tabulated-list-mode-map
  "RET" #'nix-system-search-show-description
  "w"   #'nix-system-search-copy-name
  "s"   #'nix-system-search-new)

(define-derived-mode nix-system-search-mode tabulated-list-mode "Nix Search"
  "Major mode for displaying nix package search results."
  (setq tabulated-list-format [("Package" 35 t)
                                ("Version" 15 t)
                                ("Description" 0 t)]
        tabulated-list-padding 2)
  (tabulated-list-init-header)
  (hl-line-mode 1))

(defun nix-system-search (query)
  "Search nixpkgs for QUERY and display results in a tabulated list."
  (interactive "sSearch nixpkgs: ")
  (let ((buf (get-buffer-create "*nix-system: search*")))
    (with-current-buffer buf
      (nix-system-search-mode)
      (setq nix-system-search--query query
            tabulated-list-entries nil)
      (tabulated-list-print t)
      (setq header-line-format (format "Searching nixpkgs for '%s'..." query)))
    (pop-to-buffer buf)
    (nix-system--search-async query buf)))

(defun nix-system--search-async (query buffer)
  "Run nix search for QUERY and populate BUFFER with results."
  (let ((proc (start-process "nix-search" " *nix-search-output*"
                             "nix" "search" "nixpkgs" query "--json")))
    (set-process-sentinel
     proc
     (lambda (process _event)
       (when (eq (process-status process) 'exit)
         (if (not (buffer-live-p buffer))
             (message "Search buffer was killed")
           (let ((entries (when (zerop (process-exit-status process))
                           (with-current-buffer (process-buffer process)
                             (nix-system--parse-search-json
                              (buffer-string))))))
             (with-current-buffer buffer
               (setq tabulated-list-entries entries)
               (tabulated-list-print t)
               (setq header-line-format
                     (format "nixpkgs search: '%s' (%d results)"
                             nix-system-search--query
                             (length entries))))
             (kill-buffer (process-buffer process)))))))))

(defun nix-system--parse-search-json (json-string)
  "Parse JSON-STRING from nix search into tabulated-list entries."
  (condition-case nil
      (let ((data (json-parse-string json-string :object-type 'alist))
            entries)
        (dolist (item data)
          (let* ((full-name (symbol-name (car item)))
                 (info (cdr item))
                 (pname (alist-get 'pname info))
                 (version (or (alist-get 'version info) ""))
                 (description (or (alist-get 'description info) "")))
            (push (list full-name (vector pname version description)) entries)))
        (nreverse entries))
    (error nil)))

(defun nix-system-search-show-description ()
  "Show full description of the package at point."
  (interactive)
  (when-let* ((entry (tabulated-list-get-entry)))
    (message "%s %s — %s" (aref entry 0) (aref entry 1) (aref entry 2))))

(defun nix-system-search-copy-name ()
  "Copy the package name at point to the kill ring."
  (interactive)
  (when-let* ((entry (tabulated-list-get-entry)))
    (let ((name (aref entry 0)))
      (kill-new name)
      (message "Copied: %s" name))))

(defun nix-system-search-new (query)
  "Start a new search from within the search buffer."
  (interactive "sSearch nixpkgs: ")
  (nix-system-search query))

;;; Store info

(defun nix-system-store-size ()
  "Show Nix store disk usage."
  (interactive)
  (nix-system--run "store size" "du" "-sh" "/nix/store"))

(defun nix-system-store-optimise ()
  "Optimise the Nix store by hard-linking identical files."
  (interactive)
  (when (y-or-n-p "Optimise Nix store? This can take a while. ")
    (nix-system--run "store optimise" "sudo" "nix" "store" "optimise")))

;;; Edit config

(defun nix-system-edit-config ()
  "Open the NixOS flake directory."
  (interactive)
  (find-file nix-system-flake-dir))

(defun nix-system-edit-flake ()
  "Open flake.nix."
  (interactive)
  (find-file (expand-file-name "flake.nix" nix-system-flake-dir)))

;;; Transient menu

(transient-define-prefix nix-system ()
  "Manage NixOS system."
  [:description
   (lambda () (format "NixOS — %s" (nix-system--hostname)))
   ["Rebuild"
    ("s" "switch" nix-system-rebuild-switch)
    ("t" "test" nix-system-rebuild-test)
    ("b" "boot" nix-system-rebuild-boot)
    ("d" "dry build" nix-system-rebuild-dry)]
   ["Flake"
    ("u" "update all" nix-system-flake-update)
    ("U" "update input" nix-system-flake-update-input)]
   ["Generations & Store"
    ("l" "list generations" nix-system-list-generations)
    ("g" "garbage collect" nix-system-gc)
    ("S" "store size" nix-system-store-size)
    ("O" "store optimise" nix-system-store-optimise)]
   ["Config"
    ("e" "edit flake dir" nix-system-edit-config)
    ("f" "edit flake.nix" nix-system-edit-flake)
    ("/" "search packages" nix-system-search)]])

(provide 'nix-system)
;;; nix-system.el ends here
