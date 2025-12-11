;;; zlua.el --- Emacs integration for z.lua  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Kinney Zhang

;; Author: Kinney Zhang
;; URL: https://github.com/Kinneyzhang/zlua
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience, matching

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the MIT License.

;;; Commentary:

;; zlua.el provides Emacs integration for z.lua, a fast directory jumping
;; tool that learns your habits. This package allows you to:
;;
;; - Jump to frequently/recently visited directories
;; - Track directory visits automatically in dired-mode
;; - Use interactive selection with completion
;; - List matching directories
;;
;; See README.md for installation and usage instructions.

;;; Code:

(require 'dired)
(require 'cl-lib)

(defgroup zlua nil
  "Emacs integration for z.lua directory jumping."
  :group 'convenience
  :prefix "zlua-")

(defcustom zlua-executable nil
  "Path to lua executable.
If nil, will search for lua, luajit, lua5.3, lua5.2, or lua5.1 in PATH."
  :type '(choice (const :tag "Auto-detect" nil)
                 (file :tag "Path to lua"))
  :group 'zlua)

(defconst zlua-directory
  (file-name-directory (or load-file-name buffer-file-name)))

(defcustom zlua-script (expand-file-name "z.lua" zlua-directory)
  "Path to z.lua script.
This must be set to the absolute path of z.lua script."
  :type '(choice (const :tag "Not set" nil)
                 (file :tag "Path to z.lua"))
  :group 'zlua)

(defcustom zlua-enable-auto-track t
  "Whether to automatically track directory visits in dired-mode.
When non-nil, visiting directories in dired will update z.lua database."
  :type 'boolean
  :group 'zlua)

(defvar zlua--lua-executable nil
  "Cached path to lua executable.")

(defvar zlua--dirs-cache nil
  "Cached list of tracked directories.")

(defvar zlua--dirs-cache-time nil
  "Time when the directories cache was last updated.")

(defcustom zlua-cache-timeout 60
  "Number of seconds before the directories cache expires.
Set to 0 to disable caching."
  :type 'integer
  :group 'zlua)

(defun zlua--find-lua-executable ()
  "Find lua executable in PATH."
  (or zlua--lua-executable
      zlua-executable
      (setq zlua--lua-executable
            (cl-some (lambda (name)
                       (executable-find name))
                     '("lua" "luajit" "lua5.3" "lua5.2" "lua5.1")))))

(defun zlua--check-configuration ()
  "Check if z.lua is properly configured.
Returns t if configuration is valid, otherwise signals an error."
  (let ((lua-exe (zlua--find-lua-executable))
        (z-script zlua-script))
    (unless lua-exe
      (error "zlua: lua executable not found. Please install lua or set `zlua-executable'"))
    (unless z-script
      (error "zlua: z.lua script not found. Please set `zlua-script' to the path of z.lua"))
    (unless (file-exists-p z-script)
      (error "zlua: z.lua script not found at %s" z-script))
    t))

(defun zlua--call-zlua (args)
  "Call z.lua with ARGS and return output as string.
Returns nil if there's an error."
  (when (zlua--check-configuration)
    (let ((lua-exe (zlua--find-lua-executable)))
      (with-temp-buffer
        (let ((exit-code (apply #'call-process lua-exe nil t nil
                                zlua-script args)))
          (if (zerop exit-code)
              (string-trim (buffer-string))
            nil))))))

(defun zlua--add-path (path)
  "Add PATH to z.lua database.
Normalizes PATH by removing trailing slashes to avoid duplicate entries."
  (when (and (zlua--check-configuration)
             (file-directory-p path))
    (let ((lua-exe (zlua--find-lua-executable))
          (random-val (random 2147483647))
          ;; Normalize path by removing trailing slashes
          (normalized-path (directory-file-name (expand-file-name path))))
      (setenv "_ZL_RANDOM" (number-to-string random-val))
      (call-process lua-exe nil 0 nil
                    zlua-script "--add" normalized-path))))

(defun zlua--get-matches (pattern &optional list-all)
  "Get directories matching PATTERN.
If LIST-ALL is non-nil, return all matches with scores.
Otherwise return the best match."
  (let* ((args (if list-all
                   (list "-l" pattern)
                 (list "--cd" pattern)))
         (output (zlua--call-zlua args)))
    (when (and output (not (string-empty-p output)))
      (if list-all
          (split-string output "\n" t)
        output))))

(defun zlua--cache-expired-p ()
  "Return t if the directories cache has expired."
  (or (null zlua--dirs-cache-time)
      (null zlua--dirs-cache)
      (zerop zlua-cache-timeout)
      (> (float-time (time-subtract (current-time) zlua--dirs-cache-time))
         zlua-cache-timeout)))

(defun zlua--get-all-dirs ()
  "Get all tracked directories, using cache when available.
Returns a list of directory paths extracted from the z.lua output."
  (when (zlua--cache-expired-p)
    (let* ((all-dirs (zlua--get-matches "." t))
           (paths nil))
      (dolist (line all-dirs)
        (when (string-match "^[0-9.,]+\\s-+\\(.+\\)$" line)
          (let ((dir (match-string 1 line)))
            (when (file-directory-p dir)
              (push dir paths)))))
      (setq zlua--dirs-cache (nreverse paths))
      (setq zlua--dirs-cache-time (current-time))))
  zlua--dirs-cache)

(defun zlua-clear-cache ()
  "Clear the directories cache.
Call this if you want to force a refresh of the tracked directories list."
  (interactive)
  (setq zlua--dirs-cache nil)
  (setq zlua--dirs-cache-time nil)
  (message "zlua: cache cleared"))

;;;###autoload
(defun zlua-jump (pattern &optional interactive-select)
  "Jump to directory matching PATTERN.
With prefix argument INTERACTIVE-SELECT, allow interactive selection
when multiple matches are found."
  (interactive
   (list (read-string "zlua jump to: ")
         current-prefix-arg))
  (if interactive-select
      (zlua-jump-interactive pattern)
    (let ((path (zlua--get-matches pattern)))
      (if (and path (file-directory-p path))
          (progn
            (dired path)
            (message "zlua: jumped to %s" path))
        (message "zlua: no matching directory found for '%s'" pattern)))))

;;;###autoload
(defun zlua-jump-interactive (pattern)
  "Jump to directory matching PATTERN with interactive selection.
Shows a list of all matching directories and allows selection."
  (interactive "szlua jump (interactive): ")
  (let* ((matches (zlua--get-matches pattern t))
         (paths (when matches
                  (mapcar (lambda (line)
                            ;; Parse output: "score path"
                            (if (string-match "^[0-9.,]+\\s-+\\(.+\\)$" line)
                                (cons line (match-string 1 line))
                              (cons line line)))
                          matches))))
    (if paths
        (let* ((choice (completing-read "Select directory: " paths nil t))
               (path (cdr (assoc choice paths))))
          (when (and path (file-directory-p path))
            (dired path)
            (message "zlua: jumped to %s" path)))
      (message "zlua: no matching directory found for '%s'" pattern))))

;;;###autoload
(defun zlua-list (pattern)
  "List all directories matching PATTERN with their scores."
  (interactive "szlua list matches for: ")
  (let ((matches (zlua--get-matches pattern t)))
    (if matches
        (with-current-buffer (get-buffer-create "*zlua matches*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "Directories matching '%s':\n\n" pattern))
            (dolist (match matches)
              (insert match "\n"))
            (goto-char (point-min))
            (special-mode))
          (display-buffer (current-buffer)))
      (message "zlua: no matching directory found for '%s'" pattern))))

;;;###autoload
(defun zlua-search-dir (pattern)
  "Find and open a file in directory matching PATTERN."
  (interactive "szlua search dir: ")
  (let ((path (zlua--get-matches pattern)))
    (if (and path (file-directory-p path))
        (let ((default-directory path))
          (call-interactively #'find-file))
      (message "zlua: no matching directory found for '%s'" pattern))))

;;;###autoload
(defun zlua-search-file (filename-pattern)
  "Find and open a file by FILENAME-PATTERN in z.lua tracked directories.
Searches for files matching FILENAME-PATTERN across all directories
in the z.lua database and allows selection if multiple matches are found.
FILENAME-PATTERN is matched as a substring in the filename.
Uses cached directory list for better performance."
  (interactive "szlua search file: ")
  (let* ((all-dirs (zlua--get-all-dirs))  ; Use cached directories
         (matching-files nil))
    (if (not all-dirs)
        (message "zlua: no tracked directories found")
      ;; Search for matching files in each directory
      (dolist (dir all-dirs)
        (condition-case nil
            (let ((files (directory-files dir t)))
              (dolist (file files)
                (when (and (file-regular-p file)
                           (string-match-p (regexp-quote filename-pattern)
                                           (file-name-nondirectory file)))
                  (push file matching-files))))
          (error nil)))  ; Ignore errors from inaccessible directories
      
      (cond
       ((null matching-files)
        (message "zlua: no files matching '%s' found in tracked directories"
                 filename-pattern))
       ((= 1 (length matching-files))
        (find-file (car matching-files))
        (message "zlua: opened %s" (car matching-files)))
       (t
        ;; Multiple matches, let user choose
        ;; Create display strings with directory context
        (let* ((file-alist
                (mapcar
                 (lambda (f)
                   (cons (format "%s (%s)"
                                 (file-name-nondirectory f)
                                 (abbreviate-file-name (file-name-directory f)))
                         f))
                 matching-files))
               (choice (completing-read "Select file: " file-alist nil t))
               (selected-file (cdr (assoc choice file-alist))))
          (when selected-file
            (find-file selected-file)
            (message "zlua: opened %s" selected-file))))))))

(defun zlua--dired-add-path ()
  "Add current dired directory to z.lua database."
  (when (and zlua-enable-auto-track
             (derived-mode-p 'dired-mode)
             dired-directory)
    (let ((dir (if (stringp dired-directory)
                   dired-directory
                 (car dired-directory))))
      (zlua--add-path dir))))

;;;###autoload
(define-minor-mode zlua-mode
  "Toggle zlua-mode.
When enabled, automatically track directory visits in dired-mode."
  :global t
  :group 'zlua
  :lighter " zlua"
  (if zlua-mode
      (add-hook 'dired-after-readin-hook #'zlua--dired-add-path)
    (remove-hook 'dired-after-readin-hook #'zlua--dired-add-path)))

;; Convenience aliases
;;;###autoload
(defalias 'zlua 'zlua-jump
  "Alias for `zlua-jump'.")

;;;###autoload
(defalias 'z 'zlua-jump
  "Convenient short alias for `zlua-jump'.")

(provide 'zlua)

;;; zlua.el ends here
