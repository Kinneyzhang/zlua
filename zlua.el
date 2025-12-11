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

(defcustom zlua-script nil
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
  "Add PATH to z.lua database."
  (when (and (zlua--check-configuration)
             (file-directory-p path))
    (let ((lua-exe (zlua--find-lua-executable))
          (random-val (random 2147483647)))
      (setenv "_ZL_RANDOM" (number-to-string random-val))
      (call-process lua-exe nil 0 nil
                    zlua-script "--add" (expand-file-name path)))))

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
                            (if (string-match "^[0-9.]+\\s-+\\(.+\\)$" line)
                                (cons (match-string 1 line) line)
                              (cons line line)))
                          matches))))
    (if paths
        (let* ((choice (completing-read "Select directory: " paths nil t))
               (path (car (rassoc choice paths))))
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
(defun zlua-find-file (pattern)
  "Find and open a file in directory matching PATTERN."
  (interactive "szlua find file in: ")
  (let ((path (zlua--get-matches pattern)))
    (if (and path (file-directory-p path))
        (let ((default-directory path))
          (call-interactively #'find-file))
      (message "zlua: no matching directory found for '%s'" pattern))))

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
