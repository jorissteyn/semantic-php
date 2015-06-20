;;; semantic-php-util.el --- Batch utils for semantic-php -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Joris Steyn

;; Author: Joris Steyn <jorissteyn@gmail.com>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

(defun semantic-php-util-batch-scan-project ()
  "Scan project recursively for source files in batch mode.

Usage: make parse PARSE_PATH=/path/to/many/source/files

This function serves no purpose other than to see if the wisent
parser can parse source files of common frameworks and libraries."
  (let* ((root (or (getenv "PARSE_PATH")
                   default-directory))
         (files (semantic-php-util-files-in-directory root))
         (total (length files))
         (progress-reporter (make-progress-reporter
                             (format "Scanning %d files in '%s'... " total root)
                             0 total 0 5 5))
         (progress 0)
         (tagcount 0))

    (require 'semantic-php)
    (require 'test/php-faux-mode)

    (dolist (file files)
      ;; Open the file in a buffer
      (find-file file)

      ;; Load fake php-mode
      (php-mode)

      ;; Parse the file
      (setq tagcount (+ tagcount (length
                                  (wisent-parse-region (point-min) (point-max)))))

      ;; Clean up and report progress
      (kill-buffer)

      (progress-reporter-update progress-reporter
                                (setq progress (1+ progress))))

    (progress-reporter-done progress-reporter)
    (message "Found total of %d tags" tagcount)))

(defun semantic-php-util-files-in-directory (directory)
  "Scan DIRECTORY recursively for source files.

Returns a list of absolute file names."
  ;; Ensure trailing slash in directory name
  (setq directory (file-name-as-directory directory))

  (let (files fullpath)
    (dolist (file (directory-files directory) files)
      (setq fullpath (concat directory file))
      (cond ((eq 0 (string-match-p "[.]" file))
             ;; skip ., .. and hidden files
             nil)
            ((file-directory-p fullpath)
             ;; Recurse into directories
             (setq files (nconc files (semantic-php-util-files-in-directory fullpath))))
            ((string-match-p "[.]php[s3457]?$" file)
             ;; Collect found source file
             (setq files (nconc files (list fullpath))))))))

(defun semantic-php-util-compile-grammar ()
  "Generate the wisent parser semantic-php-wy.el."
  (let* ((grammar-path (concat (or (getenv "PACKAGE_PATH")
                                   default-directory)
                               "/semantic-php.wy")))
    (with-temp-buffer
      (find-file grammar-path)

      (require 'semantic/grammar)
      (semantic-mode)
      (semantic-grammar-create-package t))))

(provide 'semantic-php-util)
;;; semantic-php-util.el ends here
