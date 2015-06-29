;;; semantic-php-test.el --- Tests for semantic-php -*- lexical-binding: t; -*-

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

(defmacro with-test-buffer (source &rest body)
  "Set up new buffer for isolated test.

SOURCE must contain the contents of the test buffer, the opening
tag '<?php' must be omitted. BODY is evaluated in the temporary
buffer."
  `(with-test-buffer-plain
    ,(concat "<?php " source)
    ,@body))

(defmacro with-saved-test-buffer (source &rest body)
  "Set up new buffer for isolated test and save it to a temporary file.

SOURCE must contain the contents of the test buffer, the opening
tag '<?php' must be omitted. BODY is evaluated in the temporary
buffer."
  `(let ((filename (concat temporary-file-directory
                           "/semantic-php-test/"
                           ,(md5 source)
                           ".php")))

     ;; Create fake project root in /tmp.
     (or (file-exists-p "/tmp/semantic-php-test")
         (mkdir "/tmp/semantic-php-test"))

     (with-test-buffer-plain
      ,(concat "<?php " source)

      ;; Write source file to disk.
      (write-file filename)

      (unwind-protect
          (progn
            ;; Would expect (semantic-force-refresh) gets everything in
            ;; order, but calling this hook seems the only way to initialize
            ;; a database for the test buffer. If we don't do this,
            ;; semanticdb-current-database will be nil, and much of the
            ;; context analysis won't work.
            (require 'semantic/db-mode)

            (semanticdb-semantic-init-hook-fcn)
            (semantic-force-refresh)

            ,@body)

        ;; Clean up temporary source file.
        (delete-file filename)))))

(defmacro with-test-buffer-plain (source &rest body)
  "Set up new buffer for isolated test

SOURCE must contain the contents of the test buffer, the opening
tag '<?php' must be included. BODY is evaluated in the temporary
buffer."
  `(with-temp-buffer
     (insert ,source)
     (goto-char 0)

     ;; Not the real php-mode, this is a "dummy". The major-mode must
     ;; be php-mode in order to activate the semantic mode-local
     ;; overrides.
     (php-mode)

     ;; Install the parser!
     (semantic-php-default-setup)

     ;; Need to explicitly initialize the lexer, or save the buffer to
     ;; file first. This used to be no problem in EDEP, but something
     ;; is different now. Not quite sure what.
     (semantic-lex-init)

     ,@body))

(defmacro with-lex-tokens (&rest body)
  "Load the wisent token stream for current buffer and execute BODY"
  ;; Skip the first T_OPEN_TAG token.
  `(let ((tokens (cdr (semantic-lex-buffer))))
     ,@body))

(defmacro with-semantic-tags (&rest body)
  "Analyze tags in current buffer and execute BODY"
  `(let ((tags (semantic-parse-region (point-min) (point-max))))
     ,@body))

(defmacro with-semantic-first-tag (&rest body)
  "Analyze tags in current buffer, select the first one and execute BODY"
  `(with-semantic-tags
    (with-semantic-tag (car tags)
                       ,@body)))

(defmacro with-semantic-tag (tag &rest body)
  "Analyze given TAG and execute BODY"
  `(let ((tag ,tag)
         tag-name tag-class tag-type tag-attribs tag-props
         tag-members tag-overlay tag-reparse-symbol)
     (setq tag-name (semantic-tag-name tag)
           tag-class (semantic-tag-class tag)
           tag-type (semantic-tag-type tag)
           tag-attribs (semantic-tag-attributes tag)
           tag-props (semantic-tag-properties tag)
           tag-members (semantic-tag-components tag)
           tag-overlay (semantic-tag-overlay tag)
           tag-reparse-symbol (plist-get tag-props 'reparse-symbol))
     ,@body))

(require 'test/context)
(require 'test/lexer)
(require 'test/parser/cast)
(require 'test/parser/class)
(require 'test/parser/class-member)
(require 'test/parser/condition)
(require 'test/parser/constant)
(require 'test/parser/loop)
(require 'test/parser/function)
(require 'test/parser/interface)
(require 'test/parser/misc)
(require 'test/parser/namespace)
(require 'test/parser/overlay)
(require 'test/parser/trait)
(require 'test/parser/try-catch)
(require 'test/parser/use)
(require 'test/parser/variable)

(provide 'semantic-php-test)
;;; semantic-php-test.el ends here
