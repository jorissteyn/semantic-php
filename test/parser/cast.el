;;; cast.el --- Test cases for casts -*- lexical-binding: t; -*-

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

(require 'semantic-php)
(require 'ert)
(require 'test/php-faux-mode)

(ert-deftest semantic-php-test-parser-cast-variables ()
  "Test parsing of casted variables"
  (with-test-buffer
   "(int)
    (double)
    (string)
    (array)
    (object)
    (bool)
    (unset)"
   (with-lex-tokens
    (dolist (expected '(T_INT_CAST
                        T_DOUBLE_CAST
                        T_STRING_CAST
                        T_ARRAY_CAST
                        T_OBJECT_CAST
                        T_BOOL_CAST
                        T_UNSET_CAST))
      (should (equal expected (semantic-lex-token-class (pop tokens))))))))

(provide 'test/parser/cast)
;;; cast.el ends here
