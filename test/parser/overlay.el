;;; overlay.el --- Test cases for tag overlays -*- lexical-binding: t; -*-

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

(ert-deftest semantic-php-test-parser-overlay-variable()
  "Test overlay for variable tags"
  (with-test-buffer
   "$tag = function(){};"
   (with-semantic-first-tag
    (should (equal 'simple_variable tag-reparse-symbol))
    (should (equal [7 11] tag-overlay)))))

(ert-deftest semantic-php-test-parser-overlay-variable-in-expression()
  "Test overlay for variables in expressions"
  (with-test-buffer
   "$testA  = 1 * $testB = 2;"
   (with-semantic-tags
    (with-semantic-tag (nth 0 tags)
                       (should (equal [7 13] tag-overlay))
                       (should (equal 'simple_variable tag-reparse-symbol)))
    (with-semantic-tag (nth 1 tags)
                       (should (equal [21 27] tag-overlay))
                       (should (equal 'simple_variable tag-reparse-symbol))))))

(ert-deftest semantic-php-test-parser-overlay-namespace()
  "Test overlay for namespace tags"
  (with-test-buffer
   "namespace Test;"
   (with-semantic-first-tag
    (should (equal [7 22] tag-overlay))
    (should (equal 'top_statement tag-reparse-symbol)))))

(provide 'test/parser/overlay)
;;; overlay.el ends here
