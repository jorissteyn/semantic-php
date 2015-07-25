;;; variable.el --- Test cases for variable tags -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Joris Steyn

;; Author: Joris Steyn <jorissteyn@gmail.com>
;; Created: 1 Nov 2014
;; Keywords: languages
;; Homepage: https://github.com/jorissteyn/semantic-php

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

(ert-deftest semantic-php-test-parser-variable-tags()
  "Test variable tags"
  (with-test-buffer
   "$test = 1;"
   (with-semantic-first-tag (should (equal "$test" tag-name))
                            (should (equal 'variable tag-class))
                            (should (equal [7 12] tag-overlay))
                            (should (equal 'simple_variable tag-reparse-symbol)))))

(ert-deftest semantic-php-test-parser-variable-assignment()
  "Test variable assignment"
  (with-test-buffer
   "$test = 123;"
   (with-semantic-first-tag (should (equal "$test" tag-name))
                            (should (equal 'variable tag-class)))))

(ert-deftest semantic-php-test-parser-variable-multi-statement()
  "Test multiple statements containing variables"
  (with-test-buffer
   "$a = 1; $b = 1;"
   (with-semantic-tags
    (with-semantic-tag (nth 0 tags)
                       (should (equal "$a" tag-name))
                       (should (equal 'variable tag-class)))
    (with-semantic-tag (nth 1 tags)
                       (should (equal "$b" tag-name))
                       (should (equal 'variable tag-class))))))

(ert-deftest semantic-php-test-parser-variable-multi-expression()
  "Test expressions containing multiple varibles"
  (with-test-buffer
   "($left = 1) . ($middle = 1) . ($right = 1);"
   (with-semantic-tags
    (with-semantic-tag (nth 0 tags)
                       (should (equal "$left" tag-name))
                       (should (equal 'variable tag-class)))
    (with-semantic-tag (nth 1 tags) (should (equal "$middle" tag-name)))
    (with-semantic-tag (nth 2 tags) (should (equal "$right" tag-name))))))

(ert-deftest semantic-php-test-parser-variable-multi-expression-paren-block()
  "Test expressions containing multiple variables inside paren blocks"
  (with-test-buffer
   "$left = 1 . ($middle = 1 . $right = 1);"
   (with-semantic-tags
    (with-semantic-tag (nth 0 tags)
                       (should (equal "$left" tag-name))
                       (should (equal 'variable tag-class)))
    (with-semantic-tag (nth 1 tags) (should (equal "$middle" tag-name)))
    (with-semantic-tag (nth 2 tags) (should (equal "$right" tag-name))))))

(ert-deftest semantic-php-test-parser-variable-list-expression()
  "Test list() = $var constructs"
  (with-test-buffer
   "list($a) = $b;"
   (with-semantic-tags
    (should (equal 1 (length tags)))
    (with-semantic-first-tag (should (equal "$a" tag-name)))))
  (with-test-buffer
   "list($a, $b) = $c;"
   (with-semantic-tags
    (should (equal 2 (length tags)))
    (with-semantic-tag (nth 0 tags) (should (equal "$a" tag-name)))
    (with-semantic-tag (nth 1 tags) (should (equal "$b" tag-name)))))
  (with-test-buffer
   "list($a, $b, $c) = $d;"
   (with-semantic-tags
    (should (equal 3 (length tags)))
    (with-semantic-tag (nth 0 tags) (should (equal "$a" tag-name)))
    (with-semantic-tag (nth 1 tags) (should (equal "$b" tag-name)))
    (with-semantic-tag (nth 2 tags) (should (equal "$c" tag-name))))))

(ert-deftest semantic-php-test-parser-variable-nested-list-expression()
  "Test nested list() constructs"
  (with-test-buffer
   "list($a, list($b)) = $c = $this->get();"
   (with-semantic-tags
    (should (equal 3 (length tags)))
    (with-semantic-tag (nth 0 tags)
                       (should (equal "$a" tag-name))
                       (should (equal 'variable tag-class)))
    (with-semantic-tag (nth 1 tags) (should (equal "$b" tag-name)))
    (with-semantic-tag (nth 2 tags) (should (equal "$c" tag-name))))))

(ert-deftest semantic-php-test-parser-variable-variables()
  "Test declarations in variable variables"
  :expected-result :failed
  (with-test-buffer
   "${$test = 1};"
   (with-semantic-first-tag
    (should (equal "$test" tag-name))
    (should (equal 'variable tag-class)))))

(ert-deftest semantic-php-test-parser-variable-variables-with-multi-expression()
  "Test declarations in variable variables"
  :expected-result :failed
  (with-test-buffer
   "${($a = 2) . $b = 1};"
   (with-semantic-tags
    (with-semantic-tag (nth 0 tags) (should (equal "$a" tag-name)))
    (with-semantic-tag (nth 1 tags) (should (equal "$b" tag-name))))))

(provide 'test/parser/variable)
;;; variable.el ends here
