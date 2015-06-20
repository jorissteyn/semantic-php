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
(require 'test/php-faux-mode)

(ert-deftest semantic-php-test-parser-variable-tags()
  "Test variable tags"
  (with-test-buffer
   "$test;"
   (with-semantic-first-tag (should (equal "$test" tag-name))
                            (should (equal 'variable tag-class)))))

(ert-deftest semantic-php-test-parser-variable-assignment()
  "Test variable assignment"
  (with-test-buffer
   "$test = 123;"
   (with-semantic-first-tag (should (equal "$test" tag-name))
                            (should (equal 'variable tag-class)))))

(ert-deftest semantic-php-test-parser-variable-multi-statement()
  "Test multiple statements containing variables"
  (with-test-buffer
   "$test; $test;"
   (with-semantic-tags
    (with-semantic-tag (nth 0 tags)
                       (should (equal "$test" tag-name))
                       (should (equal 'variable tag-class)))
    (with-semantic-tag (nth 1 tags)
                       (should (equal "$test" tag-name))
                       (should (equal 'variable tag-class))))))

(ert-deftest semantic-php-test-parser-variable-multi-expression()
  "Test expressions containing multiple varibles"
  (with-test-buffer
   "$left . $middle . $right;"
   (with-semantic-tags
    (with-semantic-tag (nth 0 tags)
                       (should (equal "$left" tag-name))
                       (should (equal 'variable tag-class)))
    (with-semantic-tag (nth 1 tags) (should (equal "$middle" tag-name)))
    (with-semantic-tag (nth 2 tags) (should (equal "$right" tag-name))))))

(ert-deftest semantic-php-test-parser-variable-multi-expression-paren-block()
  "Test expressions containing multiple variables inside paren blocks"
  :expected-result :failed
  (with-test-buffer
   "$left . ($middle . $right);"
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
    (with-semantic-tag (nth 0 tags) (should (equal "$a" tag-name)))
    (with-semantic-tag (nth 1 tags) (should (equal "$b" tag-name)))))
  (with-test-buffer
   "list($a, $b) = $c;"
   (with-semantic-tags
    (with-semantic-tag (nth 0 tags) (should (equal "$a" tag-name)))
    (with-semantic-tag (nth 1 tags) (should (equal "$b" tag-name)))
    (with-semantic-tag (nth 2 tags) (should (equal "$c" tag-name)))))
  (with-test-buffer
   "list($a, $b, $c) = $d;"
   (with-semantic-tags
    (with-semantic-tag (nth 0 tags) (should (equal "$a" tag-name)))
    (with-semantic-tag (nth 1 tags) (should (equal "$b" tag-name)))
    (with-semantic-tag (nth 2 tags) (should (equal "$c" tag-name)))
    (with-semantic-tag (nth 3 tags) (should (equal "$d" tag-name))))))

(ert-deftest semantic-php-test-parser-variable-nested-list-expression()
  "Test nested list() constructs"
  (with-test-buffer
   "list($a, list($b)) = $c;"
   (with-semantic-tags
    (with-semantic-tag (nth 0 tags)
                       (should (equal "$a" tag-name))
                       (should (equal 'variable tag-class)))
    (with-semantic-tag (nth 1 tags) (should (equal "$b" tag-name)))
    (with-semantic-tag (nth 2 tags) (should (equal "$c" tag-name))))))

(ert-deftest semantic-php-test-parser-variable-variables()
  "Test variable variables"
  (with-test-buffer
   "$$test;"
   (with-semantic-first-tag
    (should (equal "$test" tag-name))
    (should (equal 'variable tag-class))))
  (with-test-buffer
   "${$test};"
   (with-semantic-first-tag
    (should (equal "$test" tag-name))
    (should (equal 'variable tag-class)))))

(ert-deftest semantic-php-test-parser-variable-variables-with-multi-expression()
  (with-test-buffer
   "${$a . $b};"
   (with-semantic-tags
    (with-semantic-tag (nth 0 tags) (should (equal "$a" tag-name)))
    (with-semantic-tag (nth 1 tags) (should (equal "$b" tag-name))))))

(ert-deftest semantic-php-test-parser-variable-variables-with-multi-expression-paren-block()
  "Test expressions inside paren blocks"
  :expected-result :failed
  (with-test-buffer
   "${($a . $b) . $c};"
   (with-semantic-tags
    (with-semantic-tag (nth 0 tags) (should (equal "$a" tag-name)))
    (with-semantic-tag (nth 1 tags) (should (equal "$b" tag-name)))
    (with-semantic-tag (nth 2 tags) (should (equal "$c" tag-name))))))

(ert-deftest semantic-php-test-parser-variable-tags-in-heredocs()
  "Test variable tags in heredocs"
  (with-test-buffer
   "$a = <<<here
$b $c
here;"
   (with-semantic-tags
    (with-semantic-tag (nth 0 tags) (should (equal "$a" tag-name)))
    (with-semantic-tag (nth 1 tags) (should (equal "$b" tag-name)))
    (with-semantic-tag (nth 2 tags) (should (equal "$c" tag-name))))))

(ert-deftest semantic-php-test-parser-variable-tags-in-heredocs-braces()
  "Test variable tags inside brace blocks in heredocs"
  (with-test-buffer
   "<<<here
{$a}
here;"
   (with-semantic-tags
    (with-semantic-tag (nth 0 tags) (should (equal "$a" tag-name))))))

(ert-deftest semantic-php-test-parser-variable-tags-in-bracket-blocks()
  "Test variables in bracket blocks"
  (with-test-buffer
   "
$a[0];
$b[$c];"
   (with-semantic-tags
    (should (equal "$a" (semantic-tag-name (nth 0 tags))))
    (should (equal "$b" (semantic-tag-name (nth 1 tags))))
    (should (equal "$c" (semantic-tag-name (nth 2 tags)))))))

(provide 'test/parser/variable)
;;; variable.el ends here
