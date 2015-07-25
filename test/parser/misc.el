;;; misc.el --- Misc test cases for semantic-php -*- lexical-binding: t; -*-

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

(ert-deftest semantic-php-test-parser-misc-empty()
  "Test empty() construct"
  (with-test-buffer
   "empty($test);"
   (with-semantic-tags
    (should (equal 0 (length tags))))))

(ert-deftest semantic-php-test-parser-misc-isset()
  "Test isset() construct"
  (with-test-buffer
   "isset($a, $b);"
   (with-semantic-tags
    (should (equal 0 (length tags))))))

(ert-deftest semantic-php-test-parser-misc-eval()
  "Test eval statements"
  (with-test-buffer
   "eval($test = get());"
   (with-semantic-tags
    (should (equal 0 (length tags))))))

(ert-deftest semantic-php-test-parser-misc-include-require()
  "Test include/require statements"
  (with-test-buffer
   "require $test;")
  (with-test-buffer
   "require ($test);")
  (with-test-buffer
   "
include 'test.php';
include('test.php');
require 'test.php';
require('test.php');
include_once 'test.php';
include_once('test.php');
require_once 'test.php';
require_once('test.php');
"
   (with-semantic-tags
    (should (equal 8 (length tags)))
    (dolist (tag tags)
      (should (equal "test.php" (semantic-tag-name tag)))))))

(ert-deftest semantic-php-test-parser-misc-return-statements()
  "Test return statements"
  (with-test-buffer
   "function test() { return function() {}; }"
   (with-semantic-first-tag
    (should (equal 'function tag-class))
    (should (equal 1 (length (plist-get tag-attribs :members)))))))

(provide 'test/parser/misc)
;;; misc.el ends here
