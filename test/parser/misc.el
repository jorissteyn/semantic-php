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
   (with-semantic-first-tag (should (equal "$test" tag-name))
                            (should (equal 'variable tag-class)))))

(ert-deftest semantic-php-test-parser-misc-isset()
  "Test isset() construct"
  (with-test-buffer
   "isset($testA, $testB);"
   (with-semantic-tags
    (with-semantic-tag (nth 0 tags) (should (equal "$testA" tag-name)))
    (with-semantic-tag (nth 1 tags) (should (equal "$testB" tag-name))))))

;; TODO
(ert-deftest semantic-php-test-parser-misc-eval()
  "Test eval statements"
  (with-test-buffer
   "eval('test');")
  (with-test-buffer
   "eval($test);"))

;; TODO
(ert-deftest semantic-php-test-parser-misc-include-require()
  "Test include/require statements"
  (with-test-buffer
   "require $test;")
  (with-test-buffer
   "require ($test);")
  (with-test-buffer
   "include 'test';")
  (with-test-buffer
   "include ('test');"))

(ert-deftest semantic-php-test-parser-misc-return-statements()
  "Test return statements"
  (with-test-buffer
   "return $testA . $testB;"
   (with-semantic-tags
    (with-semantic-tag (nth 0 tags) (should (equal "$testA" tag-name)))
    (with-semantic-tag (nth 1 tags) (should (equal "$testB" tag-name)))))
  (with-test-buffer
   "
function test() {
    return $testA . $testB;
}"
   (with-semantic-first-tag
    (let ((members (plist-get tag-attribs :members)))
      (should (equal 2 (length members)))
      (with-semantic-tag (nth 0 members) (should (equal "$testA" tag-name)))
      (with-semantic-tag (nth 1 members) (should (equal "$testB" tag-name)))))))

(provide 'test/parser/misc)
;;; misc.el ends here
