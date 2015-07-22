;;; local-variables.el --- Test local variable analysis

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

;; TODO: all these tests are fine, but calling (semantic-fetch-tags)
;; before any of the semantic-* functions should not be required.

(ert-deftest semantic-php-test-local-variables-top-level ()
  "Test local variables in top-level scope"
  (with-test-buffer
   "
$a = 1;
global $b;
$c;

HERE;"
   (search-forward "HERE")
   (let ((tags (semantic-get-local-variables)))
     (should (equal 2 (length tags)))
     (with-semantic-tag (nth 0 tags) (should (equal "$a" tag-name)))
     (with-semantic-tag (nth 1 tags) (should (equal "$b" tag-name))))))

(ert-deftest semantic-php-test-local-variables-function-scope ()
  "Test local variables in function scope"
  (with-test-buffer
   "
global $x;

function test($a, $b) {
  static $c;
}"
   (search-forward "global")
   (let ((tags (semantic-get-local-variables)))
     (should (equal 1 (length tags)))
     (should (equal "$x" (semantic-tag-name (car tags)))))

   (search-forward "static")
   (let ((tags (semantic-get-local-variables)))
     (should (equal 3 (length tags)))
     (should (equal "$a" (semantic-tag-name (nth 0 tags))))
     (should (equal "$b" (semantic-tag-name (nth 1 tags))))
     (should (equal "$c" (semantic-tag-name (nth 2 tags)))))))

(ert-deftest semantic-php-test-local-variables-method-scope ()
  "Test local variables in methods scope"
  (with-test-buffer
   "
class A {
    public function test($a, $b) {
        static $c;
    }
}"
   (search-forward "static")
   (semantic-fetch-tags)
   (let ((tags (semantic-get-local-variables (point))))
     (should (equal 5 (length tags)))
     (should (equal "$a" (semantic-tag-name (nth 0 tags))))
     (should (equal "$b" (semantic-tag-name (nth 1 tags))))
     (should (equal "$c" (semantic-tag-name (nth 2 tags))))
     (should (equal "$this" (semantic-tag-name (nth 3 tags))))
     (should (equal "self" (semantic-tag-name (nth 4 tags)))))))

(ert-deftest semantic-php-test-local-variables-method-scope-with-parent ()
  "Test local variables in methods scope with parent"
  (with-test-buffer
   "
class A extends B {
    public function test() {
        static $a;
    }
}"
   (search-forward "static")
   (semantic-fetch-tags)
   (let ((tags (semantic-get-local-variables (point))))
     (should (equal 4 (length tags)))
     (should (equal "$a" (semantic-tag-name (nth 0 tags))))
     (should (equal "$this" (semantic-tag-name (nth 1 tags))))
     (should (equal "A" (semantic-tag-name (semantic-tag-type (nth 1 tags)))))
     (should (equal "parent" (semantic-tag-name (nth 2 tags))))
     (should (equal "B" (semantic-tag-type (nth 2 tags))))
     (should (equal "self" (semantic-tag-name (nth 3 tags)))))))

(provide 'test/local-variables)
;; local-variables.el ends here
