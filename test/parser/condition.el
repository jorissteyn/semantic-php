;;; condition.el --- Test cases for condition structures -*- lexical-binding: t; -*-

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

(ert-deftest semantic-php-test-parser-condition-if-construct()
  "Test parsing of if constructs"
  (with-test-buffer
   "
if (($a = 1) === ($b = get())) {
    $c = true;
} else {
    $d = true;
}"
   (with-semantic-tags
    (with-semantic-tag (nth 0 tags) (should (equal "$a" tag-name)))
    (with-semantic-tag (nth 1 tags) (should (equal "$b" tag-name)))
    (with-semantic-tag (nth 2 tags) (should (equal "$c" tag-name)))
    (with-semantic-tag (nth 3 tags) (should (equal "$d" tag-name))))))

(ert-deftest semantic-php-test-parser-condition-if-construct-alternative()
  "Test parsing of if constructs in template notation"
  (with-test-buffer
   "
if ($a === $b):
    class A {}
else:
    class B {}
endif;"
   (with-semantic-tags
    (with-semantic-tag (nth 0 tags) (should (equal "A" tag-name)))
    (with-semantic-tag (nth 1 tags) (should (equal "B" tag-name))))))

(ert-deftest semantic-php-test-parser-condition-switch-construct()
  "Test parsing of switch constructs"
  (with-test-buffer
   "
switch ($a = 1) {
    case $b:
        break $c = 1;
    case 1:
        break;
    default:
        $d = 1;
}"
   (with-semantic-tags
    (with-semantic-tag (nth 0 tags) (should (equal "$a" tag-name)))
    (with-semantic-tag (nth 1 tags) (should (equal "$c" tag-name)))
    (with-semantic-tag (nth 2 tags) (should (equal "$d" tag-name))))))

(ert-deftest semantic-php-test-parser-condition-switch-construct-alternative()
  "Test parsing of switch constructs in template notation"
  (with-test-buffer
   "
switch ($a = 1):
    case $b:
        break $c = 1;
    case 1:
        break;
    default:
        $d = 1;
endswitch;"
   (with-semantic-tags
    (with-semantic-tag (nth 0 tags) (should (equal "$a" tag-name)))
    (with-semantic-tag (nth 1 tags) (should (equal "$c" tag-name)))
    (with-semantic-tag (nth 2 tags) (should (equal "$d" tag-name))))))

(ert-deftest semantic-php-test-parser-condition-if-statement-1()
  "Test parsing of if-statements (variation)"
  (with-test-buffer
   "
if ($a = get() !== null) {
    $b = get();
}"
   (with-semantic-tags
    (with-semantic-tag (nth 0 tags) (should (equal "$a" tag-name)))
    (with-semantic-tag (nth 1 tags) (should (equal "$b" tag-name))))))

(provide 'test/parser/condition)
;;; condition.el ends here
