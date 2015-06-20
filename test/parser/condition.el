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
(require 'test/php-faux-mode)

(ert-deftest semantic-php-test-parser-condition-if-construct()
  "Test parsing of if constructs"
  (with-test-buffer
   "
if ($a === $b) {
    $c;
} else {
    $d;
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
    $c;
else:
    $d;
endif;"
   (with-semantic-tags
    (with-semantic-tag (nth 0 tags) (should (equal "$a" tag-name)))
    (with-semantic-tag (nth 1 tags) (should (equal "$b" tag-name)))
    (with-semantic-tag (nth 2 tags) (should (equal "$c" tag-name)))
    (with-semantic-tag (nth 3 tags) (should (equal "$d" tag-name))))))

(ert-deftest semantic-php-test-parser-condition-switch-construct()
  "Test parsing of switch constructs"
  (with-test-buffer
   "
switch ($a) {
    case $b:
        break $c;
    case 1:
        break;
    default:
        $d;
}"
   (with-semantic-tags
    (with-semantic-tag (nth 0 tags) (should (equal "$a" tag-name)))
    (with-semantic-tag (nth 1 tags) (should (equal "$b" tag-name)))
    (with-semantic-tag (nth 2 tags) (should (equal "$c" tag-name)))
    (with-semantic-tag (nth 3 tags) (should (equal "$d" tag-name))))))

(ert-deftest semantic-php-test-parser-condition-switch-construct-alternative()
  "Test parsing of switch constructs in template notation"
  (with-test-buffer
   "
switch ($a):
    case $b:
        break $c;
    case 1:
        break;
    default:
        $d;
endswitch;"
   (with-semantic-tags
    (with-semantic-tag (nth 0 tags) (should (equal "$a" tag-name)))
    (with-semantic-tag (nth 1 tags) (should (equal "$b" tag-name)))
    (with-semantic-tag (nth 2 tags) (should (equal "$c" tag-name)))
    (with-semantic-tag (nth 3 tags) (should (equal "$d" tag-name))))))

(provide 'test/parser/condition)
;;; condition.el ends here
