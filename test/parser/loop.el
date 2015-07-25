;;; loop.el --- Test cases for looping structures -*- lexical-binding: t; -*-

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

(ert-deftest semantic-php-test-parser-loop-foreach()
  "Test tag generation for foreach parts"
  (with-test-buffer
   "foreach ($a as $b) { $c = 1; }"
   (with-semantic-tags
    (should (equal 2 (length tags)))
    (with-semantic-tag (nth 0 tags) (should (equal "$b" tag-name)))
    (with-semantic-tag (nth 1 tags) (should (equal "$c" tag-name)))))
  (with-test-buffer
   "foreach ($a as $b => $c) {}"
   (with-semantic-tags
    (should (equal 2 (length tags)))
    (with-semantic-tag (nth 0 tags) (should (equal "$b" tag-name)))
    (with-semantic-tag (nth 1 tags) (should (equal "$c" tag-name)))))
  (with-test-buffer
   "foreach ($a as $b): $c = 1; endforeach;"
   (with-semantic-tags
    (should (equal 2 (length tags)))
    (with-semantic-tag (nth 0 tags) (should (equal "$b" tag-name)))
    (with-semantic-tag (nth 1 tags) (should (equal "$c" tag-name)))))
  (with-test-buffer
   "foreach ($a as list($b, $c)) { $d = 1; }"
   (with-semantic-tags
    (should (equal 3 (length tags)))
    (with-semantic-tag (nth 0 tags) (should (equal "$b" tag-name)))
    (with-semantic-tag (nth 1 tags) (should (equal "$c" tag-name)))
    (with-semantic-tag (nth 2 tags) (should (equal "$d" tag-name))))))

(ert-deftest semantic-php-test-parser-loop-while()
  "Test while loops"
  (with-test-buffer
   "while (($a = 1) % $b = 1) {
    $c = 1;
}"
   (with-semantic-tags
    (should (equal 3 (length tags)))
    (with-semantic-tag (nth 0 tags) (should (equal "$a" tag-name)))
    (with-semantic-tag (nth 1 tags) (should (equal "$b" tag-name)))
    (with-semantic-tag (nth 2 tags) (should (equal "$c" tag-name))))))

(ert-deftest semantic-php-test-parser-loop-while-2()
  "Test while loops - double assignment don't work. Not at all related to loops!"
  (with-test-buffer
   "while ($a = 1 % $b = 1) {
    $c = 1;
}"
   (with-semantic-tags
    (should (equal 3 (length tags)))
    (with-semantic-tag (nth 0 tags) (should (equal "$a" tag-name)))
    (with-semantic-tag (nth 1 tags) (should (equal "$b" tag-name)))
    (with-semantic-tag (nth 2 tags) (should (equal "$c" tag-name))))))

(provide 'test/parser/loop)
;;; loop.el ends here
