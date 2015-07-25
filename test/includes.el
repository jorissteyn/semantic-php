;;; includes.el --- Test include statements

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

(ert-deftest semantic-php-test-includes ()
  "Test include statements in different levels of scope"
  (with-test-buffer
   "
include 'a.php';

function x() {
    require_once 'b.php';
}

class Test {
   public function test() {
       require 'c.php';
   }
}
"
   (let ((tags (semantic-find-tags-included (current-buffer))))
     (with-semantic-tag (nth 0 tags) (should (equal "a.php" tag-name)))
     (with-semantic-tag (nth 1 tags) (should (equal "b.php" tag-name)))
     (with-semantic-tag (nth 2 tags) (should (equal "c.php" tag-name))))))

(provide 'test/includes)
;; includes.el ends here
