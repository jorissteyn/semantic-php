;;; class-members.el --- Test class member analysis

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

(ert-deftest semantic-php-test-class-members-type ()
  "Test type deduction on class member"
  :expected-result :failed
  (with-saved-test-buffer
   "
class A {
     /**
      * @var B
      */
     public $test;
}

class B {}

A::$test
"
   (search-forward "A::$test")

   (let* ((ctxt (semantic-analyze-current-context))
          (prefixtypes (oref ctxt prefixtypes)))
     (with-semantic-tag (car prefixtypes)
                        (should (equal "B" tag-name))
                        (should (equal 'type tag-class))
                        (should (equal "class" tag-type))))))


(provide 'test/class-members)
;; class-members.el ends here
