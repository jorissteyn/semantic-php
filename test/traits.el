;;; traits.el --- Test traits

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

(ert-deftest semantic-php-test-traits-scope ()
  "Test scope calculation for trait usage."
  :expected-result :failed
  (with-saved-test-buffer
   "
trait A { function a() {} }
trait B { function b() {} }
trait C { function c() {} }
class D {
    use A;
    use B;
    use C;
    public function d() {
        /**/
    }
}
"
   (search-forward "/**/")

   (let* ((scope (semantic-calculate-scope))
          (tag (oref scope tag))
          (scopetags (oref scope scope))
          (parents (oref scope parents)))
     (should (equal "d" (semantic-tag-name tag)))
     (should (equal "D" (semantic-tag-name (car parents))))
     (should (equal "d" (semantic-tag-name (nth 0 scopetags))))
     (should (equal "C" (semantic-tag-name (nth 1 scopetags))))
     (should (equal "B" (semantic-tag-name (nth 2 scopetags))))
     (should (equal "A" (semantic-tag-name (nth 3 scopetags))))
     (should (equal "a" (semantic-tag-name (nth 4 scopetags))))
     (should (equal "b" (semantic-tag-name (nth 5 scopetags))))
     (should (equal "c" (semantic-tag-name (nth 6 scopetags)))))))

(ert-deftest semantic-php-test-traits-completions ()
  "Test completions for members imported by traits"
  :expected-result :failed
  (with-saved-test-buffer
   "
trait A {
    public function a() {}
}

class B {
    use A;

    public function b() {
        $this->
    }
}
"
   (search-forward "$this->")

   (let ((completions (semantic-analyze-possible-completions
                       (semantic-analyze-current-context))))
     (with-semantic-tag (nth 0 completions)
                        (should (equal "a" tag-name))
                        (should (equal 'function tag-class)))
     (with-semantic-tag (nth 1 completions)
                        (should (equal "b" tag-name))
                        (should (equal 'function tag-class))))))

(provide 'test/traits)
;; traits.el ends here
