;;; use-statements.el --- Test use statements

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

;; TODO: context analysis quickly becomes a mess, need a proper way of
;; testing this.

(require 'semantic-php)
(require 'ert)

(ert-deftest semantic-php-test-use-statements-class-alias ()
  "Test context calculation for an aliassed class"
  (with-saved-test-buffer
   "use A as AliasA;

class A {}

AliasA"
   (let* ((ctxt (semantic-analyze-current-context (point-max)))
          (prefix (oref ctxt prefix)))
     (with-semantic-tag (car prefix)
                        (should (equal "A" tag-name))
                        (should (equal 'type tag-class))
                        (should (equal "class" tag-type))))))

(ert-deftest semantic-php-test-use-statements-class-alias-namespace-braces()
  "Test context calculation for an aliassed class in a brace-block namespace"
  :expected-result :failed
  (with-saved-test-buffer
   "
namespace A {
    class A extends X {}
}

namespace B {
    use A\\A as AliasA;

    /**/ AliasA
}"
   (search-forward "/**/ AliasA")
   (let* ((ctxt (semantic-analyze-current-context))
          (prefix (oref ctxt prefix)))
     (with-semantic-tag (car prefix)
                        (should (equal "A" tag-name))
                        (should (equal "class" tag-type))
                        (should (semantic-tag-type-superclasses tag))))))

(ert-deftest semantic-php-test-use-statements-class-alias-namespace-braceless()
  "Test context calculation for an aliassed class in a braceless namespace"
  :expected-result :failed
  (with-saved-test-buffer
   "
namespace A;
class A {}

namespace B;
class A extends \\A\\A {}

use A\\A as AliasA;

AliasA"
   (let* ((ctxt (semantic-analyze-current-context (point-max)))
          (prefix (oref ctxt prefix)))
     (with-semantic-tag (car prefix)
                        (should (equal "A" tag-name))
                        (should (equal "class" tag-type))
                        (should (not (semantic-tag-type-superclasses tag)))))))

(ert-deftest semantic-php-test-use-statements-namespace-alias-braces()
  "Test context calculation for an aliassed namespace"
  :expected-result :failed
  (with-saved-test-buffer
   "
namespace A {
    class A extends X {}
}

namespace B {
    use A as AliasA;

    /**/ AliasA\\A
}"
   (search-forward "/**/ AliasA\\A")
   (let* ((ctxt (semantic-analyze-current-context))
          (prefix (oref ctxt prefix)))
     (with-semantic-tag (car prefix)
                        (should (equal "A" tag-name))
                        (should (equal "class" tag-type))
                        (should (semantic-tag-type-superclasses tag))))))

(ert-deftest semantic-php-test-use-statements-namespace-alias-braceless()
  "Test context calculation for an aliassed namespace"
  (with-saved-test-buffer
   "
namespace A;
class A {}

namespace B;
class A extends A\\A {}

use A as AliasA;

AliasA\\A"
   (let* ((ctxt (semantic-analyze-current-context (point-max)))
          (prefix (oref ctxt prefix)))
     (with-semantic-tag (car prefix)
                        (should (equal "A" tag-name))
                        (should (equal "class" tag-type))
                        (should (not (semantic-tag-type-superclasses tag)))))))

(provide 'test/use-statements)
;; use-statements.el ends here
