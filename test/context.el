;;; context.el --- Test context functions

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

(ert-deftest semantic-php-test-context-simple-local-variable()
  "Test type deduction on local variables"
  ;; TODO: This fails because the parser does not set the variable
  ;; type in expr_without_variable.
  :expected-result :failed
  (with-saved-test-buffer
   "
class A {}

$var = new A;
"
   (search-forward "new A")
   (move-end-of-line 1)
   (insert "$var->")

   (let* ((ctxt (semantic-analyze-current-context))
          (prefixtypes (oref ctxt prefixtypes)))
     (with-semantic-tag (car prefixtypes)
                        (should (equal "A" tag-name))
                        (should (equal 'type tag-class))
                        (should (equal "class" tag-type))))))

(ert-deftest semantic-php-test-context-method-argument()
  "Test type deduction on method arguments"
  (with-saved-test-buffer
   "
class A {}

function test(A $arg) {
  $var = new A;
}
"
   (search-forward "new A")
   (move-end-of-line 1)
   (insert "$arg->")

   (let* ((ctxt (semantic-analyze-current-context))
          (prefixtypes (oref ctxt prefixtypes)))
     (with-semantic-tag (car prefixtypes)
                        (should (equal "A" tag-name))
                        (should (equal 'type tag-class))
                        (should (equal "class" tag-type))))))

(ert-deftest semantic-php-test-context-function-return ()
  "Test type deduction on function return value"
  :expected-result :failed
  (with-test-buffer
   "
class A {}

/**
 * @return A
 */
function test() {
  return new A;
}

/**/ test()->
"
   (search-forward "/**/ test()")

   (let* ((ctxt (semantic-analyze-current-context))
          (prefixtypes (oref ctxt prefixtypes)))
     (with-semantic-tag (car prefixtypes)
                        (should (equal "A" tag-name))
                        (should (equal 'type tag-class))
                        (should (equal "class" tag-type))))))

(ert-deftest semantic-php-test-context-typed-function-return ()
  "Test type deduction on typed function return value"
  (with-saved-test-buffer
   "
class A {}

function test() : A {
  return new A;
}

/**/ test()->
"
   (search-forward "/**/ test()->")

   (let* ((ctxt (semantic-analyze-current-context))
          (prefixtypes (oref ctxt prefixtypes)))
     (with-semantic-tag (car prefixtypes)
                        (should (equal "A" tag-name))
                        (should (equal 'type tag-class))
                        (should (equal "class" tag-type))))))

(ert-deftest semantic-php-test-context-class-simple ()
  "Test context calculation for an regular class"
  ;; Test non-aliassed behaviour.
  (with-saved-test-buffer
   "class A {}

A"
   (let* ((ctxt (semantic-analyze-current-context (point-max)))
          (prefix (oref ctxt prefix)))
     (with-semantic-tag (car prefix)
                        (should (equal "A" tag-name))
                        (should (equal 'type tag-class))
                        (should (equal "class" tag-type))))))

(ert-deftest semantic-php-test-context-class-namespace-braces ()
  "Test context calculation for a brace-block namespaced class"
  (with-saved-test-buffer
   "
namespace A {
    class A {}
}

\\A\\A"
   (let* ((ctxt (semantic-analyze-current-context (point-max)))
          (prefix (oref ctxt prefix)))
     (with-semantic-tag (car prefix)
                        (should (equal "A" tag-name))
                        (should (equal "class" tag-type))))))

(ert-deftest semantic-php-test-context-class-multiple-namespaces-braces ()
  "Test context calculation for a namespaced class in different namespaces"
  (with-saved-test-buffer
   "
namespace A {
    class A {}
}

namespace B {
    class A {}
    /**/ \\A\\A
}"
   (search-forward "/**/ \\A\\A")
   (let* ((ctxt (semantic-analyze-current-context))
          (prefix (oref ctxt prefix)))
     (with-semantic-tag (car prefix)
                        (should (equal "A" tag-name))
                        (should (equal "class" tag-type))))))

(ert-deftest semantic-php-test-context-class-namespace-braceless()
  "Test context calculation for a class in a braceless namespace"
  :expected-result :failed
  (with-saved-test-buffer
   "
namespace A;
class A {}

namespace B;
class A extends \\A\\A {}

\\A\\A"
   (let* ((ctxt (semantic-analyze-current-context (point-max)))
          (prefix (oref ctxt prefix)))
     (with-semantic-tag (car prefix)
                        (should (equal "A" tag-name))
                        (should (equal "class" tag-type))
                        (should (not (semantic-tag-type-superclasses tag)))))))

(provide 'test/context)
;; context.el ends here
