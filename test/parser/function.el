;;; function.el --- Test cases for function tags -*- lexical-binding: t; -*-

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

(ert-deftest semantic-php-test-parser-function-declarations()
  "Test function declarations tags"
  (with-test-buffer
   "function test() {}"
   (with-semantic-first-tag (should (equal "test" tag-name))
                            (should (equal 'function tag-class))
                            (should (equal 0 (length (plist-get tag-attribs :arguments)))))))

(ert-deftest semantic-php-test-parser-function-arguments()
  "Test function arguments"
  (with-test-buffer
   "function test($a, $b) {}"
   (with-semantic-first-tag
    (let ((arguments (plist-get tag-attribs :arguments)))
      (should (equal 2 (length arguments)))
      (with-semantic-tag (nth 0 arguments)
                         (should (equal "$a" tag-name))
                         (should (equal 'variable tag-class)))
      (with-semantic-tag (nth 1 arguments)
                         (should (equal "$b" tag-name))
                         (should (equal 'variable tag-class)))))))

(ert-deftest semantic-php-test-parser-function-argument-optional()
  "Test function with optional arguments"
  (with-test-buffer
   "function test($a = null, $b = 123) {}"
   (with-semantic-first-tag
    (let ((arguments (plist-get tag-attribs :arguments)))
      (should (equal 2 (length arguments)))
      (with-semantic-tag (nth 0 arguments)
                         (should (equal "$a" tag-name))
                         (should (equal 'variable tag-class)))
      (with-semantic-tag (nth 1 arguments)
                         (should (equal "$b" tag-name))
                         (should (equal 'variable tag-class)))))))

(ert-deftest semantic-php-test-parser-function-argument-nonempty-array-initializer()
  "Test function with array initializer in argument"
  :expected-result :failed
  ;; old array-notation (but display as new notation)
  (with-test-buffer
   "function test($a = array(1, 2, 3)) {}"
   (with-semantic-first-tag
    (let ((arguments (plist-get tag-attribs :arguments)))
      (should (equal 1 (length arguments)))
      (with-semantic-tag (car arguments)
                         (should (equal "[]" (plist-get tag-attribs :default-value)))))))
  ;; short array-notation
  (with-test-buffer
   "function test($a = [1, 2, 3]) {}"
   (with-semantic-first-tag
    (let ((arguments (plist-get tag-attribs :arguments)))
      (should (equal 1 (length arguments)))
      (with-semantic-tag (car arguments)
                         (should (equal "[]" (plist-get tag-attribs :default-value))))))))

(ert-deftest semantic-php-test-parser-function-argument-empty-array-initializer()
  "Test function with empty array initializer in argument"
  :expected-result :failed
  ;; old array-notation (but display as new notation)
  (with-test-buffer
   "function test($a = array()) {}"
   (with-semantic-first-tag
    (let ((arguments (plist-get tag-attribs :arguments)))
      (should (equal 1 (length arguments)))
      (with-semantic-tag (car arguments)
                         (should (equal "[]" (plist-get tag-attribs :default-value)))))))
  ;; new array-notation
  (with-test-buffer
   "function test($a = []) {}"
   (with-semantic-first-tag
    (let ((arguments (plist-get tag-attribs :arguments)))
      (should (equal 1 (length arguments)))
      (with-semantic-tag (car arguments)
                         (should (equal "[]" (plist-get tag-attribs :default-value))))))))

(ert-deftest semantic-php-test-parser-function-argument-by-reference()
  "Test function with by-reference argument"
  (with-test-buffer
   "function test(&$a) {}"
   (with-semantic-first-tag
    (let ((arguments (plist-get tag-attribs :arguments)))
      (should (equal 1 (length arguments)))
      (with-semantic-tag (car arguments)
                         (should (equal "$a" tag-name))
                         (should (equal 'variable tag-class))
                         (should (equal '("&") (plist-get tag-attribs :typemodifiers))))))))

(ert-deftest semantic-php-test-parser-function-argument-variadic()
  "Test function with variadic arguments"
  (with-test-buffer
   "function test(...$a) {}"
   (with-semantic-first-tag
    (let ((arguments (plist-get tag-attribs :arguments)))
      (should (equal 1 (length arguments)))
      (with-semantic-tag (car arguments)
                         (should (equal "$a" tag-name))
                         (should (equal 'variable tag-class))
                         (should (equal '("...") (plist-get tag-attribs :typemodifiers))))))))

(ert-deftest semantic-php-test-parser-function-nested-declaration()
  "Test functions in functions"
  (with-test-buffer
   "function testA() { function testB() {} }"
   (with-semantic-first-tag
    (should (equal "testA" tag-name))
    (should (equal 'function tag-class))
    (should (equal 1 (length (plist-get tag-attribs :members))))
    (let ((members (plist-get tag-attribs :members)))
      (should (equal 1 (length members)))
      (with-semantic-tag (car members)
                         (should (equal "testB" tag-name))
                         (should (equal 'function tag-class)))))))

(ert-deftest semantic-php-test-parser-function-anonymous()
  "Test closures"
  (with-test-buffer
   "function($a, $b) use ($c, $d) {
function(){};
};"
   (with-semantic-first-tag
    (should (equal "closure" tag-name))
    (should (equal 'function tag-class))
    (should (equal 2 (length (plist-get tag-attribs :arguments))))
    (should (equal 2 (length (plist-get tag-attribs :lexical-scope))))
    (should (equal 1 (length (plist-get tag-attribs :members)))))))

(ert-deftest semantic-php-test-parser-function-call()
  "Test function call"
  (with-test-buffer
   "$test = <<<EOT
EOT
;"
   (semantic-fetch-tags)))

(ert-deftest semantic-php-test-parser-function-call-2()
  "Test function call"
  (with-test-buffer
   "test(<<<EOT
EOT
);"
   (semantic-fetch-tags)))

(ert-deftest semantic-php-test-parser-function-error-1()
  "Test something that might choke the parser"
  (with-test-buffer
   "
function x() {
    switch(1) {
    };

    return 1;
}"
   (semantic-fetch-tags)))

(provide 'test/parser/function)
;;; function.el ends here
