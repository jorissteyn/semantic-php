;;; class-member.el --- Test cases for class member tags -*- lexical-binding: t; -*-

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

(ert-deftest semantic-php-test-parser-class-member-methods()
  "Test functions as class members"
  (with-test-buffer
   "
class Test {
  function testA() {}
  function testB($a, $b = null) {}
}"
   (with-semantic-first-tag
    (let ((members (plist-get tag-attribs :members)))
      (with-semantic-tag (nth 0 members)
                         (should (equal "testA" tag-name))
                         (should (equal 'function tag-class)))
      (with-semantic-tag (nth 1 members)
                         (should (equal "testB" tag-name))
                         (should (equal 'function tag-class))
                         (should (equal 2 (length (plist-get tag-attribs :arguments)))))))))

(ert-deftest semantic-php-test-parser-class-member-method-modifiers()
  "Test method modifiers"
  (with-test-buffer
   "
class Test {
  public function testPublic() {}
  protected function testProtected() {}
  private function testPrivate() {}
  function testOldSchool() {}
  final static function testFinalStatic() {}
}"
   (with-semantic-first-tag
    (let ((members (plist-get tag-attribs :members)))
      (with-semantic-tag (nth 0 members) (should (equal '("public") (plist-get tag-attribs :typemodifiers))))
      (with-semantic-tag (nth 1 members) (should (equal '("protected") (plist-get tag-attribs :typemodifiers))))
      (with-semantic-tag (nth 2 members) (should (equal '("private") (plist-get tag-attribs :typemodifiers))))
      (with-semantic-tag (nth 3 members) (should (equal '() (plist-get tag-attribs :typemodifiers))))
      (with-semantic-tag (nth 4 members) (should (equal '("final" "static") (plist-get tag-attribs :typemodifiers))))))))

(ert-deftest semantic-php-test-parser-class-member-variables-visibility()
  "Test variables with visibility modifiers as class members"
  (with-test-buffer
   "
class Test {
  public $testPublic;
  protected $testProtected;
  private $testPrivate;
}"
   (with-semantic-first-tag
    (let ((members (plist-get tag-attribs :members)))
      (with-semantic-tag (nth 0 members)
                         (should (equal "$testPublic" tag-name))
                         (should (equal 'variable tag-class))
                         (should (equal '("public") (plist-get tag-attribs :typemodifiers))))
      (with-semantic-tag (nth 1 members)
                         (should (equal "$testProtected" tag-name))
                         (should (equal 'variable tag-class))
                         (should (equal '("protected") (plist-get tag-attribs :typemodifiers))))
      (with-semantic-tag (nth 2 members)
                         (should (equal "$testPrivate" tag-name))
                         (should (equal 'variable tag-class))
                         (should (equal '("private") (plist-get tag-attribs :typemodifiers))))))))

(ert-deftest semantic-php-test-parser-class-member-variables-php4()
  "Test class properties PHP4-style"
  (with-test-buffer
   "
class Test {
  var $oldSchool;
}"
   (with-semantic-first-tag
    (let ((members (plist-get tag-attribs :members)))
      (with-semantic-tag (car members)
                         (should (equal "$oldSchool" tag-name))
                         (should (equal 'variable tag-class)))))))

(ert-deftest semantic-php-test-parser-class-member-static-variables()
  "Test class static variable properties"
  (with-test-buffer
   "
class Test {
  public static $withModifier;
}"
   (with-semantic-first-tag
    (let ((members (plist-get tag-attribs :members)))
      (with-semantic-tag (car members)
                         (should (equal "$withModifier" tag-name))
                         (should (equal 'variable tag-class))
                         (should (equal '("public" "static") (plist-get tag-attribs :typemodifiers))))))))

(ert-deftest semantic-php-test-parser-class-member-variables-initializers()
  "Test class properties with initializers"
  (with-test-buffer
   "
class Test {
  public $withInitializer = 123;
}"
   (with-semantic-first-tag
    (let ((members (plist-get tag-attribs :members)))
      (with-semantic-tag (car members)
                         (should (equal "$withInitializer" tag-name))
                         (should (equal 'variable tag-class)))))))

(ert-deftest semantic-php-test-parser-class-member-variables-multi-decl()
  "Test class properties multiple variables per visibility keyword"
  (with-test-buffer
   "
class Test {
  public $testA, $testB;
}"
   (with-semantic-first-tag
    (let ((members (plist-get tag-attribs :members)))
      (with-semantic-tag (nth 0 members)
                         (should (equal "$testA" tag-name))
                         (should (equal 'variable tag-class))
                         (should (equal '("public") (plist-get tag-attribs :typemodifiers))))
      (with-semantic-tag (nth 1 members)
                         (should (equal "$testB" tag-name))
                         (should (equal 'variable tag-class))
                         (should (equal '("public") (plist-get tag-attribs :typemodifiers))))))))

(ert-deftest semantic-php-test-parser-class-member-abstract-methods()
  "Test abstract methods"
  (with-test-buffer
   "
class Test {
  abstract function testAbstract();
  abstract public function testPublic();
}"
   (with-semantic-first-tag
    (let ((members (plist-get tag-attribs :members)))
      (with-semantic-tag (nth 0 members) (should (equal '("abstract") (plist-get tag-attribs :typemodifiers))))
      (with-semantic-tag (nth 1 members) (should (equal '("abstract" "public") (plist-get tag-attribs :typemodifiers))))))))

(ert-deftest semantic-php-test-parser-class-member-constructor-destructor()
  "Test constructor and destructor methods"
  (with-test-buffer
   "
class Test {
  public function __construct() {}
  public function __destruct() {}
  public function __nonstruct() {}
}"
   (with-semantic-first-tag
    (let ((members (plist-get tag-attribs :members)))
      (with-semantic-tag (nth 0 members) (should (plist-get tag-attribs :constructor-flag)))
      (with-semantic-tag (nth 1 members) (should (plist-get tag-attribs :destructor-flag)))
      (with-semantic-tag (nth 2 members) (should (not (plist-get tag-attribs :contructor-flag))))))))

(ert-deftest semantic-php-test-parser-class-member-old-constructor()
  "Test old-school constructor declaration"
  ;; Not implemented, we could check if the method name equals the
  ;; unqualified class name.
  :expected-result :failed
  (with-test-buffer
   "
class Test {
  public function Test() {}
}"
   (with-semantic-first-tag
    (let ((members (plist-get tag-attribs :members)))
      (with-semantic-tag (nth 0 members) (should (plist-get tag-attribs :constructor-flag)))))))

(ert-deftest semantic-php-test-parser-class-member-constants()
  "Test class constants"
  (with-test-buffer
   "
class Test {
  const TESTA = 123;
  const TESTB = 'abc';
  const TESTC = 1, TESTD = 2;
}"
   (with-semantic-first-tag
    (let ((members (plist-get tag-attribs :members)))
      (with-semantic-tag (nth 0 members)
                         (should (equal "TESTA" tag-name))
                         (should (equal 'constant tag-class)))
      (with-semantic-tag (nth 1 members)
                         (should (equal "TESTB" tag-name))
                         (should (equal 'constant tag-class)))
      (with-semantic-tag (nth 2 members) (should (equal "TESTC" tag-name)))
      (with-semantic-tag (nth 3 members) (should (equal "TESTD" tag-name)))))))

(provide 'test/parser/class-member)
;;; class-member.el ends here
