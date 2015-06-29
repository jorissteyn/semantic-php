;;; class.el --- Test cases for class tags -*- lexical-binding: t; -*-

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

(ert-deftest semantic-php-test-parser-class-declarations()
  "Test tags for regular class declarations"
  (with-test-buffer
   "class Test {}"
   (with-semantic-first-tag
    (should (equal "Test" tag-name))
    (should (equal 'type tag-class))
    (should (equal "class" (plist-get tag-attribs :type)))
    (should (equal [7 20] tag-overlay))
    (should (equal 'class_declaration_statement tag-reparse-symbol)))))

(ert-deftest semantic-php-test-parser-class-declarations-conditional()
  "Test tags for conditional class declarations"
  (with-test-buffer
   "
if (true) {
    class Test {}
}
if (true):
    class Test {}
endif;
"
   (with-semantic-tags
    (with-semantic-tag (nth 0 tags)
                       (should (equal [24 37] tag-overlay))
                       (should (equal 'class_declaration_statement tag-reparse-symbol)))

    (with-semantic-tag (nth 1 tags)
                       (should (equal [55 68] tag-overlay))
                       (should (equal 'class_declaration_statement tag-reparse-symbol))))))

(ert-deftest semantic-php-test-parser-class-inheritance()
  "Test class declarations inheritance (extends)"
  (with-test-buffer
   "class Test extends TestAbstract {}"
   (with-semantic-first-tag (should (equal "TestAbstract" (plist-get tag-attribs :superclasses))))))

(ert-deftest semantic-php-test-parser-class-interface-implementation()
  "Test classes implementing interfaces (implements)"
  (with-test-buffer
   "class Test implements TestA, TestB {}"
   (with-semantic-first-tag
    (should (equal '("TestA" "TestB") (plist-get tag-attribs :interfaces)))
    (should (equal [7 44] tag-overlay))
    (should (equal 'class_declaration_statement tag-reparse-symbol)))))


(ert-deftest semantic-php-test-parser-class-type-modifiers()
  "Test class declaration with type modifiers (abstract, final)"
  (with-test-buffer
   "abstract class Test {}"
   (with-semantic-first-tag (should (equal "Test" tag-name))
                            (should (equal 'type tag-class))
                            (should (equal "class" (plist-get tag-attribs :type)))
                            (should (equal '("abstract") (plist-get tag-attribs :typemodifiers)))))
  (with-test-buffer
   "final class Test {}"
   (with-semantic-first-tag (should (equal "Test" tag-name))
                            (should (equal 'type tag-class))
                            (should (equal "class" (plist-get tag-attribs :type)))
                            (should (equal '("final") (plist-get tag-attribs :typemodifiers))))))

(provide 'test/parser/class)
;;; class.el ends here
