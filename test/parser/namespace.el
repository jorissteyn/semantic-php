;;; namespace.el --- Test cases for namespace tags -*- lexical-binding: t; -*-

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

(ert-deftest semantic-php-test-parser-namespace-declarations()
  "Test namespace declarations tags"
  (with-test-buffer
   "namespace A;"
   (with-semantic-first-tag (should (equal "A" tag-name))
                            (should (equal 'type tag-class))
                            (should (equal "namespace" tag-type))))
  (with-test-buffer
   "namespace A\\B\\C;"
   (with-semantic-first-tag (should (equal "A\\B\\C" tag-name))
                            (should (equal 'type tag-class))))
  (with-test-buffer
   "namespace A\\B\\C {}"
   (with-semantic-first-tag (should (equal "A\\B\\C" tag-name))
                            (should (equal 'type tag-class)))))

;; TODO: this always just worked, why does the parser choke on it now?
;; (ert-deftest semantic-php-test-parser-namespace-bogus-declarations()
;;   "Test namespace tags for invalid declarations"
;;   (with-test-buffer
;;    "namespace \\A;"
;;    (with-semantic-first-tag (should (equal nil tag)))))

(ert-deftest semantic-php-test-parser-namespace-unnamed-declarations()
  "Test namespace tags for anonymous namespaces"
  (with-test-buffer
   "namespace {}"
   (with-semantic-first-tag (should (equal "\\" tag-name)))))

(ert-deftest semantic-php-test-parser-namespace-declarations-brace-block()
  "Test namespace declaration with brace block"
  (with-test-buffer
   "namespace TestNS {
class TestClass {}
}"
   ;; A namespace with a brace block suggests multiple namespaces in
   ;; one file. In those cases, don't list the tags as siblings of the
   ;; namespace but nest them under their respective namespace.
   (with-semantic-first-tag (should (equal "TestNS" tag-name))
                            (should (equal 1 (length (plist-get tag-attribs :members)))))))

(provide 'test/parser/namespace)
;;; namespace.el ends here
