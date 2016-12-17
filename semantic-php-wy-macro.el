;;; semantic-php-wy-macro.el --- Custom semantic-php.wy grammar action macros

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

(require 'semantic/wisent/grammar)

;; TODO: create cleverer expand* macro's to accomodate cases like
;; inline_use_statements, parenthesized expressions, etc.

(defun semantic-php-wy-macro-INCLUDE-TAG (name system-flag &rest attributes)
  "Expand call to INCLUDE-TAG grammar macro.

Same as grammar-macros.el INCLUDE-TAG but allows NAME to be the
result of the expr rule. An include tag is emitted only when NAME
is a string, or NAME is list of one 'code tag with name
\"scalar\" and contains the include name in the :detail
attribute.

Return the form to create a semantic tag of class include.
See the function `semantic-tag-new-include' for the meaning of
arguments NAME, SYSTEM-FLAG and ATTRIBUTES."
  `(let (incname)
     (if (stringp ,name)
         (setq incname ,name)
       (when (and (equal 1 (length ,name))
                  (semantic-tag-of-class-p (car ,name) 'scalar))
         (setq incname (semantic-tag-name (car ,name)))))

     (when incname
       (wisent-cook-tag
        (wisent-raw-tag
         (semantic-tag-new-include (substring incname 1 -1) ,system-flag ,@attributes))))))

(defun semantic-php-wy-macro-EXPANDLIST (tags)
  "Expand a list of raw tags TAGS."
  `(dolist (tag ,tags ,tags)
     (wisent-cook-tag tag)))

(defun semantic-php-wy-macro-USEGROUP (name decls)
  "Resolve the names for use declarations inside a group statement.

NAME is the group name, DECLS is the compound use declarations."
  `(let (result)
     (dolist (decl ,decls (nreverse result))
       (let ((realtag (car (semantic-tag-type-members decl))))
         (semantic-tag-set-name realtag
                                (concat ,name
                                        "\\"
                                        (semantic-tag-name realtag)))
         (setcar (semantic-tag-type-members decl) realtag)
         (push decl result)))))

(defun semantic-php-wy-macro-USEDECL (name &optional alias)
  "Create the relevant tags for single use declaration.

NAME is the fully- or unqualified name, ALIAS is the optional alias."
  `(wisent-raw-tag
    (semantic-tag-new-include
     ,name
     nil)))

(defun semantic-php-wy-macro-USETYPE (decl type)
  "Set the type of a use declaration.

DECL is the result of the USEDECL macro, TYPE is the declaration
type (class, function, const)."
  `(dolist (tag (if (semantic-tag-p ,decl)
                    (list ,decl)
                  ,decl) ,decl)
     ;; TODO: This is wrong but works. We should really not need to
     ;; change the type (class, trait, etc), but instead create a new
     ;; tag with a different class (type, function, etc).
     (semantic-tag-put-attribute tag :type ,type)))

(provide 'semantic-php-wy-macro)
;;; semantic-php-wy-macro.el ends here
