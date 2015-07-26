;;; semantic-php.el --- Semantic integration for PHP -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Joris Steyn

;; Author: Joris Steyn <jorissteyn@gmail.com>
;; Created: 1 Nov 2014
;; Keywords: languages
;; Homepage: https://github.com/jorissteyn/semantic-php
;; Version: 0

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

;;; Commentary:

;; Installation from VCS for development purposes:
;;
;; 1. Clone the semantic-php repository
;; 2. Run 'make dist' to generate the parser and autoload definitions
;; 3. Load the generated semantic-php/loaddefs.el in your init file:
;;
;;    (add-to-list 'load-path "~/path/to/semantic-php")
;;    (load "~/path/to/semantic-php/loaddefs.el")

;;; Code:

(require 'semantic-php-wy)
(require 'semantic)
(require 'semantic/doc)
(require 'subr-x)

;;;###autoload
(add-hook 'php-mode-hook 'semantic-php-default-setup)

;;;###autoload
(defun semantic-php-default-setup ()
  "Setup semantic-php in the current buffer"
  (semantic-php-wy--install-parser)

  (setq
   ;; Lexical analysis
   semantic-lex-analyzer 'semantic-php-wy-lexer

   ;; Syntax table modifications
   semantic-lex-syntax-modifications
   '(
     (?= ".")
     (?& ".")
     (?+ ".")
     (?- ".")
     (?| ".")
     (?< ".")
     (?> ".")
     (?% ".")
     (?' "\"")
     (?\" "\"")
     (?` "\"")
     (?_ "w")
     (?$ "_")
     (?/ ". 124b")
     (?* ". 23")
     (?\n "> b")
     (?# "< b"))

   ;; Semantic requires this expression for line-comments,
   ;; if lexing without major mode
   semantic-lex-comment-regex "\\s<\\|\\(/\\*\\|//\\)"

   ;; Not related to semantic, this variable needs to be turned on in
   ;; order for comments to not get non-comment syntax properties in
   ;; their contents. This is also done by cc-mode (and thus
   ;; php-mode), and is only here to ensure correct parsing without a
   ;; major mode (test cases, batch project scan)
   parse-sexp-ignore-comments t

   ;; Separators to use when finding context prefix
   semantic-type-relation-separator-character '("::" "->")
   semantic-command-separation-character ";"

   ;; Tag expansion
   semantic-tag-expand-function 'semantic-php-expand-tag

   ;; imenu setup
   semantic-imenu-summary-function 'semantic-format-tag-prototype
   imenu-create-index-function 'semantic-create-imenu-index

   ;; Specify the labels of different tag types
   semantic-bucketize-tag-class 'semantic-php-bucketize-tag-class

   semantic-symbol->name-assoc-list '((namespace . "Namespaces")
                                      (class     . "Classes")
                                      (interface . "Interfaces")
                                      (trait     . "Traits")
                                      (variable  . "Variables")
                                      (constant  . "Constants")
                                      (function  . "Functions"))

   semantic-symbol->name-assoc-list-for-type-parts '((constant . "Constants")
                                                     (variable . "Properties")
                                                     (function . "Methods"))))

;; Define all modes applicable for semantic-php.
(define-child-mode web-mode php-mode)

(defun semantic-php-expand-tag (tag)
  "Expand compound declarations found in TAG into separate tags.

If the name of the tag is a cons cell, assume (name . (start . end))
and set bounds accordingly."
  ;; TODO: Improve the incremental reparse of compound statements like
  ;; 'public $a, $b;'.
  nil)

(defun semantic-php-bucketize-tag-class (tag)
  "Get the type of given TAG.

This function augments the standard bucketize behaviour by
emitting separate symbols for classes, interfaces and traits."
  (let ((tag-class (semantic-tag-class tag))
        (tag-attribs (semantic-tag-attributes tag)))
    ;; For type tags, return the type attribute as set by the parser
    ;; (class, interface, trait). For all other tags, return the
    ;; regular tag class.
    (if (eq tag-class 'type)
        ;; Convert string to symbol accepted by semantic-symbol->*
        (intern (plist-get tag-attribs :type))
      tag-class)))

(define-mode-local-override semantic-tag-components
  php-mode (tag)
  "Return a list of components for TAG."
  (cond ((semantic-tag-of-class-p tag 'type)
	 (semantic-tag-type-members tag))
        ((semantic-tag-of-class-p tag 'function)
         (append (semantic-tag-function-arguments tag)
                 (semantic-tag-type-members tag)))
        (t nil)))

(define-mode-local-override semantic-get-local-variables
  php-mode (&optional point)
  "Get local values from the context of point.

This implementation is very different from
`semantic-get-local-variables-default'. We use the current parser
result to extract variable tags based on the current context, or
the top-level context when not in a function context. It will not
re-parse part of the buffer."
  (let ((functiontag (semantic-current-tag-of-class 'function))
        (functionparent (car-safe (semantic-find-tags-by-type
                           "class" (semantic-find-tag-by-overlay))))
        classparen    ;; the parent class of the function parent
        alltags       ;; collect all tags in scope (TODO: use scope object)
        variabletags  ;; list of variable tags
        namelist)     ;; the names of tags in variabletags (used for dedupping)

    (setq alltags
          (if functiontag
              ;; In a function.
              (append (semantic-tag-function-arguments functiontag)
                      (semantic-tag-type-members functiontag))
            ;; In the toplevel space.
            (semantic-fetch-tags)))

    ;; Find local variables and remove duplicates.
    (dolist (tag alltags variabletags)
      (when (and (semantic-tag-of-class-p tag 'variable)
                 (not (member (semantic-tag-name tag) namelist)))
        (push (semantic-tag-name tag) namelist)
        (push tag variabletags)))

    ;; Handle special function variables/keywords.
    (when functionparent
      ;; If in non-static context, add $this.
      (unless (member "static" (semantic-tag-get-attribute functiontag :typemodifiers))
        (push (semantic-tag-new-variable "$this" functionparent nil) variabletags))

      ;; Find the parent class of the parent, this is the type of the
      ;; parent keyword.
      (setq classparent (car-safe (semantic-tag-type-superclasses functionparent)))
      (when classparent
        (push (semantic-tag-new-variable "parent" classparent nil) variabletags))

      ;; Add self as variable (while not actually a variable).
      (push (semantic-tag-new-variable "self" functionparent nil) variabletags))

    (nreverse variabletags)))

(define-mode-local-override semantic-analyze-split-name
  php-mode (name)
  "Split up NAME by namespace parts."
  ;; Note: namespaces in PHP are not hierarchical so we need to figure
  ;; out a way to never walk the split name all the way down. We
  ;; actually only want to split the name in two pieces:
  ;;   A\B\C -> A\B, C
  ;; but semantic calls this function repeatedly untill there's
  ;; nothing left to split, this is not easy to change.
  (let ((parts (delete "" (split-string name "\\\\"))))
    (if (= (length parts) 1)
        (car parts)
      parts)))

(define-mode-local-override semantic-find-tags-included
  php-mode (&optional table)
  "Find all include/require tags in TABLE.

TABLE is a tag table.  See `semantic-something-to-tag-table'.

Override the default behaviour to look in members of toplevel
tags. Unlike in C, include statements can appear anywhere in a
file."
  (semantic-find-tags-by-class 'include
                               (semantic-flatten-tags-table table)))

(define-mode-local-override semantic-documentation-for-tag
  php-mode (&optional tag nosnarf)
  "Find documentation from TAG and return it as a clean string.

TAG might have DOCUMENTATION set in it already.  If not, there may be
some documentation in a comment preceding TAG's definition which we
can look for.  When appropriate, this can be overridden by a language specific
enhancement.
Optional argument NOSNARF means to only return the lexical analyzer token for it.
If nosnarf if 'lex, then only return the lex token."
  (let ((docstring (semantic-tag-get-attribute tag :documentation)))
    (unless (stringp docstring)
      (setq docstring (semantic-php-doc-snarf-comment-for-tag tag))
      (semantic-tag-put-attribute tag :documentation docstring))
    docstring))

(defun semantic-php-doc-snarf-comment-for-tag (tag)
  "Extract a docstring from the docblock preceding TAG.

semantic-php does not use semantic-doc-snarf-comment-for-tag:
* snarf-comment-for-tag makes assumptions on the character
   classes inside the documentation string, this is very error
   prone and fails for common cases like when embedding URLs in
   the comment
* semantic requires comment-end which is not set by cc-mode or php-mode"
  (let ((docblock "")
        (docstring ""))
    (save-excursion
      ;; Find the tag.
      (semantic-go-to-tag tag)
      (beginning-of-line)
      (backward-char)

      ;; Extract the docblock contents.
      (when (looking-back "\*/")
        (let ((docend (match-beginning 0))
              docstart)
          (when (search-backward "/\*\*" nil t)
            (setq docstart (match-end 0)
                  docblock (buffer-substring-no-properties docstart docend))))))

    ;; Split the contents and produce a docstring.
    (dolist (line (split-string docblock "\n" t) docstring)
      (setq line (string-trim line))
      (setq docstring (concat docstring
                              "\n"
                              (if (not (string= "*" line))
                                  (string-remove-prefix "* " line)))))))

(provide 'semantic-php)
;;; semantic-php.el ends here
