;;; test/lexer.el --- Lexer test cases for semantic-php -*- lexical-binding: t; -*-

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

;;;; Integer literals
(ert-deftest semantic-php-test-lexer-integer-literals ()
  "Test integer literals (T_LNUMBER)"
  (with-test-buffer
   "1 123 0 012 0xcafeBABE 0Xdeadbeaf 0b00 0B100"
   (with-lex-tokens
    (dolist (token tokens)
      (should (equal 'T_LNUMBER (car token))))))
  (with-test-buffer
   "099 0xcafeGUY 0b123 1e1"
   (with-lex-tokens
    (dolist (token tokens)
      (should-not (equal 'T_LNUMBER (car token)))))))

;;;; Floating-point literals
(ert-deftest semantic-php-test-lexer-floating-point-literals ()
  "Test floating-point literals (T_DNUMBER)"
  (with-test-buffer
   ".5 0.5 5. 1e1 1E1 1e-1 1e+1 .5e-1 0e0 0e-0"
   (with-lex-tokens
    (dolist (token tokens)
      (should (equal 'T_DNUMBER (car token)))))))

;;;; Non-encapsed string literals
(ert-deftest semantic-php-test-lexer-nonencapsed-string-literals ()
  "Test non-encapsed string literals (T_STRING)"
  (with-test-buffer
   "test"
   (with-lex-tokens
    (should (equal 'T_STRING (car (car tokens)))))))

;;;; Encapsed string literals
(ert-deftest semantic-php-test-lexer-encapsed-string-literals ()
  "Test encapsed string literals (T_CONSTANT_ENCAPSED_STRING)"
  (with-test-buffer
   "\"test\" 'test' '!@#$%^&*()'
    '\\'' \"\\\"\" '\"test\"' 'test
    newline'"
   (with-lex-tokens
    (dolist (token tokens)
      (should (equal 'T_CONSTANT_ENCAPSED_STRING (car token)))))))

;;;; Variable names
(ert-deftest semantic-php-test-lexer-variable-names()
  "Test variable names (T_VARIABLE)"
  (with-test-buffer
   "$variable $$variable"
   (with-lex-tokens
    (should (equal 'T_VARIABLE (car (nth 0 tokens))))
    (should (equal 'DOLLAR     (car (nth 1 tokens))))
    (should (equal 'T_VARIABLE (car (nth 2 tokens)))))))

;;;; Punctuation
(ert-deftest semantic-php-test-lexer-punctuation()
  "Test punctuation recognized by the lexer"
  (with-test-buffer
   "Test\\Test"
   (with-lex-tokens
    (should (equal 'T_STRING       (car (nth 0 tokens))))
    (should (equal 'T_NS_SEPARATOR (car (nth 1 tokens))))
    (should (equal 'T_STRING       (car (nth 2 tokens))))))
  (with-test-buffer
   "test;"
   (with-lex-tokens
    (should (equal 'T_STRING  (car (nth 0 tokens))))
    (should (equal 'SEMICOLON (car (nth 1 tokens))))))
  (with-test-buffer
   "! @ $ % ^ & * ? / < > , . ~ |"
   (with-lex-tokens
    (should (equal 'NEGATE       (car (nth 0 tokens))))
    (should (equal 'AT           (car (nth 1 tokens))))
    (should (equal 'DOLLAR       (car (nth 2 tokens))))
    (should (equal 'MODULO       (car (nth 3 tokens))))
    (should (equal 'XOR          (car (nth 4 tokens))))
    (should (equal 'AMPERSAND    (car (nth 5 tokens))))
    (should (equal 'MULTIPLY     (car (nth 6 tokens))))
    (should (equal 'QUESTION     (car (nth 7 tokens))))
    (should (equal 'DIVIDE       (car (nth 8 tokens))))
    (should (equal 'LT           (car (nth 9 tokens))))
    (should (equal 'GT           (car (nth 10 tokens))))
    (should (equal 'COMMA        (car (nth 11 tokens))))
    (should (equal 'DOT          (car (nth 12 tokens))))
    (should (equal 'BITNOT       (car (nth 13 tokens))))
    (should (equal 'OR           (car (nth 14 tokens))))))
  (with-test-buffer
   "++ -- **"
   (with-lex-tokens
    (should (equal 'T_INC        (car (nth 0 tokens))))
    (should (equal 'T_DEC        (car (nth 1 tokens))))
    (should (equal 'T_POW        (car (nth 2 tokens)))))))

;;;; Keywords
(ert-deftest semantic-php-test-lexer-keywords()
  "Test keyword lexer"
  (with-test-buffer
   "class namespace print"
   (with-lex-tokens
    (should (equal 'T_CLASS     (car (nth 0 tokens))))
    (should (equal 'T_NAMESPACE (car (nth 1 tokens))))
    (should (equal 'T_PRINT     (car (nth 2 tokens)))))))

;;;; Open and close tags
(ert-deftest semantic-php-test-lexer-open-close-tags()
  "Test open/close tags"
  (with-test-buffer
   "<?= ?> <? <?php"
   (with-lex-tokens
    (should (equal 'T_OPEN_TAG_WITH_ECHO (car (nth 0 tokens))))
    (should (equal 'T_CLOSE_TAG          (car (nth 1 tokens))))
    (should (equal 'T_OPEN_TAG           (car (nth 2 tokens))))
    (should (equal 'T_OPEN_TAG           (car (nth 3 tokens)))))))

;;;; HTML before/after/between php tags
(ert-deftest semantic-php-test-lexer-html-tags()
  "Test html tags"
  (with-test-buffer-plain
   "html <?"
   (let ((tokens (semantic-lex (point-min) (point-max))))
     (should (equal 'T_INLINE_HTML (car (nth 0 tokens))))))
  (with-test-buffer-plain
   "<? ?> html"
   (let ((tokens (semantic-lex (point-min) (point-max))))
     (should (equal 'T_OPEN_TAG    (car (nth 0 tokens))))
     (should (equal 'T_CLOSE_TAG   (car (nth 1 tokens))))
     (should (equal 'T_INLINE_HTML (car (nth 2 tokens)))))))

;;;; Test nowdocs
(ert-deftest semantic-php-test-lexer-nowdocs()
  "Test nowdoc tokens"
  (with-test-buffer
   "<<<'now'
encapsed $encapsed
now;"
   (with-lex-tokens
    (should (equal 'T_START_HEREDOC           (car (nth 0 tokens))))
    (should (equal 'T_ENCAPSED_AND_WHITESPACE (car (nth 1 tokens))))
    (should (equal 'T_END_HEREDOC             (car (nth 2 tokens)))))))

;;;; Test heredocs
(ert-deftest semantic-php-test-lexer-heredocs()
  "Test heredoc tokens"
  (with-test-buffer
   "<<<here
encapsed encapsed
here;"
   (with-lex-tokens
    (should (equal 'T_START_HEREDOC            (car (nth 0 tokens))))
    (should (equal 'T_ENCAPSED_AND_WHITESPACE  (car (nth 1 tokens))))
    (should (equal 'T_END_HEREDOC              (car (nth 2 tokens))))))
  (with-test-buffer
   "<<<here
here
;"
   (with-lex-tokens
    (should (equal 'T_START_HEREDOC            (car (nth 0 tokens))))
    (should (equal 'T_ENCAPSED_AND_WHITESPACE  (car (nth 1 tokens))))
    (should (equal 'T_END_HEREDOC              (car (nth 2 tokens))))))
  (with-test-buffer
   "<<<here
$variable
here;"
   (with-lex-tokens
    (should (equal 'T_START_HEREDOC           (car (nth 0 tokens))))
    (should (equal 'T_ENCAPSED_AND_WHITESPACE (car (nth 1 tokens))))
    (should (equal 'T_END_HEREDOC             (car (nth 2 tokens))))))
  (with-test-buffer
   "<<<here
encapsed $variable encapsed

encapsed after newline

here;"
   (with-lex-tokens
    (should (equal 'T_START_HEREDOC           (car (nth 0 tokens))))
    (should (equal 'T_ENCAPSED_AND_WHITESPACE (car (nth 1 tokens))))
    (should (equal 'T_END_HEREDOC             (car (nth 2 tokens))))))
  (with-test-buffer
   "<<<here
{$variable}
here;"
   (with-lex-tokens
    (should (equal 'T_START_HEREDOC           (car (nth 0 tokens))))
    (should (equal 'T_ENCAPSED_AND_WHITESPACE (car (nth 1 tokens))))
    (should (equal 'T_END_HEREDOC             (car (nth 2 tokens)))))))

(ert-deftest semantic-php-test-lexer-heredocs-escaped-variables()
  "Test heredoc tokens with escaped variables"
  (with-test-buffer
   "<<<here
\\$test
here;"
   (with-lex-tokens
    (should (equal 'T_START_HEREDOC            (car (nth 0 tokens))))
    (should (equal 'T_ENCAPSED_AND_WHITESPACE  (car (nth 1 tokens))))
    (should (equal 'T_END_HEREDOC              (car (nth 2 tokens)))))))

(ert-deftest semantic-php-test-lexer-heredocs-looks-like-variables()
  "Test heredoc tokens matches dollars as T_ENCAPSED.. if not a variable"
  (with-test-buffer
"<<<here

test $ $$

here;"
  (with-lex-tokens
   (should (equal 'T_START_HEREDOC            (car (nth 0 tokens))))
   (should (equal 'T_ENCAPSED_AND_WHITESPACE  (car (nth 1 tokens))))
   (should (equal 'T_END_HEREDOC              (car (nth 2 tokens))))
   (should (equal 'SEMICOLON                  (car (nth 3 tokens)))))))

;;;; Handles all kinds of comments
(ert-deftest semantic-php-test-lexer-comments()
  "Test comment handling"
  (with-test-buffer
   "// test"
   (with-lex-tokens
    (should (= 0 (length tokens)))))
  (with-test-buffer
   "# test"
   (with-lex-tokens
    (should (= 0 (length tokens)))))
  (with-test-buffer
   "/* test */"
   (with-lex-tokens
    (should (= 0 (length tokens)))))
  (with-test-buffer
   "/* tëst */"
   (with-lex-tokens
    (should (= 0 (length tokens))))))

;;;; handles unicode without unmatched syntax errors
(ert-deftest semantic-php-test-lexer-unicode()
  "Test unicode string literals"
  (with-test-buffer
   "'☹'"
   (with-lex-tokens (should (equal 'T_CONSTANT_ENCAPSED_STRING (car (nth 0 tokens))))))
  ;; Unicode in heredocs currently fails because we run the lexer on
  ;; the contents, and the lexer is not built to handle arbitrary
  ;; string contents. Parsing of now-/heredocs needs another approach.
  (with-test-buffer
   "<<<UNICODE
٩(-̮̮̃-̃)۶ ٩(●̮̮̃•̃)۶ ٩(͡๏̯͡๏)۶ ٩(-̮̮̃•̃)
UNICODE;"
   (with-lex-tokens
    (should (equal 'T_START_HEREDOC (car (nth 0 tokens))))
    (should (equal 'T_ENCAPSED_AND_WHITESPACE (car (nth 1 tokens))))
    (should (equal 'T_END_HEREDOC (car (nth 2 tokens)))))))

(ert-deftest semantic-php-test-lexer-comment-parse-sexp-ignore()
  "Test parser wont explode when comment contains string delimiters"
  (with-test-buffer
   "
if (1) {
  // Don't explode
  $a = 1;
}

$b = true;
$c = $d;
"
   ;; TODO: above script causes the parser to emit invalid tag lists
   ;; when parse-sexp-ignore-comments is off. Luckily it's always on -
   ;; but it would be interesting to debug what's going on, there
   ;; might be an underlying problem here.
   (with-semantic-tags
    (should (equal 3 (length tags))))))

;; This test case is a simplified version of Zend_Gdata_Docs, for some
;; reason the lexer chokes on the html contents. The combination of
;; the first #header, together with the dot in 2.4em triggers the
;; error.
;;
;; Tips for debugging this:
;;  * put breakpoints in semantic-php-wy.el
;;  * or add debug statements to lexical analyzers in semantic-php.wy
;;  * inspect the end position of emitted inline_html tokens
;;  * perhaps the order of the analyzers in semantic-php-wy-lexer is wrong
(ert-deftest semantic-php-test-lexer-parse-inline-html()
  "Test parser wont explode on inline html"
  :expected-result :failed
  (with-test-buffer
   "function startHTML() {
?>
        #header h1 {
        }

        #header p {
            line-height: 2.4em;
        }

<?php } ?>"
   (with-semantic-tags
    (should (equal 1 (length tags))))))

(provide 'test/lexer)
;;; lexer.el ends here
