;;; test/php-faux-mode.el --- Dummy major mode for tests

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

;; Function overrides for semantic are only activated when the major
;; mode is named 'php-mode'. For the purpose of the test suite, we
;; enable a dummy php-mode because nothing in semantic-php depends on
;; a specific php-mode version.

(unless (featurep 'php-mode)
  (define-derived-mode php-mode fundamental-mode
    (setq mode-name "PHP-FAUX")))

(provide 'test/php-faux-mode)
;;; test/php-faux-mode.el ends here
