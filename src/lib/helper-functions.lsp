    ;; Pegexel is a little exercises generator, using common-lisp.
    ;; Copyright (C) 2012 Yves Combe <yves@ycombe.net>

    ;; This program is free software: you can redistribute it and/or modify
    ;; it under the terms of the GNU Affero General Public License as published by
    ;; the Free Software Foundation, either version 3 of the License, or
    ;; (at your option) any later version.

    ;; This program is distributed in the hope that it will be useful,
    ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
    ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    ;; GNU Affero General Public License for more details.

    ;; You should have received a copy of the GNU Affero General Public License
    ;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;
;
;
(defun add-grammar (grammar)
;  (in-package cl-user)
  (setf *grammar* (append *grammar* (symbol-value grammar))))
(export  'add-grammar)


(defun debug-symbol (value)
  (when *debug* (format t "~A -> ~A~%"  (symbol-name value) (symbol-value value))))

;
; load supplementary grammar file
;
; code is evalued in cl-user package, bàck to current package at end
(defun load-grammar-file-and-eval-code (name &key (main nil))
  (let ((filename (if main name 
		      (first (directory (make-pathname :directory (pathname-directory *grammardir*) :name name :type "grammar")))))
	(grammar nil)
	(code nil)
	(current-package-name (package-name *package*)))
    (IN-PACKAGE :cl-user)
    (when *debug* (format t "Reading and parsing file ~A~%~A~%" filename *grammardir* ))
    (unless filename (error "File not found  !"))
    (multiple-value-setq (grammar code) (split-exercice (read-grammar filename)))
    (if *exo-grammar* (nconc *exo-grammar* grammar)
	(setf *exo-grammar* grammar))
    (if *exo-code* (nconc *exo-code* code)
	(setf *exo-code* code))    
    (eval (cons 'progn *exo-code*))
    (eval `(in-package ,current-package-name))))
