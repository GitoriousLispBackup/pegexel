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
; File scaning in string
;
(defun get-file-in-lines (filename)
  "Read file into a list of string separated by newlines.
   Parsing of script is passed to get-extern-script"
  (with-open-file (stream filename)
		 (loop 
		    for line = (read-line stream nil)
		    for pos = (search *begin-script* line)
		    while line
		    unless pos collect line
		    when pos collect (subseq line 0 pos)
		    when pos collect (get-extern-script (subseq line (+ pos (length *begin-script*))) stream)
		    collect (format nil "~%"))))

(defun get-file-in-string (filename)
  "concatenate strings to make file into only one string."
    (apply #'concatenate 'string (get-file-in-lines filename)))

(defun escape-bs-in-string (str)
  "Double '\' to escape them for lisp reader."
  (let ((inside-string nil))
    (coerce (loop for char across str
	       collect char
	       when (and inside-string (equal char #\\)) collect char
	       when (equal char #\") do (setf inside-string (not inside-string)))
	    'string)))

(defun get-from-string (str)
  "Parse string with lisp reader, in package :template."
  (with-input-from-string (stream str) 
    (let ((res nil))
    (in-package :template)
    (cl:setf res 
	  (cl:loop for line = (cl:read stream nil)
	     while line
	     collect line))
    (cl:in-package :script)
    res)))

(defun read-grammar (filename)
  "Read and parse grammar from filename."
  (let ((string-lines (get-file-in-string filename)))
    (get-from-string 
     (if *no-escape* string-lines
	 (escape-bs-in-string string-lines)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; After reading, getting grammar part and code part of the exercise file
;
; get GRAMMAR part and CODE part 

; i can't dereference let vars ?
(defvar grammar nil "Local to put read grammar in.")
(defvar code nil "Local to put read grammar in.")
(defvar variables nil "Local to put read variables in.")
(defvar levels nil "Local to put read variables in.")
(defvar exo-sep (list (template 'grammar) 'grammar 
		      (template 'code) 'code
		      (template 'variables) 'variables
		      (template 'levels) 'levels)
  "Match template symbol with sexpr destination." )

(defun split-exercice (content)
  "Split parsed content into variable, grammar, code parts."
  (setf grammar nil)
  (setf code nil)
  (setf variables nil)
  (setf levels nil)
  (let* ((part nil))
    (dolist (item content)
      (let ((sep-item (getf exo-sep item)))
	(cond (sep-item (setf part sep-item))
	      ((null part) nil)
	      (t (setf (symbol-value part) (cons item (symbol-value part)))))))
    (script-debug "Parsed :~%grammar~%~A~%/grammar~%code~%~A~%/code~%variables~%~A~%/variables~%levels~%~A~%/levels~%" grammar code variables levels)
    (values (reverse grammar)  (reverse code) (reverse variables) (reverse levels))))

;
; load supplementary grammar file
;
; code is evalued in cl-user package, bàck to current package at end
(defun load-grammar-file-and-eval-code (name &key (main nil))
  "Read, parse, set variables and eval code from a template file."
  (let ((filename (if main name 
		      (first (directory (make-pathname :directory (pathname-directory *grammardir*) :name name :type "grammar")))))
	(grammar nil)
	(code nil)
	(variables nil))
    (script-debug "Reading and parsing file ~A~%" filename)
    (unless filename (error "File not found  !"))
    (multiple-value-setq (grammar code variables levels) (split-exercice (read-grammar filename)))
    (if *exo-grammar* (nconc *exo-grammar* grammar)
	(setf *exo-grammar* grammar))
    (if *exo-code* (nconc *exo-code* code)
	(setf *exo-code* code))
    (if  *exo-variables* (nconc *exo-variables* variables)
	 (setf *exo-variables* variables))
    (if *exo-levels* (nconc  *exo-levels* levels)
	(setf *exo-levels* levels))
    (when variables (defvar-all))
    (when code (eval (cons 'progn *exo-code*)))
    (when levels (init-levels))))
(export 'load-grammar-file-and-eval-code)
