#!/usr/bin/sbcl --script
;#!/usr/bin/gcl -f
;#!/usr/bin/sbcl --script
;#!/usr/bin/clisp -q -q
;#!/bin/ecl -shell

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
; Arguments reading
;

; From Rosetta Code http://rosettacode.org/wiki/Command-line_arguments#Common_Lisp
; changed clisp (ext:arv) to ext:args, better in script usage
; tested with GCL, CLISP, SBCL, ECL
(defun my-argv ()
  (or
   #+clisp (cons "clisp" ext:*args*)
   #+sbcl sb-ext:*posix-argv*
   #+clozure (ccl::command-line-arguments)
   #+gcl si:*command-args*
   #+ecl (loop for i from 0 below (si:argc) collect (si:argv i))
   #+cmu extensions:*command-line-strings*
   #+allegro (sys:command-line-arguments)
   #+lispworks sys:*line-arguments-list*
   nil))

(defvar *args* (my-argv))


(defvar *grammar* "The grammar used by generate.")
(defvar *exo-grammar* ())
(defvar *exo-code* ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   get parameters
;

(defvar *debug*  nil "show debug information")
(defvar *default-tex-environment* "exercice" "set output inside tex environment ENV (unused if grammar does not specify TEX)")
(defvar *no-escape* nil "do not escape backslashes in strings" )
(defvar *help*  nil "print usage information")
(defvar *run-in-source* nil "run directly from source (no installation)")

; show short long var string description
(defparameter *accepted-arguments*
  '((t "-d" "--debug" *debug* nil)
    (t "-h" "--help" *help* nil)
    (t "-t" "--tex-environment" *default-tex-environment* "ENV")
    (t "-n" "--no-escape-bs" *no-escape* nil)
    (nil "-s" "--run-in-source" *run-in-source* nil))
  "List of arguments")

(defun delete-param-pair (param list)
  (let ((nth (position param list :test #'equal)))
    (if nth (delete-if (constantly t) list :start nth :end (+ nth 2))
	list)))
(export  'delete-param-pair)

(defun get-parameter (short long  &key (string nil) (var nil) (keep nil))
  (let* ((member-short (member  short *args* :test 'string=))
	 (member-long (member  long *args* :test 'string=))
	 (member-rest (or member-short member-long))
	 (opt (if member-short short long))
	 (stringval (if string (second member-rest)
			t)))
    (cond (member-rest
	   (when  var (setf (symbol-value var) stringval))
	   (unless keep (if string (delete-param-pair opt *args*) (delete opt *args* :test #'equal)))
	   (when (and *debug* var) (format t "variable ~A set to ~A~%" (symbol-name var) (symbol-value var)))
	   t)
	  (t nil))))

(defun parse-arguments (arguments)
  (loop for (nil short long var stringp nil) in arguments
       do (get-parameter short long :string stringp :var var)))

(defun show-help ()
  (when *help*
    (flet ((get-description (var) (documentation var 'variable)))
      (format t "pegexel [options] file~%")
      (format t "~Twhere options are:~%")
      (loop for (show short long var string) in *accepted-arguments*
	 do (when show (format t "~T~T~3A ~:[      ~;~:*~6A~] or ~20A ~:[      ~;~:*~6A~] : ~T~A~%" short string long  string (get-description var))))
      (quit))))
 

;with GCL, *load-truename* is not set, but first args is script name
(defun get-script-path ()
  (flet ((get-script-path-from-args ()  (truename (first *args*)))
	 (remove-last (list) (let ((nth+1 (length list)))
			       (remove-if (constantly t) list :start (1- nth+1) :end nth+1))))
    (make-pathname 
     :directory (remove-last (pathname-directory
		 (handler-case 
		     *load-truename*
		   (unbound-variable (err) (get-script-path-from-args))))))))

(parse-arguments *accepted-arguments*)

(show-help)

(defvar *basedir* (get-script-path) "base directory installation")
(defvar *libdir* (merge-pathnames (if *run-in-source* "lib/" "share/pegexel/lib/") *basedir*))
(defvar *hookdir*  (merge-pathnames (if *run-in-source* "hooks/" "share/pegexel/hooks/") *basedir*))
(defvar *grammardir*  (merge-pathnames (if *run-in-source* "grammars/" "share/pegexel/grammars/") *basedir*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; hooks package contains functions directy callable fram grammar
(defpackage :hooks
  (:use :cl)
  (:use :cl-user))
(in-package hooks)
(defvar *values-to-init* nil "List of couples (var value) to init at generation start")
(defun init-hooks ()
  (loop for (var value) in *values-to-init*
       do (setf (symbol-value var) value)))
(in-package cl-user)

(loop for  filename in 
     (sort (mapcar #'namestring (directory (make-pathname :directory (pathname-directory *libdir*) :name :wild :type "lsp"))) #'string<)
   do (when *debug* (format t "Loading library file ~A~%" filename))
     (load filename))

(loop for  filename in 
     (sort (mapcar #'namestring (directory (make-pathname :directory (pathname-directory *hookdir*) :name :wild :type "lsp"))) #'string<)
   do (when *debug* (format t "Loading hooks file ~A~%" filename))
     (in-package hooks)
     (load filename)
     (in-package cl-user))


;
; Initialisation
;
; I just want random not to give always the same results
(setf *random-state* (make-random-state t))


; reading exercise description file
(defvar *filename* (first (reverse *args*)) "last elt of args has to be the filename")
(debug-symbol '*filename*)

;(setf *filename* "exo1.etl")

;
; read and scan exercice file
;
(load-grammar-file-and-eval-code  *filename* :main t)

; generate and print 
(format t "~{~A~^~@[ ~]~}~^ ~%" (sym-to-string (generate-exo *exo-grammar*)))
