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

(defpackage :script
  (:documentation "Namespace for the main script")
  (:use :common-lisp))

;; hooks package contains functions directy callable fram grammar
(defpackage :hooks
  (:documentation "Namespace for the functions callable inside the grammar generation")
  (:use :common-lisp)
  (:use :script))

;; template package group code from template
(defpackage :template
  (:documentation "Namespace for the template exercise")
  (:use :hooks)
  (:use :script)
  (:use :common-lisp))

(in-package :hooks)
(export  'init-hooks)
(use-package :template)

(in-package :script)
(defvar *grammar* () "The grammar used by generate.")
(defvar *exo-grammar* () "grammar part of exercise template")
(defvar *exo-code* () "code part of exercise template")
(defvar *exo-variables* () "variables def part of exercise template")
(export '(*grammar* *exo-grammar* *exo-code*  *exo-variables* ))


(use-package :template)
(use-package :hooks)
(defvar *generate* () "WIll be set to #'generate")
(export  '*generate*)

;
; Arguments reading
;

; From Rosetta Code http://rosettacode.org/wiki/Command-line_arguments#Common_Lisp
; changed clisp (ext:arv) to ext:args, better for script usage
; tested with GCL, CLISP, SBCL, ECL
(defun my-argv ()
  "Get the command-lines arguments."
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

; from http://www.cliki.net/Portable%20Exit
; fixed :unix-code in sbcl
(defun quit (&optional code)
      ;; This group from "clocc-port/ext.lisp"
      #+allegro (excl:exit code)
      #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
      #+cmu (ext:quit code)
      #+cormanlisp (win32:exitprocess code)
      #+gcl (lisp:bye code)                     ; XXX Or is it LISP::QUIT?
      #+lispworks (lw:quit :status code)
      #+lucid (lcl:quit code)
      #+sbcl (sb-ext:quit) 
;     #+sbcl (sb-ext:quit :unix-code (typecase code (number code) (null 0) (t 1)))
      ;; This group from Maxima
      #+kcl (lisp::bye)                         ; XXX Does this take an arg?
      #+scl (ext:quit code)                     ; XXX Pretty sure this *does*.
      #+(or openmcl mcl) (ccl::quit)
      #+abcl (cl-user::quit)
      #+ecl (si:quit)
      ;; This group from 
      #+poplog (poplog::bye)                    ; XXX Does this take an arg?
      #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl
            kcl scl openmcl mcl abcl ecl)
      (error 'not-implemented :proc (list 'quit code)))

(defvar *args* (my-argv) "Command line arguments.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   get parameters
;

(defvar *debug* nil "show debug information")
(defvar *quiet* nil "suppress normal output")
(defvar *default-tex-environment* "Exercise" "if grammar specify TEX, set output in environment ENV.")
(defvar *no-escape* nil "do not escape backslashes in strings" )
(defvar *help*  nil "print usage information")
(defvar *run-in-source* nil "run directly from source (no installation)")
(defvar *output-directory* "pegexel-output/" "output directory to place generated files (relative to template file).")
(defvar *output-filename* nil  "output filename (default generated from template filename).")
(defvar *force-rewrite* nil "Force overwrite existing output file.")
(export '(*debug* *no-escape* *default-tex-environment*))

; show short long var string
(defparameter *accepted-arguments*
  '((t "-d" "--debug" *debug* nil)
    (t "-q" "--quiet" *quiet* nil)
    (t "-h" "--help" *help* nil)
    (t "-t" "--tex-env" *default-tex-environment* "ENV")
    (t "-n" "--no-escape-bs" *no-escape* nil)
    (t "-w" "--where" *output-directory* "DIR")
    (t "-o" "--output" *output-filename* "FILE")
    (t "-f" "--force"  *force-rewrite* nil)
    (nil "-s" "--run-in-source" *run-in-source* nil))
  "List of parsed script arguments")

; suppress name/value pair from list
(defun delete-param-pair (param list)
  "Suppress from a list a key and the following value. TODO: use remf ?" 
  (let ((nth (position param list :test #'equal)))
    (if nth (delete-if (constantly t) list :start nth :end (+ nth 2))
	list)))

; exported ? Not exported for now.  (export  'delete-param-pair)

(defun script-debug (&rest format-values)
  (when *debug* (apply #'format (cons *error-output* format-values))))
(export 'script-debug)

; get a parameter in short on long format and delete it from *args*
(defun get-parameter (short long  args &key (string nil) (var nil) (keep nil))
  "Parse parameter from arguments list." 
  (let* ((member-short (member  short args :test 'string=))
	 (member-long (member  long args :test 'string=))
	 (member-rest (or member-short member-long))
	 (opt (if member-short short long))
	 (stringval (if string (second member-rest)
			t)))
    (cond (member-rest
	   (when  var (setf (symbol-value var) stringval))
	   (unless keep (if string 
			    (delete-param-pair opt args) 
			    (delete opt args :test #'equal)))
	   (when var (script-debug "variable ~A set to ~A~%" 
				   (symbol-name var) 
				   (symbol-value var)))
	   t)
	  (t nil))))

(defun parse-arguments (accepted-arguments args)
  "Parse all accepted arguments from list."
  (loop for (nil short long var stringp nil) in accepted-arguments
       do (get-parameter short long args :string stringp :var var)))

(defun show-help ()
  "Show usage."
  (when *help*
    (flet ((get-description (var) (documentation var 'variable)))
      (format t "pegexel [options] file~%")
      (format t "~Twhere options are:~%")
      (loop for (show short long var string) in *accepted-arguments*
	 do (when show (format t "~T~T~3A ~:[      ~;~:*~6A~] or ~15A ~:[      ~;~:*~6A~] : ~T~A~%" short string long  string (get-description var))))
      (quit))))
 
;with GCL, *load-truename* is not set, but first args is script name
(defun get-script-path (args)
  "Get script path from *load-truename* or first command-line argument (GCL only ?)"
  (flet ((get-script-path-from-args (args)  (truename (first args)))
	 (remove-last (list) (let ((nth+1 (length list)))
			       (remove-if (constantly t) list :start (1- nth+1) :end nth+1))))
    (make-pathname 
     :directory (remove-last (pathname-directory
		 (handler-case 
		     *load-truename*
		   (unbound-variable (err) (get-script-path-from-args args))))))))

(parse-arguments *accepted-arguments* *args*)

(show-help)


(defvar *basedir* (get-script-path *args*) "base directory installation")
(defvar *libdir* (merge-pathnames (if *run-in-source* "lib/" "share/pegexel/lib/") *basedir*))
(defvar *hookdir*  (merge-pathnames (if *run-in-source* "hooks/" "share/pegexel/hooks/") *basedir*))
(defvar *grammardir*  (merge-pathnames (if *run-in-source* "grammars/" "share/pegexel/grammars/") *basedir*))

(defun load-files-from-directory (dir)
  "Load *.lsp files from directory."
  (loop for  filename in
       (sort (mapcar #'namestring (directory (make-pathname :directory (pathname-directory dir) :name :wild :type "lsp"))) #'string<)
     do (script-debug "Loading file ~A~%" filename)
       (load filename)))
(export '(*hookdir* *grammardir* load-files-from-directory) )

;; Libraries are loaded in script pakage -> Check that
(load-files-from-directory *libdir*)

(in-package :hooks)
(load-files-from-directory *hookdir*)

(script-debug "Exporting from hooks :~{~A ~}~%" (apropos-list "§-" :hooks))
(export (apropos-list "§-" :hooks))

(in-package :script)
;
; Initialisation
;
; I just want random not to give always the same results
(setf *random-state* (make-random-state t))

;(setf *filename* "exo1.etl")

;
; read and scan exercice file
;

(load-grammar-file-and-eval-code  *filename* :main t)

; generate and print
; generate before get output filename because extension can be changed in generation.
(let* ((syms (generate-exo *exo-grammar*))
       (output-filename (output-file *filename*  *output-directory*  *output-filename*)))
  (when (probe-file output-filename)
    (cond  (*force-rewrite* (script-debug "Deleting existing file ~A~%" output-filename)
			    (delete-file  output-filename))
	   (t (format *error-output* "File ~A exists! Use -f to overwrite." output-filename)
	      (quit))))
  (script-debug "Output filename ~A~%" (namestring output-filename))
  (with-open-file (stream output-filename :direction :output)
    (unless *quiet* (format t "Generating file ~A~%" output-filename))
    (format stream "~{~A~^~@[ ~]~}~^ ~%" (sym-to-string syms))))
