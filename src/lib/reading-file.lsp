
    ;; Pegexel is a little exercises generator, using common-lisp.
    ;; Copyright (C) 2012 Yves Combe <yves@ycombe.net>

    ;; This program is free software: you can redistribute it and/or modify
    ;; it under the terms of the GNU General Public License as published by
    ;; the Free Software Foundation, either version 3 of the License, or
    ;; (at your option) any later version.

    ;; This program is distributed in the hope that it will be useful,
    ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
    ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    ;; GNU General Public License for more details.

    ;; You should have received a copy of the GNU General Public License
    ;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;
; File scanning with read
; UNUSED
(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read stream nil)
          while line
          collect line)))

;
; File scaning in string
;
(defun get-file-in-lines (filename)
  (with-open-file (stream filename)
		 (loop for line = (read-line stream nil)
		    while line
		    collect line
		    collect " ")))

(defun get-file-in-string (filename)
    (apply #'concatenate 'string (get-file-in-lines filename)))

(defun escape-bs-in-string (str)
  (let ((inside-string nil))
    (coerce (loop for char across str
	       collect char
	       when (and inside-string (equal char #\\)) collect char
	       when (equal char #\") do (setf inside-string (not inside-string)))
	    'string)))


(defun get-from-string (str)
  (let ((stream (make-string-input-stream  str)))
    (loop for line = (read stream nil)
       while line
       collect line)))


(defun read-grammar (filename)
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
(defvar grammar nil)
(defvar code nil)
(defun split-exercice (content)
  (setf grammar nil)
  (setf code nil)
  (let* ((part nil)
	 (exo-sep '(grammar code)))
    (dolist (item content)
      (cond ((member item exo-sep) (setf part item))
	    ((null part) nil)
	    (t (setf (symbol-value part) (cons item (symbol-value part))))))
    (when *debug* (format t "grammar~%~A~%/grammar~%code~%~A~%/code~%" grammar code))
    (values (reverse grammar)  (reverse code))))

