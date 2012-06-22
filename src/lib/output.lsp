
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Output part
;
; Passing generated list to strings, with  upcase after '.' '?' '!'
;
(defvar *end-phrase* (list "." "!" "?" ))

(defun word-capitalize (word)
  (cond ((null word) nil)
	      ((stringp word) word)
	      (t (format nil "~@(~a~)" word))))

(defun word-downcase (word)
  (cond ((null word) nil)
	((stringp word) word)
	(t (format nil "~(~a~)" word))))


(defun sym-to-string (phrases)
  (let ((last-was-end-phrase t))
    (loop for word in phrases
       collect (cond ((equal word 'nospace) nil)
		     ((equal word 'newline) (format nil "~%"))
		     (last-was-end-phrase (setf last-was-end-phrase nil) 
					  (word-capitalize word))
		     (t (when (member word  *end-phrase* :test #'equal) 
			  (setf last-was-end-phrase t))
			(word-downcase word)))
       when (equal word 'newline) collect nil)))

