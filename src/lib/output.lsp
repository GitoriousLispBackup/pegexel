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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Output part
;
; Passing generated list to strings, with  upcase after '.' '?' '!'
;
(defvar *end-phrase* (list "." "!" "?" ) "Strings marking end of phrases.")

(defun word-capitalize (word)
  "String from symbol with first letter of word upcase."
  (cond ((null word) nil)
	      ((stringp word) word)
	      (t (format nil "~@(~a~)" word))))

(defun word-downcase (word)
  "String from symbol downcase."
  (cond ((null word) nil)
	((stringp word) word)
	(t (format nil "~(~a~)" word))))

;; 'nothing is ignored
;; 'nospace replaced by nil (used by last format not to add space after word)
;; 'upcase or end-phrase makes begin of next word uppercased.
(defun sym-to-string (phrases)
  "Pass list of symbols and string to list of well formated strings."
  (let ((last-was-end-phrase t))
    (loop for word in phrases unless (eq-template word 'nothing)
       collect (cond ((eq-template word 'upcase) 
		      (setf last-was-end-phrase t) "")
		     ((eq-template word 'nospace) nil)
		     ((eq-template word 'newline) (format nil "~%"))
		     (last-was-end-phrase (setf last-was-end-phrase nil) 
					  (word-capitalize word))
		     (t (when (member word  *end-phrase* :test #'equal) 
			  (setf last-was-end-phrase t))
			(word-downcase word)))
       when (or (eq-template word 'newline)
		(eq-template word 'upcase)) collect nil)))

