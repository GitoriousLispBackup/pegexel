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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Grammar scanning part
;

; This part is largely based on the book "Paradigms of Artificial Intelligence Programming: Case Studies in Common Lisp", simple phrase generator exemple
; see  http://norvig.com/paip.html
; code source http://norvig.com/paip/simple.lisp

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))

(defun rule-rhs (rule)
  "The right hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))


(defun variable? (phrase)
  (cond ((not (symbolp phrase)) phrase)
	(t (if (variable-p phrase) (eval phrase)
	       phrase))))

; This function is modified version of Norvig's to fit my goal
(defun generate (phrase)
  "Generate a random sentence or phrase"
  (when *debug* (format t "generate ~A~%" phrase))
  (if (listp phrase)
      (hook-or-mappend phrase)
      (let ((choices (rewrites phrase)))
	(if (null choices)
	    (list (variable? phrase))
	    (funcall *generate* (random-elt choices))))))

(setf *generate* #'generate)


(defun hook-or-mappend (listval)
  (let ((First (first listval)))
    (typecase First
      (symbol (let ((fsym (first (member First (apropos-list "§-" :hooks)))))
		(if (fboundp fsym) 
		    (apply fsym (rest listval))
		    (mappend *generate* listval))))
      (t (mappend *generate* listval)))))

(defun generate-exo (grammar &optional (phrase 'exercise))
  (let ((start (template phrase)))
    (setf *grammar* grammar)
    (init-hooks)  
    (funcall *generate* start)))
