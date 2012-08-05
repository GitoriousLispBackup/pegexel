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
(defun split-string (mark str)
  "Split string with mark as separator."
  (let ((length-mark (length mark)))
    (loop 
       for splitting = str then  (subseq splitting (+ pos length-mark))
       for pos = (search "," splitting)
       collect (subseq splitting 0 pos) 
       while pos)))

(defun intervalle-from-string (str)
  "Return (a b) if str is a-b, reading a and b as numbers.
   If str is nos a-b read str as number."
  (let ((str- (search "-" str)))
    (cond (str- (mapcar #'get-number-and-check (list (subseq str 0 str-) (subseq str (1+ str-)))))
	  (t  (get-number-and-check str)))))

(defun get-number-and-check (str)
  "Read a number from str.
   Raise an error if read does not return a number."
  (let ((num (read-from-string str)))
    (if (numberp num) num
	(error "Error parsing string ~A as number !" str))))

(defun levels (str)
  "Parse level string, return list of numbers."
  (let ((levels (mapcar #'intervalle-from-string (split-string "," str))))
    (sort (loop for value in levels
	     when (numberp value) collect value
	     when (consp value)
	     nconc (loop for num from (first value) to (second value)
		      collect num))
	  #'<)))

(defun set-levels (value)
  "Set levels used, giving precedence to argument."
  (when (and *levels* (stringp *levels*)) (setf *levels* (levels *levels*))
	(return-from set-levels))
  (setf *levels* (typecase value
		   (null (loop for i in *exo-levels* collect (first i)))
		   (string (levels value))
		   (list value)
		   (number (list value))
		   (t (loop for i in *exo-levels* collect (first i))))))

(defun set-levels-variable ()
  "Set ?levels (list of values)."
  (let ((level-var (template '?levels)))
    (eval `(defvar ,level-var nil))
    (pushnew level-var *variables*)
    (setf (get level-var 'no-reinit) t)
    (setf (symbol-value level-var) (apply #'append (mapcar  #'(lambda (x) (mapcar #'eval-if-needed (rest (rest (assoc x *exo-levels*))))) *levels*)))
    (script-debug "LEVELS ?levels is ~A~%" (symbol-value level-var))))

(defun init-levels ()
  "Initialise levels. Set *levels* (list of levels) and ?levels (list of values)."
  (let ((default (first *exo-levels*))
	(default-value nil))
    (when (equal (first default) :default)
      (setf default-value (second default))
      (pop *exo-levels*))
    (set-levels default-value)
    (set-levels-variable))
  (script-debug "Levels initialised to ~A~%" *levels*))
	