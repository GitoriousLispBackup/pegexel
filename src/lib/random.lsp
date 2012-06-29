;; round-to : round to part of unit (deicimal, third, etc... 
;; idea from http://www.codecodex.com/wiki/Round_a_number_to_a_specific_decimal_place#Common_Lisp
(defun round-to (number unit-th &optional (what #'round))
  (/ (funcall what (* number unit-th)) unit-th))

(defun p-list-of-k (p k)
  (loop repeat p collect k))

; Box-Muller method: http://fr.wikipedia.org/wiki/Loi_normale#Cas_de_la_loi_normale_.C3.A0_une_dimension
; result rounded to 1/unit-th
; 0 <= result < k 
(defun random-normal (k &optional &key (mu nil) (sigma nil) (round-unit-th 1))
  (let ((result k)
	(rmu (or mu (/ k 2)))
	(rsigma (or sigma (/ k 2))))
    (loop until (and (<= 0 result) (< result k))
       do (setf result (round-to (+ rmu (* rsigma (sqrt (* -2 (log (random 1.0)))) (cos (* 2 pi (random 1.0))))) round-unit-th)))
    result))

;
; random partition of n in p numbers. uniform on normal.
; Can be float or fract, specifying part of 1 to use as unit (unit-th 5 -> 0.2 as unit element)
;
(defun partition (n p &optional &key (mu nil) (sigma nil) (min 0) (normal nil) (unit-th 1) (float nil))
  (when (> (* min p) n) (error "Partition of ~A with ~A numbers >= ~A !~%" n p min))
  (let ((rlist (p-list-of-k p min)))
    (loop repeat (* (- n (* min p)) unit-th)
       do 
;	 (setf *random-state* (make-random-state t))
	 (let ((pos (if normal (random-normal p :mu mu :sigma sigma)
			(random p))))
	   (incf (nth pos rlist) (/ 1 unit-th))))
    (if float (mapcar #'float rlist)
	rlist)))
