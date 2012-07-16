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

(load-files-from-directory *xcvbdir* :type "lisp")

(defun get-description (name &key (command nil) (file-type nil))
  "Get description list for script."
  (unless command (error "Missing command in ~A script~%" name))
  (list name command file-type))

(defun get-extern-script (str stream)
  "Read extern script description and content."
  (let ((description nil)
	(start-pos 0)
	(return-str ""))
    (in-package :template)
    (multiple-value-setq  (description start-pos) (read-from-string str))
    (in-package :script)
    (setq description 
	  (apply #'get-description description))
    (setq  *scripts*
	  (acons (first description)
		 (append (rest  description)
			 (cons (subseq str start-pos)
			       (loop  
				  for line = (read-line stream nil)
				  for pos = (search end-script line)
				  while line
				  unless pos collecting line into result
				  when pos collecting (subseq line 0 pos) into result
				  when pos do 
				    (progn 
				      (setq return-str (subseq line (+ pos (length end-script))))
				      (return result))
				  finally (return result))))
		 *scripts*))
    return-str))

(defun script-command (key)
  (second (assoc key *scripts*)))

(defun script-type (key)
  (third (assoc key *scripts*)))

(defun script-content (key)
  (cdddr (assoc key *scripts*)))

(defun run-script (name  params)
  (let* ((type (script-type name))
	(command (script-command name))
	(filename (namestring (make-pathname :name (string (gensym "temp-script-")) :type type))))
    (with-open-file (sc filename :direction :output :if-exists :supersede)
      (format sc "~{~A~%~}" (script-content name)))
    (let ((run-command (list 'xd:run-program/ `',(append (when command (list command))
							 (list filename) 
							 params)
							 :output :string)))
      (script-debug "Running script with command: ~A~%" run-command)
      (string-right-trim '(#\Space #\e #\t #\Newline) (eval run-command)))))

(export 'run-script)