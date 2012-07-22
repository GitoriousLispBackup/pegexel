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

(defun check-number3 (str)
  "Check if str is between 000 and 999"
  (and (string< "000" str)
       (string< str "999")))

(defun output-name-from-input-name (output-directory input-name)
  "Get output file name from template name + number."
  (let* ((name (concatenate 'string (pathname-name input-name) "_"))
	 (files* (directory (make-pathname :directory (pathname-directory output-directory) 
					  :name :wild
					  :type *output-type*)))
	 (files (loop for f in files* when (equal 0 (search name (pathname-name f)))
		   collect  (subseq (pathname-name f) (length name))))
 	 (number (loop for f in files when (check-number3 f)
		       maximizing (read-from-string f) into max
		       finally (return (1+ (or max  0))))))
    (make-pathname :directory (pathname-directory output-directory)
		   :name (concatenate 'string name (format nil "~3,'0d" number))
		   :type *output-type*)))

(defun base-name (path)
  "base name of path"
  (when path (namestring
	      (make-pathname :name (pathname-name path)
			   :type (pathname-type path)))))

(defun get-directory-from-path (path)
  (if path (namestring (make-pathname :directory (pathname-directory path))) 
      ""))

(defun is-absolute (dir)
  (equal :absolute (first (pathname-directory (or dir "")))))

(defun merge-directories (inputdir where outputdir)
  (append (pathname-directory where)
	  (cdr  ;remove :relative from the front
           (pathname-directory inputdir))
	  (cdr (pathname-directory outputdir))))

(defun get-full-pathname (inputdir where outputdir)
  "merges pathnames with output dir, input dir (if not absolute), output file dir."
  (let ((checked-inputdir (unless (is-absolute inputdir) inputdir)))
    (make-pathname :directory (merge-directories checked-inputdir  where outputdir))))

(defun check-file-type (file)
  (if (not (equal *output-type* (pathname-type file)))
      (concatenate 'string file "." *output-type*)
      file))

(defun output-file (input where output)
  "Get output file."
  (when (string= "--" output) (return-from output-file output))
  (let* ((output-file (when output (check-file-type (base-name output))))
	 (input-directory (get-directory-from-path input))
	 (output-directory (get-directory-from-path output))
	 (outdir (ensure-directories-exist (get-full-pathname input-directory where output-directory))))
    (namestring (if output 
		    (pathname (concatenate 'string (namestring outdir) output-file))
		    (output-name-from-input-name outdir input)))))

(defun output-stream (outfilename)
  (when (string= "--" outfilename) (return-from output-stream *STANDARD-OUTPUT*))
  (cond (*force-rewrite* (open outfilename :direction :output :if-exists :supersede))
	((probe-file outfilename)
	 (format *error-output* "File ~A exists! Use -f to overwrite.~%" outfilename)
	 (quit))
	(t (open outfilename :direction :output))))