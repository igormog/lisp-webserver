(in-package :myweb.util)

(defvar *new-line* (concatenate 'string (string #\Return) (string #\Newline)))

(defun parse-request (stream)
  (let ((header (read-utf-8-string stream 10)))
    (if (eq (length header) 0)
	'()
	(if (equal (subseq header 0 4) "POST")
	    (parse-post-header header stream)
	    (parse-get-header header stream)))))
