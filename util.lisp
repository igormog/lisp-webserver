(in-package :myweb.util)

(defvar *new-line* (concatenate 'string (string #\Return) (string #\Newline)))

(defun parse-request (stream)
  (let ((header (read-utf-8-string stream 10)))
    (if (eq (length header) 0)
	'()
	(if (equal (subseq header 0 4) "POST")
	    (parse-post-header header stream)
	    (parse-get-header header stream)))))

(defun read-utf-8-string (stream &optional (end 0))
  (let ((byte -1)
	(buffer (make-array 1 :fill-pointer 0 :adjustable t)))
    (handler-case 
	(loop do
	     (setq byte (read-byte stream))
	     (if (/= byte end) (vector-push-extend byte buffer))
	   while (/= byte end))
      (end-of-file ()))
    (trivial-utf-8:utf-8-bytes-to-string buffer)))

(defun response-write (text stream)
  (trivial-utf-8:write-utf-8-bytes text stream))

(defun parse-get-header (header stream)
  (cons "GET" 
	(cons (parse-path (subseq header (position #\/ header) (position #\Space header :from-end t)))
	      (parse-headers stream))))

(defun parse-headers (stream)
  (let ((headers nil)
	(header nil))
    (loop do
	 (setq header (read-utf-8-string stream 10))
	 (if (> (length header) 2) (setq headers (cons (parse-header header) headers)))
	 while (> (length header) 2))
    (reverse headers)))