(in-package :myweb)

(defvar *listen-socket* nil)
(defvar *listen-thread* nil)

(defvar *request-mutex* (make-lock "request-mutex"))
(defvar *request-threads* (list))

(defvar *worker-mutex* (make-lock "worker-mutex"))
(defvar *workers* (list))
(defvar *worker-num* 0)
(defvar *idle-workers* (list))
(defvar *idle-workers-num* 0)
(defvar *request-queue* (list))

(defun start-http (host port &key (worker-limit 10) (idle-workers 1))
  (if (not *listen-socket*)
      (setq *listen-thread* 
	    (make-thread (lambda () (http-acceptor host port worker-limit idle-workers)) 
			 :name "socket-acceptor"))
      "http server already started"))

(defun http-acceptor (host port worker-limit idle-workers)
  ;; Start Listening on specified host and port 
  (setq *listen-socket* (socket-listen host port
				       :reuse-address t
				       :element-type '(unsigned-byte 8)
				       :backlog (* worker-limit 4)))
  (let ((request-id 0)
	(worker-id 0))
    (loop while *listen-thread* do
	 (let* ((socket (socket-accept *listen-socket* :element-type '(unsigned-byte 8))))
	   (incf request-id)
	   (acquire-lock *worker-mutex*)
	   (cond 
	     ;; All workers busy, append request to queue
	     ((>= *worker-num* worker-limit)
	      (push (cons request-id socket) *request-queue*))
	     ;; If idle worker available
	     ((> *idle-workers-num* 0)
	      ;; Get worker from idle workers
	      (push (cons request-id socket) *request-queue*)
	      (condition-notify (caar *idle-workers*)))
	     ;; Add new Worker
	     (t (incf worker-id)
		(incf *worker-num*)
		(setq *workers* 
		      (cons 
		       (make-thread
			(lambda () 
			  (worker-thread request-id socket idle-workers))
			:name (concatenate 'string 
					   "socket-worker-"
					   (prin1-to-string worker-id))) 
		       *workers*)))
	     )
	   (release-lock *worker-mutex*)
	   t))))