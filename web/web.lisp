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
