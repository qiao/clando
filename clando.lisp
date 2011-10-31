(in-package #:clando)

(defstruct task
  id           ; md5 hash of description
  description  ; description of this task
  created-at   ; created date (in ANSI date format)
  due-at       ; due date (in ANSI date format)
  project      ; project this task belongs to (text description)
  priority)    ; priority of this task


;; override `make-task' to auto-generate task-id
(let ((orig-make-task #'make-task))
  (setf (symbol-function 'make-task)
        #'(lambda (&rest args)
            (let ((task (apply orig-make-task args)))
              (setf (task-id task) (md5 (task-description task)))
              task))))


(defun md5 (str)
  (flet ((byte-array-to-hex-string (bytevec)
           (format nil "~(~{~2,'0X~}~)" (coerce bytevec 'list))))


    (byte-array-to-hex-string (sb-md5:md5sum-string str))))
(defun main (&rest args)
  (princ (make-task :description "hello, world"))
  (print args))
