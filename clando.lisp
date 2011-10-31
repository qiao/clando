(in-package #:clando)


(defstruct task
  id           ; hash of description
  description  ; description of this task
  created-at   ; created date (YYYY-mm-dd)
  due-at       ; due date (YYYY-mm-dd)
  project      ; project this task belongs to (text description)
  priority)    ; priority of this task


;; wrap `make-task' to auto-generate `id' and `created-at'
(let ((orig-make-task #'make-task))
  (setf (symbol-function 'make-task)
        #'(lambda (&rest args)
            (let ((task (apply orig-make-task args)))
              (setf (task-id task)         (hash (task-description task)))
              (setf (task-created-at task) (current-timestamp))
              task))))


(defun hash (str)
  "get the hash for a string"
  (flet ((byte-array->hex-string (byte-array)
           (format nil "~(~{~2,'0X~}~)" (coerce byte-array 'list))))
    (byte-array->hex-string (sb-md5:md5sum-string str))))


(defun current-timestamp ()
  "get the current timestamp, in format YYYY-mm-dd"
  (let* ((time-struct (multiple-value-list (get-decoded-time)))
         (date  (fourth time-struct))
         (month (fifth time-struct))
         (year  (sixth time-struct)))
    (format nil "~d-~2,'0d-~2,'0d" year month date)))


;(defun dump-tasks (task-list file-path)
  ;(setf (task-list (sort task-list #'string< :key '#task-id)))
  ;(with-open-file (fstream file-path :direction :output)))


(defun main (&rest args)
  (pprint args)
  (pprint (current-timestamp))
  (pprint (make-task :description "hello, world")))
