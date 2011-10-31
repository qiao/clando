(in-package #:clando)

(defconstant +tasks-path+ #P"~/.tasks")

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


(defun sort-tasks (tasks)
  "sort tasks according to id"
  (sort tasks #'string< :key #'task-id))


(defun format-task (task)
  "format task into csv"
  (labels ((task-attr (attr)
             ;; convert from attribute name to corresponding value
             ;; i.e. (task-attr 'id) -> (task-id task)
             (funcall 
               (symbol-function
                 (intern 
                   (concatenate 'string
                                "TASK-"
                                (symbol-name attr))
                   :clando))
               task))
           (format-value (value)
             (format nil "~A," value)))
    (let ((attr-values (mapcar #'task-attr 
                               '(id 
                                 created-at 
                                 due-at 
                                 project 
                                 priority
                                 description))))
      (apply #'concatenate 'string (mapcar #'format-value attr-values)))))


(defun dump-tasks (tasks file-path)
  "dump tasks to file"
  (setf tasks (sort-tasks tasks))
  (with-open-file (fstream file-path 
                   :direction :output
                   :if-exists :supersede)
    (dolist (task tasks)
      (format fstream "~A~%" (format-task task)))))


(defun main (&rest args)
  (pprint args)
  (let ((tasks nil))
    (push (make-task :description "hello") tasks)
    (push (make-task :description "world") tasks)
    (push (make-task :description "foo") tasks)
    (push (make-task :description "bar") tasks)
    (dump-tasks tasks +tasks-path+)))
