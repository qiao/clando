(in-package #:clando)


(defstruct task
  id           ; hash of description
  description  ; description of this task
  created-at   ; created date (YYYY-mm-dd)
  due-at       ; due date (YYYY-mm-dd)
  project      ; project this task belongs to (text description)
  priority)    ; priority of this task


(defun new-task (&rest args)
  "make a new task with `id' and `created-at' auto-generated"
  (let ((task (apply #'make-task args)))
    (setf (task-id task) (hash (task-description task)))
    (setf (task-created-at task) (current-timestamp))
    task))

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


(defun task->csv (task)
  "dump task into csv"
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
             (format nil "~A" value)))
    (let ((attr-values (mapcar #'task-attr 
                               '(id created-at due-at project 
                                 priority description))))
      (format nil "~{~A~^,~}" (mapcar #'format-value attr-values)))))


(defun csv->task (csv)
  "load task from csv"
  (labels ((split (sep str)
             (let ((acc nil))
               (labels ((recur (str)
                          (let ((pos (position sep str)))
                            (if pos
                                (progn
                                  (push (subseq str 0 pos) acc)
                                  (recur (subseq str (1+ pos))))
                                (push str acc)))))
                 (recur str))
               (nreverse acc)))
           (parse-nil (str)
             (if (string-equal str "NIL")
                 nil
                 str)))
      (destructuring-bind (id created-at due-at project 
                            priority . description)
                          (mapcar #'parse-nil (split #\, csv))
        (make-task :id id
                   :created-at created-at
                   :due-at due-at
                   :project project
                   :priority priority
                   :description (format nil "~{~A~^,~}" description)))))


(defun dump-tasks (tasks file-path)
  "dump tasks to file"
  (setf tasks (sort-tasks tasks))
  (with-open-file (fstream file-path 
                   :direction :output
                   :if-exists :supersede)
    (dolist (task tasks)
      (format fstream "~A~%" (task->csv task)))))


(defun load-tasks (file-path)
  "load tasks from file"
  (let ((tasks nil))
    (with-open-file (fstream file-path
                     :direction :input)
      (do ((line (read-line fstream nil)
                 (read-line fstream nil)))
          ((null line))
        (push (csv->task line) tasks)))
    tasks))


(defun main (&rest args)
  (pprint args)
  (let ((tasks nil))
    (push (new-task :description "hello, world") tasks)
    (push (new-task :description "world") tasks)
    (push (new-task :description "foo") tasks)
    (push (new-task :description "bar") tasks)
    (dump-tasks tasks *tasks-path*)
    (print (load-tasks *tasks-path*))))
