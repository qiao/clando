(in-package #:clando)


;;; global


(defparameter *pending-tasks* nil)
(defparameter *done-tasks*    nil)


;;; utilities


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


(defun string-split (sep str)
  "split a string by separator into a list of substrings"
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


(defun string-join (sep strs)
  "join a list of strings by separator"
  (format nil (concatenate 'string "~{~A~^" sep "~}") strs))


(defun string-startswith (prefix str)
  "determine if the string starts with the prefix"
  (string-equal prefix (subseq str 0 (length prefix))))


(defun prefixes (ids)
  "get the mapping from each id to its shortest identifiable prefix"
  (let ((mapping (make-hash-table)))
    ;; for each id in ids, enumerate the length of its prefix from 0 and 
    ;; see if it collides with some other ids.
    (dolist (id ids)
      (do ((len 1 (1+ len)))
          ((> len 32)) ; md5sum
        (let ((pre (subseq id 0 len)))
          ;; `pre' will be the prefix of `id' if every id of 
          ;; the remaining ids does not start with the `pre'
          (when (every #'(lambda (id2)
                         (not (string-startswith pre (subseq id2 0 len))))
                     (remove id ids :test #'string-equal))
                (setf (gethash id mapping) pre)
                (return)))))
    mapping))


;;; task definition


(defstruct task
  id           ; hash of description
  description  ; description of this task
  created-at   ; created date (YYYY-mm-dd)
  due-at       ; due date (YYYY-mm-dd)
  project      ; project this task belongs to (text description)
  priority     ; priority of this task
  done)        ; whether this task is done


(defun new-task (&rest args)
  "make a new task with `id' and `created-at' auto-generated"
  (let ((task (apply #'make-task args)))
    (setf (task-id task) (hash (task-description task)))
    (setf (task-created-at task) (current-timestamp))
    task))


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
                                 priority done description))))
      (string-join "," (mapcar #'format-value attr-values)))))


(defun csv->task (csv)
  "load task from csv"
  (labels ((parse-nil (str)
             (if (string-equal str "NIL")
                 nil
                 str)))
    (destructuring-bind (id created-at due-at project 
                         priority done . description)
                        (mapcar #'parse-nil (string-split #\, csv))
      (make-task :id          id
                 :created-at  created-at
                 :due-at      due-at
                 :project     project
                 :priority    priority
                 :done        done
                 :description (string-join "," description)))))


(defun dump-tasks (tasks filepath)
  "dump tasks to file"
  (setf tasks (sort-tasks tasks))
  (with-open-file (fstream filepath
                   :direction :output
                   :if-exists :supersede)
    (dolist (task tasks)
      (format fstream "~A~%" (task->csv task)))))


(defun load-tasks (filepath)
  "load tasks from file"
  (let ((tasks nil))
    (with-open-file (fstream filepath
                     :direction :input
                     :if-does-not-exist nil)
      (when fstream
        (do ((line (read-line fstream nil)
                   (read-line fstream nil)))
            ((null line))
          (push (csv->task line) tasks))))
    tasks))


(defun load-pending-tasks ()
  (setf *pending-tasks* (load-tasks *pending-tasks-path*)))

(defun dump-pending-tasks ()
  (dump-tasks *pending-tasks* *pending-tasks-path*))

(defun load-done-tasks ()
  (setf *done-tasks* (load-tasks *done-tasks-path*)))

(defun dump-done-tasks ()
  (dump-tasks *done-tasks* *done-tasks-path*))


;;; command-line operations


(defun cmd-add (&rest args)
  (load-pending-tasks)
  (if (null args)
      (error "No descriptions specified")
      (let* ((description (string-join " " args))
             (task (new-task :description description)))
        (push task *pending-tasks*)
        (dump-pending-tasks))))


(defun cmd-list (&rest args)
  (load-pending-tasks)
  (flet ((print-task (task)
           (format t "~A~%" (task-description task))))
    (mapc #'print-task *pending-tasks*)))


(defun cmd-help (&rest args)
  (format t "Usage: ~%")); TODO


(defun dispatch (args &rest binds)
  "dispatch command-line arguments to handlers"
  (let ((app (first args))
        (cmd (or (second args) ""))
        (arg (cddr args)))
    (dolist (bind binds (cmd-help))
      (let ((handler (symbol-function (first bind)))
            (patterns (second bind)))
        (when (position cmd patterns :test #'string-equal)
          (apply handler arg)
          (return))))))



(defun main (&rest args)
  (dispatch args
            '(cmd-add  #("add" "adds" "a" "create" "creates" "c"))
            '(cmd-list #("" "list" "lists" "l" "lst" "show" "shows" "s"))
            '(cmd-help #("help" "h" "?" "--help" "-h"))))
