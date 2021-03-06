(in-package #:clando)


;;; utilities


(defun hash (str)
  "get the hash of a string"
  (flet ((byte-array->hex-string (byte-array)
           (format nil "~(~{~2,'0X~}~)" (coerce byte-array 'list))))
    (byte-array->hex-string (sb-md5:md5sum-string str))))


(defun current-timestamp ()
  "get the current timestamp, in format YYYY-mm-dd-HH:MM:SS"
  (multiple-value-bind (sec minute hour date month year)
                       (get-decoded-time)
    (format nil "~d-~2,'0d-~2,'0d-~2,'0d:~2,'0d:~2,'0d" 
                year month date   hour   minute sec)))


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
          ;; `pre' will be the prefix of `id' if every remaining id 
          ;; does not start with `pre'
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
  priority)    ; priority of this task


(defun task-hash (task)
  "get the hash of a task"
  (hash (concatenate 'string
                     (task-created-at task)
                     (task-description task))))


(defun new-task (&rest args)
  "make a new task with `id' and `created-at' auto-generated"
  (let ((task (apply #'make-task args)))
    (setf (task-created-at task) (current-timestamp))
    (setf (task-id task) (task-hash task))
    task))


(defun edit-task-description (description task)
  "return a new task with description changed"
  (let ((new-task (copy-task task)))
    (setf (task-description new-task) description)
    new-task))


(defun sort-dump-tasks (tasks)
  "sort tasks according to id"
  (sort tasks #'string< :key #'task-id))


(defun sort-list-tasks (tasks)
  "sort tasks according to created date"
  (sort tasks #'string< :key #'task-created-at))


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
      (string-join "," (mapcar #'format-value attr-values)))))


(defun csv->task (csv)
  "load task from csv"
  (labels ((parse-nil (str)
             (if (string-equal str "NIL")
                 nil
                 str)))
    (destructuring-bind (id created-at due-at project 
                         priority . description)
                        (mapcar #'parse-nil (string-split #\, csv))
      (make-task :id          id
                 :created-at  created-at
                 :due-at      due-at
                 :project     project
                 :priority    priority
                 :description (string-join "," description)))))


(defun dump-tasks (tasks filepath)
  "dump tasks to file"
  (setf tasks (sort-dump-tasks tasks))
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

(defun find-tasks-by-prefix (prefix tasks)
  (remove-if-not #'(lambda (task)
                     (string-startswith prefix (task-id task)))
                 tasks))


(defun load-pending-tasks ()
  (load-tasks *pending-tasks-path*))

(defun dump-pending-tasks (tasks)
  (dump-tasks tasks *pending-tasks-path*))

(defun load-done-tasks ()
  (load-tasks *done-tasks-path*))

(defun dump-done-tasks (tasks)
  (dump-tasks tasks *done-tasks-path*))


;;; commands


(defmacro with-find-tasks ((task tasks) prefixes &body body)
  (let ((prefix (gensym))
        (tasks-with-prefix (gensym))
        (task-count (gensym)))
    `(dolist (,prefix ,prefixes)
       (let* ((,tasks-with-prefix (find-tasks-by-prefix ,prefix ,tasks))
              (,task-count (length ,tasks-with-prefix)))
         (cond ((> ,task-count 1)
                (format t "Error: Ambiguous prefix ~A~%" ,prefix)
                (return))
               ((= ,task-count 1)
                (let ((,task (first ,tasks-with-prefix)))
                  ,@body))
               (t
                (format t "Error: Unknown prefix ~A~%" ,prefix)
                (return)))))))


(defun cmd-add (&rest args)
  (if (null args)
      (error "No descriptions specified")
      (let* ((description (string-join " " args))
             (task (new-task :description description))
             (pending-tasks (load-pending-tasks)))
        (push task pending-tasks)
        (dump-pending-tasks pending-tasks))))


(defun cmd-list (&rest args)
  (labels ((num-length (num)
             (length (princ-to-string num))))
    (let* ((pending-tasks (load-pending-tasks))
           (task-ids      (mapcar #'task-id pending-tasks))
           (pre-map       (prefixes task-ids))
           (max-id-length (apply #'max (mapcar #'(lambda (id)
                                                   (num-length 
                                                     (gethash id pre-map)))
                                               task-ids))))
      (mapc #'(lambda (task)
                (let ((pre (gethash (task-id task) pre-map))
                      (des (task-description task)))
                  (format t "~VA  ~A~%" max-id-length pre des)))
            (sort-list-tasks pending-tasks)))))


(defun cmd-finish (&rest prefixes)
  (let ((pending-tasks (load-pending-tasks))) 
    (with-find-tasks (task pending-tasks) prefixes
      (dump-pending-tasks (remove task pending-tasks))
      (dump-done-tasks (cons task (load-done-tasks))))))


(defun cmd-remove (&rest prefixes)
  (let ((pending-tasks (load-pending-tasks)))
    (with-find-tasks (task pending-tasks) prefixes
      (dump-pending-tasks (remove task pending-tasks)))))


(defun cmd-edit (&rest args)
  (if (< (length args) 2)
      (cmd-help)
      (let ((prefix        (first args))
            (description   (string-join " " (rest args)))
            (pending-tasks (load-pending-tasks)))
        (with-find-tasks (task pending-tasks) (list prefix)
          (let ((rest-tasks (remove task pending-tasks))
                (new-task (edit-task-description description task)))
            (dump-pending-tasks (cons new-task rest-tasks)))))))
  

(defun cmd-help (&rest args)
  (let ((help-text "usage: clando <command> [<args>]

available commands:
    add     <description>       Add a task with description
    list                        List all pending tasks with their ids
    finish  <id>                Mark the task of the giving id as finished
    edit    <id> <description>  Edit the description of a task
    remove  <id>                Remove the task of the given id
    help                        Show this message ~%"))
    (format t help-text)))


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
            '(cmd-add    #("add" "adds" "a" "create" "creates" "c"))
            '(cmd-list   #("" "list" "lists" "l" "lst" "show" "shows" "s"))
            '(cmd-help   #("help" "h" "?" "--help" "-h"))
            '(cmd-finish #("finish" "fin" "f" "done"))
            '(cmd-remove #("remove" "rm" "rem" "delete" "d" "del"))
            '(cmd-edit   #("edit" "e" "change" "mod" "modify"))))
