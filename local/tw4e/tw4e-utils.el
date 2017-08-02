;;; tw4e-utils.el --- Utilities for tw4e

;;; Commentary:

;; This implementation use "task rc.color:off next" to get the list of tasks. It
;; output a table starting with headers as follow:
;;
;;     ID  Active Age Deps P Tag                    Recur Due Description                                                                                                            Urg
;;     --- ------ --- ---- - ---------------------- ----- --- ---------------------------------------------------------------------------------------------------------------------- -----
;;
;; The first row is called "Raw Names" as it provide the column names. The
;; second row is called "Raw Sizes" as it provide the size of each column in the
;; form of an amound of dashes (-).
;;
;; As of 2016, "task export" was not providing the same amount of information as
;; "task info". The following example illustrate that:
;;
;;     $ t 114 info
;;
;;     Name                  Value
;;     ID                    114
;;     Description           foo
;;     Status                Pending
;;     This task is blocking 115 bar
;;     Entered               2017-07-14 21:40:42 (50s)
;;     Last modified         2017-07-14 21:40:42 (50s)
;;     Tags                  work
;;     Virtual tags          BLOCKING PENDING READY TAGGED UNBLOCKED
;;     UUID                  3223f484-03ad-431a-8b7c-cb1bab4451dd
;;     Urgency                8.8
;;
;;         blocking      1 *    8 =      8
;;         tags        0.8 *    1 =    0.8
;;                                  ------
;;                                     8.8
;;
;;     $ t 114 export
;;     [
;;     {"id":114,"description":"foo","entry":"20170714T194042Z","modified":"20170714T194042Z","status":"pending","tags":["work"],"uuid":"3223f484-03ad-431a-8b7c-cb1bab4451dd","urgency":8.8}
;;     ]
;;
;; The "task info" command tell that 114 is blocking 115 but not the "task
;; export" one. The same hold true for the virtual tags.

;;; Code:

(require 'seq)
(require 'tw4e-vars)

;;; Functions

(defun tw4e--headers-buffer ()
  "Return the *Task Headers* buffer."
  (get-buffer-create tw4e/headers-buffer-name))

(defun tw4e--log-buffer ()
  "Return the *Task Logs* buffer."
  (get-buffer-create tw4e/log-buffer-name))

(defun tw4e--run (&optional command filter extra)
  (let* ((args (tw4e--build-program-args command filter extra))
         (cmdline (format "%s %s"
                          tw4e/program-path
                          (s-join " " args))))
    (with-current-buffer (tw4e--log-buffer)
      (insert (format "Run: %s\n" cmdline)))
    (shell-command cmdline
                   (current-buffer)
                   (tw4e--log-buffer))))

(defun tw4e--build-program-args (&optional command filter async)
  "Build the list of arguments to pass to the task program to run the given COMMAND with the given FILTER."
  (append tw4e/program-arguments
          (if filter
              (list filter))
          (list (if command command "tw4e"))
          (if async
              (list "&"))))

(defun tw4e--column-lengths (raw-sizes)
  "Compute the length of each column from the given RAW-SIZES string."
  (seq-map 'length
           (split-string raw-sizes " ")))

(defun tw4e--read-line ()
  "Read an entire line and advance the point."
  (beginning-of-line)
  (let* ((line (buffer-substring-no-properties (point)
                                               (line-end-position))))
    (forward-line)
    line))

;; (("ID" . 3) ("Active" . 6) ("Age" . 3) ("Deps" . 4) ("P" . 1) ("Tag" . 22) ("Recur" . 5) ("Due" . 3) ("Description" . 118) ("Urg" . 5))

(defun tw4e--read-toc ()
  "Read the report headers from the *Task* buffer to build the table of content."
  (beginning-of-buffer)
  (forward-line)
  (let* ((names (split-string (tw4e--read-line)
                              " "
                              t))
         (lengths (tw4e--column-lengths (tw4e--read-line))))
    (mapcar* 'cons names lengths)))

(defun tw4e--read-attribute (raw-name)
  "Return the symbol corresponding to the attribute macthing the given RAW-NAME."
  (cond
   ((equal raw-name "Urgency")     'urgency)
   ((equal raw-name "Description") 'description)
   ((equal raw-name "UUID")        'uuid)))

(defun tw4e--rtrim (str)
  (reverse (seq-drop-while (lambda (c) (char-equal c #x20)) (reverse str))))

(defun tw4e--read-value (attribute raw-value)
  "Normalize the RAW-VALUE of the given ATTRIBUTE."
  (cond
   ((equal attribute 'description) (tw4e--rtrim raw-value))
   ((equal attribute 'urgency)     (string-to-number raw-value))
   ((equal attribute 'uuid)        raw-value)))

(defun tw4e--read-task (toc)
  "Parse the current line according to the TOC to return the task as an hash table."
  (seq-reduce (lambda (task toc-entry)
                (let* ((name (car toc-entry))
                       (length (cdr toc-entry))
                       (start (point))
                       (end (+ start length)))
                  (if (<= end (line-end-position))
                      (let* ((raw-value (buffer-substring-no-properties start end))
                             (attribute  (tw4e--read-attribute name))
                             (value      (tw4e--read-value attribute raw-value)))
                        (puthash attribute value task)
                        (forward-char (+ length 1)) ;; Go to end+1.
                        task)
                    nil)))
              toc
              (make-hash-table :test 'equal)))

(defun tw4e--read-tasks ()
  "Read the tasks from the *Task* buffer."
  (let* ((toc (tw4e--read-toc))
         (tasks (list)))
    (progn
      (setf task (tw4e--read-task toc))
      (while (not (null task))
        (setf tasks (append tasks
                            (list task))
              task
              (tw4e--read-task toc)))
      tasks)))

(defun tw4e--fetch-and-read-tasks ()
  "Fetch the tasks into the *Task* buffer and read them."
  (with-temp-buffer
    (tw4e--run)
    (tw4e--read-tasks)))

(defun tw4e--parse-time-string (time-string)
  "Parse a TIME-STRING formatted as YYYYMMDDTHHMMSSZ (ISO 8601 combined date and time in UTC)."
  (let* ((year (substring time-string 0 4))
         (month (substring time-string 4 6))
         (day (substring time-string 6 8))
         (hour (substring time-string 9 11)) ; Skip the "T".
         (minute (substring time-string 11 13))
         (second (substring time-string 13 15))) ; Ignore the trailing "Z".
    (parse-time-string (format "%s-%s-%s %s:%s:%s" year month day
                               hour minute second))))

(provide 'tw4e-utils)

;;; tw4e-utils.el ends here
