;;; tw4e-core.el --- Task Warrior from Emacs

;;; Code:

(require 's)
(require 'seq)
(require 'tw4e-utils)

;;; Functions:

;; Core Functions:

(defun tw4e--get-attribute (attribute task)
  "Return the value of the given ATTRIBUTE of the given TASK."
  (gethash attribute task))

;; Predicates:

(defun tw4e--more-urgent-than-p (task1 task2)
  "Tell if the urgency of TASK1 is greater than or equal to the urgency of TASK2."
  (let* ((urgency1 (tw4e--get-attribute 'urgency task1))
         (urgency2 (tw4e--get-attribute 'urgency task2)))
    (>= urgency1 urgency2)))

(defun tw4e--pending-p (task)
  "Tell if the given TASK is currently pending."
  (string-equal (tw4e--get-attribute 'status task)
                "pending"))

(defun tw4e--overdue-p (task)
  "Tell if the given TASK is overdue."
  (let* ((due (tw4e--get-attribute 'due-date task)))
    ;; due is a string formatted as YYYYMMDDTHHMMSSZ, i.e., ISO 8601 combined
    ;; date and time in UTC."
    (if (null due)
        nil ; No due date means cannot be overdue :p
      (time-less-p (current-time)
                   (date-to-time due)))))

(defun tw4e--active-p (task)
  "Tell if the given TASK is active."
  (let* ((start (tw4e--get-attribute 'start task)))
    ;; start is the date at which you started working on the task.
    (not (null start))))

                                        ; XXX Rely on the "blocking" attribute that is not available upstream.
(defun tw4e--blocking-p (task)
  "Tell if the given TASK is blocking at least one another."
  (let* ((blocking (tw4e--get-attribute 'blocking task)))
    ;; blocking is an array containing the UUIDs of the tasks that are blocked
    ;; the current one.
    (not (null blocking))))

                                        ; Implementation Details:
                                        ;     Look at the 'depends property, which is a string containing the
                                        ;     coma-separated list of task UUID that block the resolution of the given
                                        ;     TASK.
(defun tw4e--blocked-p (task)
  "Tell if the given TASK is blocked by at least one another."
  (let* ((depends (tw4e--get-attribute 'depends task)))
    ;; depends is a string containing the coma-separated list of the UUID of the
    ;; tasks that block the current one.
    (not (null depends))))

(defun tw4e--sort-by-urgency (tasks toc)
  "Sort TASKS by decreasing urgency. Keep the TOC unchanged."
  (list (seq-sort 'tw4e--more-urgent-than-p tasks) toc))

(defun tw4e--pending-tasks ()
  "Return the pending tasks and the corresponding table of content."
  (let* ((tt    (tw4e--fetch-and-read-tasks))
         (tasks (car tt))
         (toc   (cdr tt)))
    (cons tasks toc)))
    ;; (tw4e--sort-by-urgency tasks toc)))

(defun tw4e--get-attribute-format-string (attribute toc)
  "Return the format string suitable for the given task ATTRIBUTE and TOC."
;;   (format "%%-%ds" (tw4e--get-attribute-column-width attribute toc)))
;; (defun tw4e--get-column-value-format (column toc)
  (let* ((vf (tw4e--get-column-property attribute 'format)))
    (s-replace "*" (number-to-string (tw4e--get-attribute-column-width attribute toc)) vf)))

(defun tw4e--get-attribute-column-width (attribute toc)
  "Return the width of the column used to display ATTRIBUTE according to TOC."
  (cdr (assoc attribute toc)))

(defun tw4e--format-attribute (attribute task toc)
  "Return the properly formatted string representing the given ATTRIBUTE of the given TASK in the context of TOC."
  (format (tw4e--get-attribute-format-string attribute toc)
          (tw4e--get-attribute attribute task)))

(defun tw4e--format-strings-as-table-row (strings properties)
  "Return a table row containing the given STRINGS propertized with the given PROPERTIES."
  (concat (apply 'propertize
                 (cons (mapconcat 'identity strings " ") properties))
          "\n"))

(defun tw4e--format-task-as-table-row (task toc attributes properties)
  "For the given TASK, return a table row containing its ATTRIBUTES propertized with the given PROPERTIES."
  (let* ((strings (mapcar (lambda (attribute)
                            (tw4e--format-attribute attribute task toc))
                          attributes)))
    (tw4e--format-strings-as-table-row strings
                                       properties)))

(defun tw4e--get-column-property (column property)
  (cdr (assoc property (cdr (assoc column tw4e/columns-definitions)))))

(defun tw4e--get-column-label-format (column toc)
  (let* ((lf (tw4e--get-column-property column 'label-format)))
    (s-replace "*" (number-to-string (tw4e--get-attribute-column-width column toc)) lf)))

(defun tw4e--get-column-label (column toc)
  (let* ((lf (tw4e--get-column-label-format column toc)))
    (format lf (tw4e--get-column-property column 'label))))

;; (list (cons 'uuid 36) (cons 'priority 1) (cons 'tags 29) (cons 'due-date 10) (cons 'due-relative 3) (cons 'description 54) (cons 'urgency 6))
(defun tw4e--table-headers (toc)
  "Return the table headers."
  (let* ((properties '(face underline)))
    (tw4e--format-strings-as-table-row (list (tw4e--get-column-label 'priority     toc)
                                             (tw4e--get-column-label 'tags         toc)
                                             (tw4e--get-column-label 'due-date     toc)
                                             (tw4e--get-column-label 'due-relative toc)
                                             (tw4e--get-column-label 'description  toc)
                                             (tw4e--get-column-label 'urgency      toc)
                                             )
                                       properties)))

(defun tw4e--get-properties (task toc)
  "Return the text properties that should be used to colorize the TASK."
  (cond
   (t                       '(face tw4e/normal-face))
   ((tw4e--active-p task)   '(face tw4e/active-face))
   ((tw4e--overdue-p task)  '(face tw4e/overdue-face))
   ((tw4e--blocked-p task)  '(face tw4e/blocked-face))
   ((tw4e--blocking-p task) '(face tw4e/blocking-face))))

(defun tw4e--table-content (tasks toc)
  "Return the table content, i.e., a line per task."
  (let* ((properties '()))
    (s-join ""
            (mapcar (lambda (task)
                      (tw4e--format-task-as-table-row task
                                                      toc
                                                      '(priority tags due-date due-relative description urgency)
                                                      (tw4e--get-properties task toc)))
                    tasks))))

(defun tw4e--task-at-point (tasks)
  "Return the task at point by matching the current line number against the given TASKS list."
  (let* ((ln (line-number-at-pos)))
    (if (= ln 1) ;; The first line is used to displày the headers.
        (error "No task at point.")
      (elt tasks
           (- ln 2)))))

(defun tw4e--current-task-uuid ()
  "Return the current task's UUID."
  (cond
   ((boundp 'tw4e/task-uuid) tw4e/task-uuid)
   ((boundp 'tw4e/tasks)
    (tw4e--get-attribute 'uuid
                         (tw4e--task-at-point tw4e/tasks)))
   (t (error "Unable to guess current task's UUID."))))

(provide 'tw4e-core)

;;; tw4e-core.el ends here
