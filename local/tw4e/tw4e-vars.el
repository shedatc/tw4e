;;; tw4e-vars.el --- Customizable variables.

;;; Customizable variables:

(defgroup tw4e nil "Task Warrior for Emacs")

(defcustom tw4e/program-path "task" "Path to the task program."
  :type 'file
  :group 'tw4e)

(defcustom tw4e/program-arguments (list "rc.color:off" "rc.defaultwidth:0" "rc.editor:emacsclient")
  "Arguments used with every call to the task program."
  :type '(repeat string)
  :group 'tw4e)

(defcustom tw4e/headers-buffer-name "*Task Headers*"
  "Name of the buffer used to display and manipulate the tasks."
  :type 'string
  :group 'tw4e)

(defcustom tw4e/log-buffer-name "*Task Logs*"
  "Name of the buffer used to store various logs."
  :type 'string
  :group 'tw4e)

(defcustom tw4e/edit-buffer-name "*Task Edit*"
  "Name of the buffer used to edit a task."
  :type 'string
  :group 'tw4e)

;; Report Output:
;; uuid                                 p t                             due        dr  desc                                                   urg
;; 08fab458-8294-4f95-ac93-1fd7e587400f H Sicilia work                  2017-10-28 1d  Traductions manquantes (.po)                               19
(defcustom tw4e/columns-definitions
  "Define the format of the columns displayed in the headers buffer."
  '((uuid         . ((report-label . "uuid")          (format . "%-*s")  (label . "UUID")        (label-format . "%-*s")))
    (priority     . ((report-label . "priority")      (format . "%-1s")  (label . "P")           (label-format . "%-1s")))
    (tags         . ((report-label . "tags")          (format . "%-*s")  (label . "Tags")        (label-format . "%-*s")))
    (due-date     . ((report-label . "due.formatted") (format . "%-14s") (label . "Due")         (label-format . "%-14s")))
    (due-relative . ((report-label . "due.relative")  (format . "%-*s")  (label . "RT")          (label-format . "%-*s")))
    (description  . ((report-label . "description")   (format . "%-*s")  (label . "Description") (label-format . "%-*s")))
    (urgency      . ((report-label . "urgency")       (format . "%-*s")  (label . "Urg")         (label-format . "%-*s"))))
  :type 'list
  :group 'tw4e)

; XXX
;; ((uuid . 36) (priority . 1) (tags . 29) (due-date . 10) (due-relative . 3) (description . 54) (urgency . 6))
;; (setf tw4e/columns-definitions
;;   '((uuid         . ((report-label . "uuid")          (format . "%-*s")  (label . "UUID")        (label-format . "%-*s")))
;;     (priority     . ((report-label . "priority")      (format . "%-1s")  (label . "P")           (label-format . "%-1s")))
;;     (tags         . ((report-label . "tags")          (format . "%-*s")  (label . "Tags")        (label-format . "%-*s")))
;;     (due-date     . ((report-label . "due.formatted") (format . "%-14s") (label . "Due")         (label-format . "%-14s")))
;;     (due-relative . ((report-label . "due.relative")  (format . "%-*s")  (label . "RT")          (label-format . "%-*s")))
;;     (description  . ((report-label . "description")   (format . "%-*s")  (label . "Description") (label-format . "%-*s")))
;;     (urgency      . ((report-label . "urgency")       (format . "%-*s")  (label . "Urg")         (label-format . "%-*s")))))

(provide 'tw4e-vars)

;;; tw4e-vars.el ends here
