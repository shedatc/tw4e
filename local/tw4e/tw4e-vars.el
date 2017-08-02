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

(provide 'tw4e-vars)

;;; tw4e-vars.el ends here
