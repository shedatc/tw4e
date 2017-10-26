;;; tw4e.el --- Task Warrior from Emacs

;; Add the following to your ~/.taskrc:
;;
;;     include ~/.tw4e.taskrc
;;
;; And put the following into ~/.tw4e.taskrc:
;;
;;     report.tw4e.description="Task Warrior for Emacs"
;;     report.tw4e.columns=uuid,description.desc,urgency
;;     report.tw4e.sort=urgency-
;;     report.tw4e.filter=status=pending

;;; Code:

(require 'tw4e-core)
(require 'tw4e-faces)
(require 'tw4e-vars)

;;; Keys:

(defvar tw4e/headers-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'tw4e/mark-done)
    (define-key map "e" 'tw4e/edit-task-at-point)
    (define-key map "r" 'tw4e/refresh-headers)
    map)
  "Keymap for the Task Headers mode.")

(defvar tw4e/edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c t f") 'tw4e/edit-finish)
    (define-key map (kbd "C-c t c") 'tw4e/edit-cancel)
    map)
  "Keymap for the Task Edit mode.")

;; Modes:

(define-derived-mode tw4e/headers-mode special-mode "Task Headers"
  :group 'tw4e
  (setq-local tw4e/tasks nil))

(define-derived-mode tw4e/edit-mode prog-mode "Task Edit"
  :group 'tw4e)

;; Commands:

(defun tw4e/setup ()
  "Setup the tw4e package."
  (interactive)
  (add-to-list 'auto-mode-alist '("\\.task\\'" . tw4e/edit-mode)))

(defun tw4e/popup-headers ()
  "List the task headers in a popup window."
  (interactive)
  (pop-to-buffer (get-buffer-create tw4e/headers-buffer-name))
  (if (not (string= major-mode "tw4e/headers-mode"))
      (progn
        (tw4e/refresh-headers)
        (goto-char (point-min))
        (forward-line))))

(defun tw4e/refresh-headers ()
  "Refresh the task headers."
  (interactive)
  (with-current-buffer (tw4e--headers-buffer)
    (let* ((orig-point (point)))
      (read-only-mode 0)
      (erase-buffer)
      (let* ((tt    (tw4e--pending-tasks))
             (tasks (car tt))
             (toc   (cadr tt)))
        (insert (tw4e--table-headers toc)
                (tw4e--table-content tasks toc))
        (goto-char orig-point)
        (tw4e/headers-mode)
        (setq-local tw4e/tasks tasks)))))

(defun tw4e/edit-task-at-point ()
  "Display details about the task at point in a popup window.  Rely on the tw4e/tasks buffer-local variable."
  (interactive)
  (let* ((task-uuid (tw4e--get-attribute 'uuid
                                         (tw4e--task-at-point tw4e/tasks))))
    (with-current-buffer (tw4e--log-buffer)
      (tw4e--run "edit" task-uuid t))))

(defun tw4e/edit-finish ()
  (interactive)
  (save-buffer)
  (kill-this-buffer)
  (tw4e/refresh-headers))

(defun tw4e/edit-cancel ()
  (interactive)
  (kill-this-buffer))

(defun tw4e/mark-done ()
  "Mark the current task as done."
  (interactive)
  (let* ((task-uuid (tw4e--current-task-uuid)))
    (with-temp-buffer
      (tw4e--run "done" task-uuid))
    (tw4e/refresh-headers)))

(provide 'tw4e)

;;; tw4e.el ends here
