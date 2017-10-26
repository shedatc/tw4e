;;; tw4e-faces.el --- Customizable Faces.

;;; Faces:

(defgroup tw4e/faces nil "Faces used by the Task Warrior for Emacs package."
  :group 'tw4e
  :group 'faces)

(defface tw4e/normal-face '((((class color)) :foreground "black"
                             ;; :background "white"
                             ))
  "Face used to colorize the normal tasks."
  :group 'tw4e/faces)

(defface tw4e/active-face '((((class color)) :foreground "grey"
                             :background "pink"))
  "Face used to colorize the active tasks."
  :group 'tw4e/faces)

(defface tw4e/overdue-face '((((class color)) :foreground "pink"))
  "Face used to colorize the overdue tasks."
  :group 'tw4e/faces)

(defface tw4e/blocked-face '((((class color)) :foreground "white"
                              :background "black"))
  "Face used to colorize the blocked tasks."
  :group 'tw4e/faces)

(defface tw4e/blocking-face '((((class color)) :foreground "black"
                               :background "grey"))
  "Face used to colorize the blocking tasks."
  :group 'tw4e/faces)

(provide 'tw4e-faces)

;;; tw4e-faces.el ends here
