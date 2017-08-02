;;; packages.el --- tw4e layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sheda <sheda@fsfe.org>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst tw4e-packages '((tw4e :location local)))

(defun tw4e/init-tw4e ()
  "Initialize the tw4e package."
  (use-package tw4e
    :commands (tw4e/popup-headers tw4e/edit-mode)
    :init
    (evil-leader/set-key "at" 'tw4e/popup-headers)
    :config
    (tw4e/setup)
    (evilified-state-evilify tw4e/headers-mode tw4e/headers-mode-map)
    ))

(defun tw4e/post-init-tw4e ()
  "Post-initialize the tw4e package."

  ;; XXX Don't know why tw4e/edit-mode-map is not automagically bound to the
  ;; major mode leader key (,).
  (spacemacs/set-leader-keys-for-major-mode
    'tw4e/edit-mode "f" 'tw4e/edit-finish)
  (spacemacs/set-leader-keys-for-major-mode
    'tw4e/edit-mode "c" 'tw4e/edit-cancel)

  ;; XXX Kludge to make it work with the keyboard-layout setup for Bépo.
  (add-hook 'tw4e/headers-mode-hook
            (lambda ()
              (define-key evil-evilified-state-map "t" 'next-line)
              (define-key evil-evilified-state-map "s" 'previous-line)))
  )

;;; packages.el ends here
