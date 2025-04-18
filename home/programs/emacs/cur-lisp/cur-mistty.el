;;; cur-mistty.el --- Extending Mistty's capabilities -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'mistty)
(require 'auth-source)

(defun cur-mistty-send-password ()
  "Read a password and send it to the current mistty buffer."
  (interactive)
  (unless (process-live-p mistty-proc) (user-error "No running process"))
  (mistty-send-string (read-passwd "Password: "))
  (mistty-send-string "\n")
  (message "Password sent."))

(provide 'cur-mistty)
;;; cur-mistty.el ends here
