;;; cur-consult-projectile.el --- Integrating Consult and Projectile -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'cur-base)
(require 'consult)
(require 'projectile)

(defgroup cur-consult-projectile ()
    "Integrate Consult and Projectile."
    :group 'consult
    :group 'projectile
    :prefix "cur-consult-projectile-")

(defvar cur-consult-projectile--files
  `( :name "Directories"
     :narrow ?d
     :default nil
     :category file
     :state ,#'consult--file-state
     :items ,(append '("./")
                     (projectile-project-dirs (projectile-project-root)))))

(defvar cur-consult-projectile--dirs
  `( :name "Files"
     :narrow ?f
     :default nil
     :category file
     :state ,#'consult--file-state
     :items ,(projectile-project-files (projectile-project-root))))

(defvar cur-consult-projectile--buffers
  `( :name     "Buffers"
     :narrow   ?b
     :category buffer
     :face     consult-buffer
     :history  buffer-name-history
     :default  t
     :state    ,#'consult--buffer-state
     :items    ,(lambda ()
                  (let ((root (projectile-project-root)))
                    (consult--buffer-query
                     :sort 'visibility
                     :directory root
                     :as #'consult--buffer-pair)
                    ;; Alternative Implementation
                    ;; (mapcar (lambda (buffer)
                    ;;           (cons (buffer-name buffer) buffer))
                    ;;         (projectile-project-buffers))
                    ))))

(defcustom cur-consult-projectile-buffer-sources
  '(cur-consult-projectile--buffers)
  "Sources used by `cur-consult-projectile-buffer'.
See `consult--multi' for a description of the source data structure."
  :type '(repeat symbol)
  :group 'cur-consult-projectile)

(defun cur-consult-projectile-buffer ()
  "Switch to buffer in project.
The default list of sources is frome `cur-consult-projectile--buffers'."
  (interactive)
  (consult-buffer cur-consult-projectile-buffer-sources))

;; (defcustom cur-consult-projectile-find-file-sources
;;   '(cur-consult-projectile--buffers)
;;   "Sources used by `consult-buffer'.
;; See also `consult-project-buffer-sources'.
;; See `consult--multi' for a description of the source data structure."
;;   :type '(repeat symbol)
;;   :group 'cur-consult-projectile)

;; (defun cur-consult-projectile-find-file ()
;;   ""
;;   (interactive)
;;   (let ((default-directory (projectile-project-root)))
;;     (consult--multi '(cur-consult-projectile--dirs
;; 		      cur-consult-projectile--files)
;; 		    :sort t)))

;; (defun cur-consult-projectile--select-multi ()
;;   (consult--multi `(( :name     "Buffers"
;;                       :narrow   ?b
;;                       :category buffer
;;                       :face     consult-buffer
;;                       :history  buffer-name-history
;;                       :default  t
;;                       :state    ,#'consult--buffer-state
;;                       :items    ,(lambda ()
;;                                    (let ((root (projectile-project-root)))
;;                                      (consult--buffer-query
;;                                       :sort 'visibility
;;                                       :directory root
;;                                       :as #'consult--buffer-pair)
;;                                      ;; Alternative Implementation
;;                                      ;; (mapcar (lambda (buffer)
;;                                      ;;           (cons (buffer-name buffer) buffer))
;;                                      ;;         (projectile-project-buffers))
;;                                      )))
;;                     ( :name "Directories"
;;                       :narrow ?d
;;                       :default nil
;;                       :category file
;;                       :state ,#'consult--file-state
;;                       :items ,(append '("./")
;;                                       (projectile-project-dirs (projectile-project-root))))

;;                     ( :name "Files"
;;                       :narrow ?f
;;                       :default nil
;;                       :category file
;;                       :state ,#'consult--file-state
;;                       :items ,(projectile-project-files (projectile-project-root))))
;;                   :sort t))

;; (defun cur-consult-projectile-switch ()
;;   "Call `projectile-switch-project' and select from candidates.

;; The candidates are as follows:
;;   - Buffers
;;   - Files
;;   - Directories
;;   - Actions"
;;   (interactive)
;;   ;; (let* ((selection (consult--multi
;;   ;; 			     `(( :name "Projects"
;;   ;; 				 :narrow ?p
;;   ;; 				 :category file
;;   ;; 				 :items ,(projectile-relevant-known-projects)))))
;;   ;; 	 (root )))
;;   (let ((projectile-switch-project-action 'cur-consult-projectile-find-file))
;;     (projectile-switch-project))
;;   )


(provide 'cur-consult-projectile)
;;; cur-consult-projectile.el ends here
