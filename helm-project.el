;; helm-project.el --- Helm source for project management. -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021 Christopher McCloud

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Helm frontend for project.el
;; 
;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'helm)
(require 'helm-source)
(require 'helm-types)
(require 'helm-buffers)
(require 'helm-for-files)
(require 'helm-utils)
(require 'project)

(defvar helm-project-source-files nil)

(defvar helm-project-source-buffers nil)

(defvar helm-project-source-projects nil)

(defvar helm-project-external-flag nil)

(defvar helm-project-files-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-find-files-map)
    (define-key map (kbd "M-g a") #'helm-project-do-grep-ag)
    (define-key map (kbd "C-c a") #'helm-project-toggle-external-flag)
    (define-key map (kbd "C-x C-f") #'helm-quit-and-find-file)
    map))

(defvar helm-project-buffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-buffer-map)
    (define-key map (kbd "M-g a") #'helm-project-do-grep-ag)
    (define-key map (kbd "C-c a") #'helm-project-toggle-external-flag)
    (define-key map (kbd "C-x C-f") #'helm-quit-and-find-file)
    map))

(defvar helm-project-projects-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-g a") #'helm-project-do-grep-ag)
    (define-key map (kbd "C-c a") #'helm-project-toggle-external-flag)
    (define-key map (kbd "C-x C-f") #'helm-quit-and-find-file)
    map))

(defun helm-project--project-buffer-list ()
  (let ((visible-bufs (helm-buffers-get-visible-buffers))
        (project-root-bufs (project-buffers (project-current)))
        (external-roots-bufs
         (when helm-project-external-flag
           (seq-mapcat (lambda (root)
			 (ignore-errors
			   (project-buffers
			    (project-current nil root))))
		       (project-external-roots (project-current))))))
    (cl-loop for buf in (append project-root-bufs external-roots-bufs) 
             ;; Divide project buffers into visible and not-visible groups
             when (memq (buffer-name buf) visible-bufs)
             collect (buffer-name buf) into visible-in-project
             else
             collect (buffer-name buf) into other-in-project
             ;; Sort buffers, same as `helm-buffers-list'
	     finally return (funcall helm-buffer-list-reorder-fn
                                     visible-in-project
                                     other-in-project))))

(defun helm-project-toggle-external-flag ()
  (interactive)
  (with-helm-alive-p
    (setq helm-project-external-flag (not helm-project-external-flag))
    (helm-force-update)
    (message (concat "Include External Roots: "
		     (if helm-project-external-flag "True" "False")))))

(defun helm-project-find-files ()
  (let* ((pc (project-current))
	 (ext (when helm-project-external-flag (project-external-roots pc))))
    (ignore-errors (project-files pc ext))))

(defun helm-project-grep-ag (arg)
  (interactive "P")
  (let* ((pc (project-current))
	 (ext (when helm-project-external-flag
		(project-external-roots pc)))
	 (default-directory (project-root pc)))
    (if ext (helm-do-grep-1 (append (list default-directory) ext) t 'grep)
      (helm-do-grep-ag arg))))

(defun helm-project-grep-ag-action (_c)
  (helm-project-grep-ag helm-current-prefix-arg))

(defun helm-project-do-grep-ag ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-project-grep-ag-action)))

(defun helm-project-switch-to-project (candidate)
  (let* ((dir (if (string-equal candidate "* Select New Project *")
		  (read-directory-name "Select Project Directory")
		candidate))
	 (pr (project--find-in-directory dir)))
    (if pr (project-remember-project pr)
      (project--remove-from-project-list dir))
    (with-helm-default-directory dir
      (helm-project))))

(defun helm-project-get-projects ()
  (append (with-temp-buffer
	    (insert-file-contents project-list-file)
	    (read (current-buffer)))
	  '(("* Select New Project *"))))

;; Helm Sources
(defclass helm-project-buffer-source (helm-source-buffers helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-project--project-buffer-list
    :custom function
    :documentation
    "A function with no arguments to create buffer list.")))

(cl-defmethod helm--setup-source ((source helm-project-buffer-source))
  (cl-call-next-method)
  (setf (slot-value source 'action)
        (append (symbol-value (slot-value source 'action))
                (helm-make-actions "Search project with Grep or AG `M-g a' " 
				   'helm-project-grep-ag-action
				   "Toggle external roots `C-c a'."
				   (lambda (_c)
				     (helm-project-toggle-external-flag)))))
  (setf (slot-value source 'keymap) helm-project-buffer-map))

(defclass helm-project-file-source (helm-source-sync helm-type-file)
  ((candidates
    :initform 'helm-project-find-files)
   (match-part :initform (lambda (candidate)
			   (if helm-ff-transformer-show-only-basename
			       (helm-basename candidate) candidate)))))

(cl-defmethod helm--setup-source ((source helm-project-file-source))
  (cl-call-next-method)
  (setf (slot-value source 'action)
        (append (symbol-value (slot-value source 'action))
                (helm-make-actions "Search project with Grep or AG `M-g a' "
				   'helm-project-grep-ag-action
				   "Toggle external roots. `C-c a'"
				   (lambda (_c)
				     (helm-project-toggle-external-flag)))))
  (setf (slot-value source 'keymap) helm-project-files-map))

(defclass helm-project-project-source (helm-source-sync)
  ((candidates
    :initform 'helm-project-get-projects)
   (action
    :initform (helm-make-actions
	       "Switch to Project"
	       'helm-project-switch-to-project
	       "Search project with Grep or AG `M-g a' "
	       'helm-project-grep-ag-action))
   (keymap :initform helm-project-projects-map)))

;;; User Facing Commands

;;;###autoload
(defun helm-project-list-projects ()
  (interactive)
  (unless (bound-and-true-p helm-project-source-projects)
    (setq helm-project-source-projects
	  (helm-make-source "Projects"
	      helm-project-project-source)))
  (helm
   :buffer "*helm projects*"
   :sources '(helm-project-source-projects)))

;;;###autoload
(defun helm-project-files ()
  (interactive)
  (unless (bound-and-true-p helm-project-source-files)
    (setq helm-project-source-files
	  (helm-make-source "Project Files"
	      helm-project-file-source)))
  (helm
   :buffer "*helm project files*"
   :sources '(helm-project-source-files)))

;;;###autoload
(defun helm-project-buffers ()
  (interactive)
  (unless (bound-and-true-p helm-project-source-buffers)
    (setq helm-project-source-buffers
	  (helm-make-source "Project Buffers"
	      helm-project-buffer-source)))
  (helm
   :buffer "*helm project buffers*"
   :sources '(helm-project-source-buffers)
   :truncate-lines t))

;;;###autoload
(defun helm-project ()
  (interactive)
  (unless (bound-and-true-p helm-project-source-files)
    (setq helm-project-source-files
	  (helm-make-source "Project Files"
	      helm-project-file-source)))
  (unless (bound-and-true-p helm-project-source-buffers)
    (setq helm-project-source-buffers
	  (helm-make-source "Project Buffers"
	      helm-project-buffer-source)))
  (unless (bound-and-true-p helm-project-source-projects)
    (setq helm-project-source-projects
	  (helm-make-source "Projects"
	      helm-project-project-source)))
  (helm
   :buffer "*Helm Project*"
   :sources '(helm-project-source-buffers
	      helm-project-source-files
	      helm-project-source-projects)
   :truncate-lines t
   :ff-transformer-show-only-basename nil
   :resume nil
   :candidate-number-limit 100))

(provide 'helm-project)

;; helm-project.el ends here


