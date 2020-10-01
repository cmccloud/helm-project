;; helm-project.el --- Helm source for project management. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Christopher McCloud

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

;;; Code:
(require 'cl-lib)
(require 'helm)
(require 'helm-source)
(require 'helm-types)
(require 'helm-buffers)
(require 'helm-for-files)

(defvar helm-project-source-project-files nil)

(defvar helm-project-source-project-buffers nil)

(defvar helm-project-source-projects nil)

(defun helm-project--project-buffer-list ()
  (seq-filter
   (lambda (name)
     (not (seq-contains-p helm-boring-buffer-regexp-list
			  name
			  #'string-match-p)))
   (mapcar #'buffer-name (project--buffer-list (project-current)))))

(defvar helm-project-files-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-find-files-map)
    (define-key map (kbd "M-g a") #'helm-project-do-grep-ag)
    map))

(defvar helm-project-buffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-buffer-map)
    (define-key map (kbd "M-g a") #'helm-project-do-grep-ag)
    map))

(defvar helm-project-projects-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-g a") #'helm-project-do-grep-ag)
    map))

(defun helm-project-grep-ag (arg)
  (interactive "P")
  (with-helm-default-directory (cdr (project-current))
    (helm-do-grep-ag arg)))

(defun helm-project-grep-ag-action (_c)
  (helm-project-grep-ag nil))

(defun helm-project-do-grep-ag ()
  (interactive)
  (helm-exit-and-execute-action 'helm-project-grep-ag-action))

(defclass helm-project-project-buffer-source (helm-source-buffers helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-project--project-buffer-list
    :custom function
    :documentation
    "A function with no arguments to create buffer list.")))

(defclass helm-project-project-files-source (helm-source-sync helm-type-file)
  ((candidates
    :initform (lambda () (project-files (project-current))))
   (volatile :initform t)))

(defclass helm-project-project-source (helm-source-sync)
  ((candidates
    :initform (lambda ()
		(with-temp-buffer
		  (insert-file-contents project-list-file)
		  (read (current-buffer)))))
   (action
    :initform (helm-make-actions
	       "Switch to Project"
	       (lambda (candidate)
		 (with-helm-default-directory candidate
		   (helm-project)))
	       "Grep project with AG `M-g a, C-u select type' "
	       'helm-project-grep-ag-action))
   (volatile :initform t)
   (keymap :initform helm-project-projects-map)))

(cl-defmethod helm--setup-source ((source helm-project-project-files-source))
  (cl-call-next-method)
  (setf (slot-value source 'action)
        (append (symbol-value (slot-value source 'action))
                (helm-make-actions "Grep project with AG `M-g a, C-u select type' "
				   'helm-project-grep-ag-action)))
  (setf (slot-value source 'keymap) helm-project-files-map))

(cl-defmethod helm--setup-source ((source helm-project-project-buffer-source))
  (cl-call-next-method)
  (setf (slot-value source 'action)
        (append (symbol-value (slot-value source 'action))
                (helm-make-actions "Grep project with AG `M-g a, C-u select type' " 
				   'helm-project-grep-ag-action)))
  (setf (slot-value source 'keymap) helm-project-buffer-map))

(defun helm-project-list-projects ()
  (interactive)
  (unless (bound-and-true-p helm-project-source-projects)
    (setq helm-project-source-projects
	  (helm-make-source "Projects"
	      helm-project-project-source)))
  (helm
   :buffer "*helm projects*"
   :sources '(helm-project-source-projects)))

(defun helm-project-project-files ()
  (interactive)
  (unless (bound-and-true-p helm-project-source-project-files)
    (setq helm-project-source-project-files
	  (helm-make-source "Project Files"
	      helm-project-project-files-source)))
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm
     :buffer "*helm project files*"
     :sources '(helm-project-source-project-files))))

(defun helm-project-project-buffers ()
  (interactive)
  (unless (bound-and-true-p helm-project-source-project-buffers)
    (setq helm-project-source-project-buffers
	  (helm-make-source "Project Buffers"
	      helm-project-project-buffer-source)))
  (helm
   :buffer "*helm project buffers*"
   :sources '(helm-project-source-project-buffers)
   :truncate-lines t))

(defun helm-project ()
  (interactive)
  (unless (bound-and-true-p helm-project-source-project-files)
    (setq helm-project-source-project-files
	  (helm-make-source "Project Files"
	      helm-project-project-files-source)))
  (unless (bound-and-true-p helm-project-source-project-buffers)
    (setq helm-project-source-project-buffers
	  (helm-make-source "Project Buffers"
	      helm-project-project-buffer-source)))
  (unless (bound-and-true-p helm-project-source-projects)
    (setq helm-project-source-projects
	  (helm-make-source "Projects"
	      helm-project-project-source)))
  (helm
   :buffer "*helm projects*"
   :sources '(helm-project-source-project-buffers
	      helm-project-source-project-files
	      helm-project-source-projects)
   :truncate-lines t
   :ff-transformer-show-only-basename nil))

(provide 'helm-project)

;; TODO: Add Project.el Support to Treemacs
;; helm-project.el ends here


