;;; helm-project.el --- Helm source for project.el projects  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023 Christopher McCloud

;; Author: Christopher McCloud
;; URL: https://github.com/cmccloud/helm-project
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (helm "3.9.0"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Helm frontend for project management using the project.el library.

;;;; Installation

;;;;; MELPA

;; If you installed from MELPA, you're done.

;;;;; Manual

;; Install these required packages:

;; helm

;; Then put this file in your load-path, and put this in your init
;; file:

;; (require 'helm-project)

;; Or using `use-package':

;; (use-package helm-project
;;   :after helm
;;   :load-path "path/to/helm-project")

;; Optionally bind some of the commands
;; 

;;;; Usage

;; Run one of these commands:

;; Use `helm-project-grep-ag' where before you used `project-find-regexp'
;; Use `helm-project-buffers' where before you used `project-switch-to-buffer'
;; Use `helm-project-buffers' where before you used `project-switch-to-buffer'
;; Use `helm-project-files' where before you used `project-find-files'
;; Use `helm-project-list-projects' where before you used `project-switch-project'

;; Or better yet, use the main entry point, `helm-project' which combines all of
;; these helm sources.

;; From within any of the `helm-project-*' sources, use <C-c a> to call
;; `helm-project-toggle-external-flag' and switch between searching across
;; the current project, or the current project and it's external roots, if any


;;;; Tips

;; + You can customize settings in the helm-project group.

;;;; Credits

;; This package would not have been possible without the following
;; packages: foo[1], which showed me how to bifurcate, and bar[2],
;; which takes care of flanges.
;;
;;  [1] https://example.com/foo.el
;;  [2] https://example.com/bar.el

;;; Code:

;;;; Requirements

(require 'foo)
(require 'bar)

;;;; Customization

(defgroup helm-project nil
  "Settings for `helm-project'."
  :link '(url-link "https://example.com/helm-project.el"))

(defcustom helm-project-something nil
  "This setting does something."
  :type 'something)

;;;; Variables

(defvar helm-project-var nil
  "A variable.")

;;;;; Keymaps

;; This technique makes it easier and less verbose to define keymaps
;; that have many bindings.

(defvar helm-project-map
  ;; This makes it easy and much less verbose to define keys
  (let ((map (make-sparse-keymap "helm-project map"))
        (maps (list
               ;; Mappings go here, e.g.:
               "RET" #'helm-project-RET-command
               [remap search-forward] #'helm-project-search-forward
               )))
    (cl-loop for (key fn) on maps by #'cddr
             do (progn
                  (when (stringp key)
                    (setq key (kbd key)))
                  (define-key map key fn)))
    map))

;;;; Commands

;;;###autoload
(defun helm-project-command (args)
  "Frobnicate the flange."
  (interactive)
  (helm-project-foo
   (helm-project--bar args)))

;;;; Functions

;;;;; Public

(defun helm-project-foo (args)
  "Return foo for ARGS."
  (foo args))

;;;;; Private

(defun helm-project--bar (args)
  "Return bar for ARGS."
  (bar args))

;;;; Footer

(provide 'helm-project)

;;; helm-project.el ends here
