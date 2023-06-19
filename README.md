<h1 align="center">Helm Project</h1>
<hr>
<p align="center">
This package provides a Helm interface for the built in project.el Emacs library.
</p>
<hr>

# Notable Features
* Inherits from built-in helm sources. e.g. act on project buffers using any of the helm buffer actions, open project files with root as you could when using helm file actions. Search within open project buffers using the `@` prefix, just as you could when using `helm-mini` or `helm-buffers-list`.

* Supports project.el external roots: If your project definition supports multiple roots, search and act across all of them at once.

# Installing
Install `helm-project` directly from [MELPA](https://melpa.org/#/helm-project)

# Configuration
``` emacs-lisp
(use-package helm-project
  :bind (("C-x C-p" . helm-project)
	 ("M-s p" . helm-project-grep-ag)
	 ([remap project-find-regexp] . helm-project-grep-ag)
	 ([remap project-switch-to-buffer] . helm-project-buffers)
	 ([remap project-find-file] . helm-project-files)
	 ([remap project-switch-project] . helm-project-list-projects)
	 :map helm-project-map
         ;; Any changes to keybinds on the helm-project-map should be made
         ;; before initializing the sources for the first time, i.e. before
         ;; calling any of the `helm-project-*' commands.
	 ("C-c s" . helm-project-grep-ag)))
```

# Commands
`helm-project-list-projects` - project-switch-project

`helm-project-files` - project-find-files

`helm-project-buffers` - project-switch-to-buffer

`helm-project-grep-ag` - project-find-regexp. May also be called from within a helm-project-* buffer with `M-g a`.

`helm-project` - This is the main entry point, and combines above three helm sources into a single interface.

`helm-project-toggle-external-flag` - Only bound when within a helm-project-* buffer, used to toggle search space between project root directory, and project root directory + external roots. Bound to `C-c a` by default.

# Examples
![helm-project-files](https://github.com/cmccloud/helm-project/blob/master/examples/helm-project.gif?raw=true)
Using `helm-project` to find a project file.

<hr>

![helm-project-grep-ag](https://github.com/cmccloud/helm-project/blob/master/examples/helm-project-grep-ag.gif?raw=true)
Using `helm-project-grep-ag` within `helm-project` to locate a definition within a project.

Bound by default to `M-g a` when within a helm buffer, as per helm conventions.

<hr>

![helm-project-file-actions](https://github.com/cmccloud/helm-project/blob/master/examples/helm-project-file-actions.gif?raw=true)
Showing inherited helm actions on project files.

<hr>

![helm-project-search-in-buffer](https://github.com/cmccloud/helm-project/blob/master/examples/helm-project-search-in-buffer.gif?raw=true)
Narrowing across open project buffers to those which contain "helm-project-source".

# Links
[Helm Homepage](https://github.com/emacs-helm/helm)

[Working with Projects - Emacs Manual](https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html)

[Project.el Emacs Mirror](https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/project.el)


