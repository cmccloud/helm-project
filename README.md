<h1 align="center">Helm Project</h1>
<hr>
<p align="center">
This package provides a Helm interface for the built in project.el Emacs library.
</p>
<hr>

## Links
[Helm Homepage](https://github.com/emacs-helm/helm)

[Working with Projects - Emacs Manual](https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html)

[Project.el Emacs Mirror](https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/project.el)

## Examples
![helm-project-files](https://github.com/cmccloud/helm-project/blob/master/examples/helm-project.gif?raw=true)
Using `helm-project` to find a project file.

![helm-project-grep-ag](https://github.com/cmccloud/helm-project/blob/master/examples/helm-project-grep-ag.gif?raw=true)
Using helm-project-grep-ag within `helm-project` to locate a definition within a project (bound by default to `M-g a` as per helm conventions).


## Commands
`helm-project-list-projects` - project-switch-project

`helm-project-files` - project-find-files

`helm-project-buffers` - project-switch-to-buffer

`helm-project-grep-ag` - project-find-regexp

`helm-project` - This is the main entry point, and combines above three helm sources into a single interface.

## Installing
Clone this repository and install using `use-package`

Set up keybindings as desired.

``` emacs-lisp
(use-package helm-project
  :load-path "/path/to/helm-project-repository"
  :bind (("C-x C-p" . helm-project)
	 ("M-s p" . helm-project-grep-ag)
	 ([remap project-find-regexp] . helm-project-grep-ag)
	 ([remap project-switch-to-buffer] . helm-project-buffers)
	 ([remap project-find-files] . helm-project-files)
	 ([remap project-switch-project] . helm-project-list-projects)))

```

## 
