;;; .emacs --- .emacs initialization -*- lexical-binding: t  -*-

;; Copyright (C) 2015-2018 Steve Buzonas

;; Author: Steve Buzonas <steve@fancyguy.com>
;; Keywords: dotemacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'cl-lib)

;; Process environment macros

(defmacro with-shell-env (shell &rest body)
  "Set `process-environment' to match that of a login shell speficied by SHELL.

The value returned is the value of the last form in BODY.
See also `with-default-shell-env'."
  `(let* ((command (concat ,shell " -cli \"env\""))
         (process-environment (split-string (shell-command-to-string command) "\n")))
    ,@body))

(defmacro with-default-shell-env (&rest body)
  "Set `process-environment' to match that of the $SHELL environment variable.

The value returned is the value of the last form in BODY.
See also `with-shell-env'."
  `(with-shell-env "$SHELL" ,@body))

;; XDG Configuration

(cl-flet*
 ((from-env (env &optional default) (or (getenv env) default nil))
  (path-split (path) (split-string path ":"))
  (append-app-dir (path) (concat path "/emacs"))
  (initialize-xdg-vars
   (lambda ()
     (defconst xdg-cache-home (from-env "XDG_CACHE_HOME" "~/.cache")
       "Defines the base directory relative to which user specific non-essential data files should be stored.

If $XDG_CACHE_HOME is either not set or empty, a default equal to $HOME/.cache is used.")
     (defconst cache-home (append-app-dir xdg-cache-home))
     
     (defconst xdg-config-home (from-env "XDG_CONFIG_HOME" "~/.config")
       "Defines the base directory relative to which user specific configuration files should be stored.

If $XDG_CONFIG_HOME is either not set or empty, a default equal to $HOME/.config is used.")
     (defconst config-home (append-app-dir xdg-config-home))

     (defconst xdg-config-dirs (path-split (from-env "XDG_CONFIG_DIRS" "/etc/xdg"))
       "Defines the preference-ordered set of base directories to search for configuration files in addition to the `xdg-config-home' base directory.

The directories in $XDG_CONFIG_DIRS should be seperated with a colon ':'.
If $XDG_CONFIG_DIRS is either not set or empty, a value equal to /etc/xdg is used.

The order of base directories denotes their importance; the first directory listed is the most important.
When the same information is defined in multiple places the information defined relative to the more important base directory takes precedent.

The base directory defined by `xdg-config-home' is considered more important than any of the base directories defined by `xdg-config-dirs'.")
     (defconst config-dirs (mapcar #'append-app-dir xdg-config-dirs))

     (defconst xdg-data-home (from-env "XDG_DATA_HOME" "~/.local/share")
       "Defines the base directory relative to which user specific data files should be stored.

If $XDG_DATA_HOME is either not set or empty, a default equal to $HOME/.local/share is used.")
     (defconst data-home (append-app-dir xdg-data-home))

     (defconst xdg-data-dirs (path-split (from-env "XDG_DATA_DIRS" "/usr/local/share:/usr/share"))
       "Defines the preference-ordered set of base directories to search for data files in addition to the `xdg-data-home' base directory.

The directories in $XDG_DATA_DIRS should be seperated with a colon ':'.
If $XDG_DATA_DIRS is either not set or empty, a value equal to /usr/local/share/:/usr/share/ is used.

The order of base directories denotes their importance; the first directory listed is the most important.
When the same information is defined in multiple places the information defined relative to the more important base directory takes precedent.

The base directory defined by `xdg-data-home' is considered more important than any of the base directories defined by `xdg-data-dirs'.")
     (defconst data-dirs (mapcar #'append-app-dir xdg-data-dirs))

     t)))
 (if (getenv "TERM_PROGRAM")
     (initialize-xdg-vars)
   (with-default-shell-env (initialize-xdg-vars))))

;; Bootstrap ENV for GUI emacs
(if (not (getenv "TERM_PROGRAM"))
    (let (shell-path)
      (with-default-shell-env (setq shell-path (getenv "PATH")))
      (setenv "PATH" shell-path)
      (setq exec-path (append (split-string shell-path ":") (list exec-directory)))))

;; Path functions

(defun add-package-dir (dir)
  "Add a directory at DIR to `load-path' including top level subdirs."
  (let ((default-directory dir))
    (setq load-path
          (append
           (let ((load-path (copy-sequence load-path))) ;; Shadow
             (append
              (copy-sequence (normal-top-level-add-to-load-path '(".")))
              (normal-top-level-add-subdirs-to-load-path)))
           load-path))))

(defun load-config (config &optional nosuffix)
  "Load a configuration file specified by CONFIG.
CONFIG should be a string.

This is an interface to the function `load'.  CONFIG is searched
for in `config-home'.

The optional NOSUFFIX argument can indicate to exclude suffixes from search."
  (interactive (list (completing-read "Load config: "
				      (apply-partially
				       'locate-file-completion-table
				       (cons config-home config-dirs)
				       (get-load-suffixes)))
		     nil))
  (let ((file (locate-file config
			   (cons config-home config-dirs)
			   (append (unless nosuffix (get-load-suffixes))
				   load-file-rep-suffixes))))
    (load file)))

;; XDG path variables

(defvar autosave-dir (expand-file-name "autosave/" data-home)
  "Location to store autosave files in.")
(make-directory autosave-dir t)

(defvar backup-dir (expand-file-name "backups" data-home)
  "Location to store backups in.")
(make-directory backup-dir t)

(defvar history-dir (expand-file-name "history" data-home)
  "Location to store hisory files in.")
(make-directory history-dir t)

(setq custom-file (expand-file-name "custom.el" data-home))
(setq package-user-dir (expand-file-name "packages" data-home))
(setq user-emacs-directory config-home)

;; Load path

(add-package-dir (concat config-home "/lisp"))
(add-package-dir (concat data-home "/lisp"))

;; Modular configuration

(setq load-prefer-newer t)

(load-config "packages")

(require 'slb-mode)

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-verbose t)
(setq use-package-always-ensure t)
(setq use-package-always-demand t)

(load custom-file :noerror)

(when (eq system-type 'darwin)
  (load-config "osx"))

(load-config "defaults")
(load-config "libraries")
(load-config "xterm")
(load-config "hydra")
(load-config "completion")
(load-config "helm")
(load-config "snippets")
(load-config "programming")
(load-config "docker")

(load-config "pandora")

;; Languages
;; (load-config "modules/lang/common-lisp")
(load-config "lang/emacs-lisp")
(load-config "lang/javascript")
(load-config "lang/typescript")
(load-config "lang/yaml")
(load-config "lang/latex")
(load-config "lang/go")
(load-config "lang/terraform")

;; (require 'nomad)

;; Local Variables:
;; mode: emacs-lisp
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; .emacs ends here
