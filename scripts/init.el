;;; init.el --- minimal djangonaut configuration

;;; Commentary:

;;; Code:

(setq tramp-verbose 2)

(require 'cask)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory)
  (setq gif-screencast-output-directory (f-join source-directory "pics")))

(require 'djangonaut)

(global-djangonaut-mode)

(setq python-shell-process-environment '("DJANGO_SETTINGS_MODULE=settings"))

(setq python-shell-extra-pythonpaths '("/code"))

(setq python-shell-interpreter "/docker:root@olympia_web_1:/usr/local/bin/python")

(load-theme 'zenburn t)

(add-to-list 'default-frame-alist '(font . "Liberation Mono-14"))

(menu-bar-mode -1)

(scroll-bar-mode -1)

(tool-bar-mode -1)

(blink-cursor-mode -1)

;;; init.el ends here
