;;; djangonaut.el --- Emacs minor mode for Django  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/proofit404/djangonaut
;; Version: 0.0.1
;; Package-Requires: ((emacs "24") (pythonic "0.1.0") (hydra "0.14.0") (dash "2.6.0") (s "1.9"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the README for more details.

;;; Code:

(require 'pythonic)
(require 'hydra)
(require 'json)
(require 'dash)
(require 's)

(defvar djangonaut-get-commands-code "
from __future__ import print_function
from django.apps import apps
from django.conf import settings
from django.core.management import get_commands
apps.populate(settings.INSTALLED_APPS)
print('\\n'.join(get_commands().keys()))
")

(defvar djangonaut-get-app-paths-code "
from __future__ import print_function
from json import dumps
from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)
paths = {app.label: app.path for app in apps.get_app_configs()}
print(dumps(paths), end='')
")

(defvar djangonaut-get-models-code "
from __future__ import print_function
from inspect import findsource, getfile
from json import dumps
from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)
models = {model.__name__: [getfile(model), findsource(model)[1]] for model in apps.get_models()}
print(dumps(models), end='')
")

(defvar-local djangonaut-directory nil)

(defun djangonaut-get-commands ()
  (split-string
   (with-output-to-string
     (with-current-buffer
         standard-output
       (call-pythonic :buffer standard-output
                      :args (list "-c" djangonaut-get-commands-code)
                      :cwd djangonaut-directory)))
   nil t))

(defun djangonaut-get-app-paths ()
  (json-read-from-string
   (with-output-to-string
     (with-current-buffer
         standard-output
       (call-pythonic :buffer standard-output
                      :args (list "-c" djangonaut-get-app-paths-code)
                      :cwd djangonaut-directory)))))

(defun djangonaut-get-models ()
  (json-read-from-string
   (with-output-to-string
     (with-current-buffer
         standard-output
       (call-pythonic :buffer standard-output
                      :args (list "-c" djangonaut-get-models-code)
                      :cwd djangonaut-directory)))))

(defun djangonaut-find-model ()
  (interactive)
  (let* ((models (djangonaut-get-models))
         (model (intern (completing-read "Model: " (mapcar 'symbol-name (mapcar 'car (djangonaut-get-models))) nil t)))
         (code (cdr (assoc model models)))
         (filename (elt code 0))
         (lineno (elt code 1)))
    (when (pythonic-remote-p)
      (setq filename (concat (pythonic-tramp-connection) filename)))
    (find-file filename)
    (goto-char (point-min))
    (forward-line lineno)))

(defun djangonaut-command (&rest command)
  (interactive (split-string (completing-read "Command: " (djangonaut-get-commands) nil nil) " " t))
  (start-pythonic :process "djangonaut"
                  :buffer "*Django*"
                  :args (append (list "manage.py") command)
                  :cwd djangonaut-directory)
  (pop-to-buffer "*Django*"))

(defun djangonaut-makemigrations (app-name)
  (interactive "sApplication: ")
  (djangonaut-command "makemigrations" app-name))

(defun djangonaut-flush ()
  (interactive)
  (djangonaut-command "flush" "--noinput"))

(defun djangonaut-migrate ()
  (interactive)
  (djangonaut-command "migrate"))

(defun djangonaut-assets-rebuild ()
  (interactive)
  (djangonaut-command "assets" "rebuild"))

(defun djangonaut-startapp (name)
  (interactive "sName:")
  (djangonaut-command "startapp" name))

(defun djangonaut-makemessages ()
  (interactive)
  (djangonaut-command "makemessages" "--all" "--symlinks"))

(defun djangonaut-compilemessages ()
  (interactive)
  (djangonaut-command "compilemessages"))

(defun djangonaut-test ()
  (interactive)
  (djangonaut-command "test"))

(defhydra djangonaut-hydra (:color blue :hint nil)
  "
                              Manage.py
----------------------------------------------------------------------------

_mm_: Enter manage.py command   _t_: Run rest           _f_: Flush
_ma_: Makemigrations            _sa_: Start new app     _a_: Rebuild Assets
_mg_: Migrate                   _c_: Compile messages
_me_: Make messages

_q_: Cancel

"
  ("mm" djangonaut-command)
  ("ma" djangonaut-makemigrations)
  ("mg" djangonaut-migrate)
  ("me" djangonaut-makemessages)
  ("sa" djangonaut-startapp)
  ("f"  djangonaut-flush)
  ("a"  djangonaut-assets-rebuild)
  ("c"  djangonaut-compilemessages)
  ("t"  djangonaut-test)
  ("q"  nil "cancel"))

(defvar djangonaut-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c r r") 'djangonaut-hydra/body)
    (define-key map (kbd "C-c r m") 'djangonaut-find-model)
    map))

(defvar djangonaut-mode-lighter " Django")

;;;###autoload
(define-minor-mode djangonaut-mode
  ""
  :lighter djangonaut-mode-lighter
  :keymap djangonaut-mode-map)

;;;###autoload
(define-globalized-minor-mode global-djangonaut-mode djangonaut-mode
  (lambda ()
    (-when-let (django-project-root
                (and (stringp buffer-file-name)
                     (locate-dominating-file default-directory "manage.py")))
      (setq djangonaut-directory (pythonic-file-name django-project-root))
      (djangonaut-mode 1))))

(provide 'djangonaut)

;;; djangonaut.el ends here
