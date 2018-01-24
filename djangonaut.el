;;; djangonaut.el --- Emacs minor mode for Django  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/proofit404/djangonaut
;; Version: 0.0.1
;; Package-Requires: ((emacs "24") (pythonic "0.1.0") (f "0.20.0"))

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
(require 'json)
(require 'f)

(defvar djangonaut-get-pythonpath-code "
from __future__ import print_function

from sys import path

print('\\n'.join(path))
")

(defvar djangonaut-get-project-root-code "
from __future__ import print_function

from importlib import import_module
from os import environ
from os.path import dirname

settings_module = environ['DJANGO_SETTINGS_MODULE']
package_name = settings_module.split('.', 1)[0]
package = import_module(package_name)
project_root = dirname(dirname(package.__file__))

print(project_root, end='')
")

(defvar djangonaut-get-commands-code "
from __future__ import print_function

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from django.core.management import get_commands

print('\\n'.join(get_commands().keys()))
")

(defvar djangonaut-get-app-paths-code "
from __future__ import print_function

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from json import dumps

paths = {app.label: app.path for app in apps.get_app_configs()}

print(dumps(paths), end='')
")

(defvar djangonaut-get-admin-classes-code "
from __future__ import print_function

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from inspect import findsource, getfile
from json import dumps

from django.contrib.admin.sites import all_sites

admin_classes = {}
for site in all_sites:
    for admin_instance in site._registry.values():
        admin_class = admin_instance.__class__
        admin_classes[admin_class.__name__] = [getfile(admin_class), findsource(admin_class)[1]]

print(dumps(admin_classes), end='')
")

(defvar djangonaut-get-models-code "
from __future__ import print_function

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from inspect import findsource, getfile
from json import dumps

models = {model._meta.app_label + '.' + model.__name__: [getfile(model), findsource(model)[1]] for model in apps.get_models()}

print(dumps(models), end='')
")

(defvar djangonaut-get-signal-receivers-code "
from __future__ import print_function

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from gc import get_objects
from inspect import findsource, getfile, getmodule
from json import dumps
from weakref import ReferenceType

from django.dispatch.dispatcher import Signal

receivers = {}
for obj in get_objects():
    if isinstance(obj, Signal):
        for lookup_key, receiver in obj.receivers:
            if isinstance(receiver, ReferenceType):
                receiver = receiver()
                if receiver is None:
                    continue
            name = getmodule(receiver).__name__ + '.' + receiver.__name__
            receivers[name] = [getfile(receiver), findsource(receiver)[1]]

print(dumps(receivers), end='')
")

(defvar djangonaut-get-drf-serializers-code "
from __future__ import print_function

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from gc import get_objects
from importlib import import_module
from inspect import findsource, getfile, isclass
from json import dumps

from rest_framework.serializers import Serializer

import_module(settings.ROOT_URLCONF)

serializers = {}
for obj in get_objects():
    if isclass(obj) and issubclass(obj, Serializer):
        serializers[obj.__name__] = [getfile(obj), findsource(obj)[1]]

print(dumps(serializers), end='')
")

(defvar djangonaut-get-views-code "
from __future__ import print_function

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from inspect import findsource, getfile, getmodule, ismethod, unwrap
from json import dumps

from django.urls import get_resolver, get_urlconf, RegexURLPattern, RegexURLResolver

views = {}

def collect_views(resolver):
    for pattern in resolver.url_patterns:
        if isinstance(pattern, RegexURLResolver):
            collect_views(pattern)
        elif isinstance(pattern, RegexURLPattern):
            view = pattern.callback
            if hasattr(view, 'view_class'):
                # Django as_view result.
                view = view.view_class
                name = getmodule(view).__name__ + '.' + view.__name__
            elif hasattr(view, 'cls'):
                # DRF as_view result.
                view = view.cls
                name = getmodule(view).__name__ + '.' + view.__name__
            else:
                view = unwrap(view)
                if ismethod(view):
                    name = getmodule(view).__name__ + '.' + view.__self__.__class__.__name__ + '.' + view.__name__
                else:
                    name = getmodule(view).__name__ + '.' + view.__name__
            views[name] = [getfile(view), findsource(view)[1]]

collect_views(get_resolver(get_urlconf()))

print(dumps(views), end='')
")

(defvar djangonaut-get-templates-code "
from __future__ import print_function

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from json import dumps
from os import walk
from os.path import join

from django.template import engines
from django.template.backends.django import DjangoTemplates
from django.template.loaders.filesystem import Loader as FileSystemLoader
from django.template.loaders.app_directories import Loader as AppDirectoriesLoader

templates = {}

for engine in engines.all():
    if isinstance(engine, DjangoTemplates):
        for loader in engine.engine.template_loaders:
            if isinstance(loader, (FileSystemLoader, AppDirectoriesLoader)):
                for template_directory in loader.get_dirs():
                    for root, _, files in walk(template_directory):
                        for template in files:
                            template_path = join(root, template)
                            templates.setdefault(template_path[len(template_directory) + 1:], template_path)

print(dumps(templates), end='')
")

(defvar djangonaut-get-template-tags-code "
from __future__ import print_function

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from importlib import import_module
from inspect import findsource, getfile, unwrap
from json import dumps

from django.template.backends.django import get_installed_libraries

libraries = get_installed_libraries()
libraries['builtin'] = 'django.template.defaulttags'

template_tags = {}
for library_name, library_path in libraries.items():
    library = import_module(library_path).register
    for tag_name, tag in library.tags.items():
        tag = unwrap(tag)
        template_tags[library_name + '.' + tag_name] = [getfile(tag), findsource(tag)[1]]

print(dumps(template_tags), end='')
")

(defvar djangonaut-get-template-filters-code "
from __future__ import print_function

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from importlib import import_module
from inspect import findsource, getfile, unwrap
from json import dumps

from django.template.backends.django import get_installed_libraries

libraries = get_installed_libraries()
libraries['builtin'] = 'django.template.defaultfilters'

template_filters = {}
for library_name, library_path in libraries.items():
    library = import_module(library_path).register
    for filter_name, filter in library.filters.items():
        filter = unwrap(filter)
        template_filters[library_name + '.' + filter_name] = [getfile(filter), findsource(filter)[1]]

print(dumps(template_filters), end='')
")

(defvar djangonaut-get-static-files-code "
from __future__ import print_function

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from json import dumps

from django.contrib.staticfiles.finders import get_finders

ignore_patterns = list(set(apps.get_app_config('staticfiles').ignore_patterns))

staticfiles = {}
for finder in get_finders():
    for path, storage in finder.list(ignore_patterns):
        staticfiles.setdefault(path, storage.path(path))

print(dumps(staticfiles), end='')
")

(defvar djangonaut-get-settings-path-code "
from __future__ import print_function

from importlib import import_module
from os import environ

settings_module = environ['DJANGO_SETTINGS_MODULE']
module = import_module(settings_module)
settings_path = module.__file__

print(settings_path, end='')
")

(defun djangonaut-get-pythonpath ()
  (split-string
   (with-output-to-string
     (with-current-buffer
         standard-output
       (call-pythonic :buffer standard-output
                      :args (list "-c" djangonaut-get-pythonpath-code))))
   nil t))

(defun djangonaut-get-project-root ()
  (with-output-to-string
    (with-current-buffer
        standard-output
      (call-pythonic :buffer standard-output
                     :args (list "-c" djangonaut-get-project-root-code)))))

(defun djangonaut-call (code)
  (let (exit-code output)
    (setq output
          (with-output-to-string
            (with-current-buffer
                standard-output
              (setq exit-code
                    (call-pythonic :buffer standard-output :args (list "-c" code))))))
    (when (not (zerop exit-code))
      (with-current-buffer (get-buffer-create "*Django*")
        (erase-buffer)
        (fundamental-mode)
        (insert output)
        (goto-char (point-min))
        (pop-to-buffer "*Django*")
        (error "Python exit with status code %d" exit-code)))
    output))

(defun djangonaut-get-commands ()
  (split-string (djangonaut-call djangonaut-get-commands-code) nil t))

(defun djangonaut-get-app-paths ()
  (json-read-from-string (djangonaut-call djangonaut-get-app-paths-code)))

(defun djangonaut-get-admin-classes ()
  (json-read-from-string (djangonaut-call djangonaut-get-admin-classes-code)))

(defun djangonaut-get-models ()
  (json-read-from-string (djangonaut-call djangonaut-get-models-code)))

(defun djangonaut-get-signal-receivers ()
  (json-read-from-string (djangonaut-call djangonaut-get-signal-receivers-code)))

(defun djangonaut-get-drf-serializers ()
  (json-read-from-string (djangonaut-call djangonaut-get-drf-serializers-code)))

(defun djangonaut-get-views ()
  (json-read-from-string (djangonaut-call djangonaut-get-views-code)))

(defun djangonaut-get-templates ()
  (json-read-from-string (djangonaut-call djangonaut-get-templates-code)))

(defun djangonaut-get-template-tags ()
  (json-read-from-string (djangonaut-call djangonaut-get-template-tags-code)))

(defun djangonaut-get-template-filters ()
  (json-read-from-string (djangonaut-call djangonaut-get-template-filters-code)))

(defun djangonaut-get-static-files ()
  (json-read-from-string (djangonaut-call djangonaut-get-static-files-code)))

(defun djangonaut-get-settings-path ()
  (djangonaut-call djangonaut-get-settings-path-code))

(defun djangonaut-run-management-command (&rest command)
  (interactive (split-string (completing-read "Command: " (djangonaut-get-commands) nil nil) " " t))
  (with-current-buffer (get-buffer-create "*Django*")
    (start-pythonic :process "djangonaut"
                    :buffer "*Django*"
                    :args (append (list "-m" "django") command)
                    :cwd (djangonaut-get-project-root))
    (erase-buffer)
    (comint-mode)
    (pop-to-buffer "*Django*")))

(defun djangonaut-dired-installed-apps ()
  (interactive)
  (let* ((apps (djangonaut-get-app-paths))
         (app (intern (completing-read "App: " (mapcar 'symbol-name (mapcar 'car apps)) nil t)))
         (directory (cdr (assoc app apps))))
    (when (pythonic-remote-p)
      (setq directory (concat (pythonic-tramp-connection) directory)))
    (dired directory)))

(defun djangonaut-find-admin-class ()
  (interactive)
  (let* ((admin-classes (djangonaut-get-admin-classes))
         (admin-class (intern (completing-read "Admin-Class: " (mapcar 'symbol-name (mapcar 'car admin-classes)) nil t)))
         (code (cdr (assoc admin-class admin-classes)))
         (filename (elt code 0))
         (lineno (elt code 1)))
    (when (pythonic-remote-p)
      (setq filename (concat (pythonic-tramp-connection) filename)))
    (find-file filename)
    (goto-char (point-min))
    (forward-line lineno)))

(defun djangonaut-find-model ()
  (interactive)
  (let* ((models (djangonaut-get-models))
         (model (intern (completing-read "Model: " (mapcar 'symbol-name (mapcar 'car models)) nil t)))
         (code (cdr (assoc model models)))
         (filename (elt code 0))
         (lineno (elt code 1)))
    (when (pythonic-remote-p)
      (setq filename (concat (pythonic-tramp-connection) filename)))
    (find-file filename)
    (goto-char (point-min))
    (forward-line lineno)))

(defun djangonaut-find-signal-receiver ()
  (interactive)
  (let* ((receivers (djangonaut-get-signal-receivers))
         (receiver (intern (completing-read "Receiver: " (mapcar 'symbol-name (mapcar 'car receivers)) nil t)))
         (code (cdr (assoc receiver receivers)))
         (filename (elt code 0))
         (lineno (elt code 1)))
    (when (pythonic-remote-p)
      (setq filename (concat (pythonic-tramp-connection) filename)))
    (find-file filename)
    (goto-char (point-min))
    (forward-line lineno)))

(defun djangonaut-find-drf-serializer ()
  (interactive)
  (let* ((serializers (djangonaut-get-drf-serializers))
         (serializer (intern (completing-read "Serializer: " (mapcar 'symbol-name (mapcar 'car serializers)) nil t)))
         (code (cdr (assoc serializer serializers)))
         (filename (elt code 0))
         (lineno (elt code 1)))
    (when (pythonic-remote-p)
      (setq filename (concat (pythonic-tramp-connection) filename)))
    (find-file filename)
    (goto-char (point-min))
    (forward-line lineno)))

(defun djangonaut-find-view ()
  (interactive)
  (let* ((views (djangonaut-get-views))
         (view (intern (completing-read "View: " (mapcar 'symbol-name (mapcar 'car views)) nil t)))
         (code (cdr (assoc view views)))
         (filename (elt code 0))
         (lineno (elt code 1)))
    (when (pythonic-remote-p)
      (setq filename (concat (pythonic-tramp-connection) filename)))
    (find-file filename)
    (goto-char (point-min))
    (forward-line lineno)))

(defun djangonaut-find-template ()
  (interactive)
  (let* ((templates (djangonaut-get-templates))
         (template (intern (completing-read "Template: " (mapcar 'symbol-name (mapcar 'car templates)) nil t)))
         (filename (cdr (assoc template templates))))
    (when (pythonic-remote-p)
      (setq filename (concat (pythonic-tramp-connection) filename)))
    (find-file filename)))

(defun djangonaut-find-template-tag ()
  (interactive)
  (let* ((tags (djangonaut-get-template-tags))
         (tag (intern (completing-read "Template Tag: " (mapcar 'symbol-name (mapcar 'car tags)) nil t)))
         (code (cdr (assoc tag tags)))
         (filename (elt code 0))
         (lineno (elt code 1)))
    (when (pythonic-remote-p)
      (setq filename (concat (pythonic-tramp-connection) filename)))
    (find-file filename)
    (goto-char (point-min))
    (forward-line lineno)))

(defun djangonaut-find-template-filter ()
  (interactive)
  (let* ((filters (djangonaut-get-template-filters))
         (filter (intern (completing-read "Template Filter: " (mapcar 'symbol-name (mapcar 'car filters)) nil t)))
         (code (cdr (assoc filter filters)))
         (filename (elt code 0))
         (lineno (elt code 1)))
    (when (pythonic-remote-p)
      (setq filename (concat (pythonic-tramp-connection) filename)))
    (find-file filename)
    (goto-char (point-min))
    (forward-line lineno)))

(defun djangonaut-find-static-file ()
  (interactive)
  (let* ((static-files (djangonaut-get-static-files))
         (static-file (intern (completing-read "Static: " (mapcar 'symbol-name (mapcar 'car static-files)) nil t)))
         (filename (cdr (assoc static-file static-files))))
    (when (pythonic-remote-p)
      (setq filename (concat (pythonic-tramp-connection) filename)))
    (find-file filename)))

(defun djangonaut-find-settings-module ()
  (interactive)
  (let ((filename (djangonaut-get-settings-path)))
    (when (pythonic-remote-p)
      (setq filename (concat (pythonic-tramp-connection) filename)))
    (find-file filename)))

(defvar djangonaut-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c r !") 'djangonaut-run-management-command)
    (define-key map (kbd "C-c r i") 'djangonaut-dired-installed-apps)
    (define-key map (kbd "C-c r a") 'djangonaut-find-admin-class)
    (define-key map (kbd "C-c r m") 'djangonaut-find-model)
    (define-key map (kbd "C-c r r") 'djangonaut-find-signal-receiver)
    (define-key map (kbd "C-c r s") 'djangonaut-find-drf-serializer)
    (define-key map (kbd "C-c r v") 'djangonaut-find-view)
    (define-key map (kbd "C-c r t") 'djangonaut-find-template)
    (define-key map (kbd "C-c r g") 'djangonaut-find-template-tag)
    (define-key map (kbd "C-c r f") 'djangonaut-find-template-filter)
    (define-key map (kbd "C-c r j") 'djangonaut-find-static-file)
    (define-key map (kbd "C-c r S") 'djangonaut-find-settings-module)
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
    (ignore-errors
      (when (djangonaut-get-project-root)
        (let ((directory (pythonic-file-name default-directory)))
          (dolist (path (djangonaut-get-pythonpath))
            (when (or (f-same? path directory)
                      (f-ancestor-of? path directory))
              (djangonaut-mode 1))))))))

(provide 'djangonaut)

;;; djangonaut.el ends here
