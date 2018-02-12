;;; djangonaut.el --- Emacs minor mode for Django  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/proofit404/djangonaut
;; Version: 0.0.1
;; Package-Requires: ((emacs "24") (pythonic "0.1.0") (f "0.20.0") (magit-popup "2.6.0"))

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

(require 'magit-popup)
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

(defvar djangonaut-get-command-definitions-code "
from __future__ import print_function

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from importlib import import_module
from inspect import findsource, getfile
from json import dumps

from django.core.management import get_commands

commands = {}
for command_name, module_name in get_commands().items():
    module = import_module(module_name + '.management.commands.' + command_name)
    command = module.Command
    commands[command_name] = [getfile(command), findsource(command)[1]]

print(dumps(commands), end='')
")

(defvar djangonaut-get-command-arguments-code "
from __future__ import print_function

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from importlib import import_module
from json import dumps
from random import choice
from string import ascii_letters
from sys import argv

from django.core.management import get_commands

arguments = {}
known_shortcuts = set([])

def get_free_shortcut(short):
    result = short
    while result in known_shortcuts:
        result = choice(ascii_letters)
    known_shortcuts.add(result)
    return result

class Parser(object):

    @staticmethod
    def add_argument(*args, **kwargs):

        if len(args) > 2 or len(args) < 1 or not all(map(lambda x: x.startswith('-'), args)):
            raise Exception('Unsupported arguments: {0} {1}'.format(args, kwargs))
        elif len(args) > 1:
            if args[0].startswith('--'):
                name = args[0]
                shortcut = get_free_shortcut(args[1][1])
            else:
                name = args[1]
                shortcut = get_free_shortcut(args[0][1])
        else:
            if args[0].startswith('--'):
                name = args[0]
                shortcut = get_free_shortcut(args[0][2])
            else:
                name = args[0]
                shortcut = get_free_shortcut(args[0][1])

        if kwargs.get('action') == 'store_true':
            target = arguments.setdefault('switches', [])
        else:
            target = arguments.setdefault('options', [])
        target.append([shortcut, kwargs['help'], name])

command_name = argv[-1]
module_name = get_commands()[command_name]
module = import_module(module_name + '.management.commands.' + command_name)
command = module.Command()
command.add_arguments(Parser)

print(dumps(arguments), end='')
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

(defvar djangonaut-get-model-managers-code "
from __future__ import print_function

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from gc import get_objects
from inspect import findsource, getfile, getmodule, isclass
from json import dumps

from django.db.models import Manager

managers = {}
for obj in get_objects():
    if isclass(obj) and issubclass(obj, Manager):
        name = getmodule(obj).__name__ + '.' + obj.__name__
        managers[name] = [getfile(obj), findsource(obj)[1]]

print(dumps(managers), end='')
")

(defvar djangonaut-get-sql-functions-code "
from __future__ import print_function

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from gc import get_objects
from inspect import findsource, getfile, getmodule, isclass
from json import dumps

from django.db.models import Func

functions = {}
for obj in get_objects():
    if isclass(obj) and issubclass(obj, Func):
        name = getmodule(obj).__name__ + '.' + obj.__name__
        functions[name] = [getfile(obj), findsource(obj)[1]]

print(dumps(functions), end='')
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
from inspect import findsource, getfile, getmodule, isclass
from json import dumps

from rest_framework.serializers import Serializer

import_module(settings.ROOT_URLCONF)

serializers = {}
for obj in get_objects():
    if isclass(obj) and issubclass(obj, Serializer):
        name = getmodule(obj).__name__ + '.' + obj.__name__
        serializers[name] = [getfile(obj), findsource(obj)[1]]

print(dumps(serializers), end='')
")

(defvar djangonaut-get-drf-permissions-code "
from __future__ import print_function

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from gc import get_objects
from importlib import import_module
from inspect import findsource, getfile, getmodule, isclass
from json import dumps

from rest_framework.permissions import BasePermission

import_module(settings.ROOT_URLCONF)

permissions = {}
for obj in get_objects():
    if isclass(obj) and issubclass(obj, BasePermission):
        name = getmodule(obj).__name__ + '.' + obj.__name__
        permissions[name] = [getfile(obj), findsource(obj)[1]]

print(dumps(permissions), end='')
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

(defvar djangonaut-app-paths-history nil)

(defvar djangonaut-commands-history nil)

(defvar djangonaut-admin-classes-history nil)

(defvar djangonaut-models-history nil)

(defvar djangonaut-model-managers-history nil)

(defvar djangonaut-sql-functions-history nil)

(defvar djangonaut-signal-receivers-history nil)

(defvar djangonaut-drf-serializers-history nil)

(defvar djangonaut-drf-permissions-history nil)

(defvar djangonaut-views-history nil)

(defvar djangonaut-templates-history nil)

(defvar djangonaut-template-tags-history nil)

(defvar djangonaut-template-filters-history nil)

(defvar djangonaut-static-files-history nil)

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

(defun djangonaut-call (code &rest args)
  (let (exit-code output)
    (setq output
          (with-output-to-string
            (with-current-buffer
                standard-output
              (setq exit-code
                    (call-pythonic :buffer standard-output :args (append (list "-c" code) args))))))
    (when (not (zerop exit-code))
      (with-current-buffer (get-buffer-create "*Django*")
        (erase-buffer)
        (fundamental-mode)
        (insert output)
        (goto-char (point-min))
        (pop-to-buffer "*Django*")
        (error "Python exit with status code %d" exit-code)))
    output))

(defun djangonaut-find-file (func prompt collection hist)
  (let* ((key (intern (completing-read prompt (mapcar 'symbol-name (mapcar 'car collection)) nil t nil hist)))
         (value (cdr (assoc key collection))))
    (when (pythonic-remote-p)
      (setq value (concat (pythonic-tramp-connection) value)))
    (apply func value nil)))

(defun djangonaut-find-file-and-line (func prompt collection hist)
  (let* ((key (intern (completing-read prompt (mapcar 'symbol-name (mapcar 'car collection)) nil t nil hist)))
         (code (cdr (assoc key collection)))
         (value (elt code 0))
         (lineno (elt code 1)))
    (when (pythonic-remote-p)
      (setq value (concat (pythonic-tramp-connection) value)))
    (apply func value nil)
    (goto-char (point-min))
    (forward-line lineno)))

(defun djangonaut-get-commands ()
  (split-string (djangonaut-call djangonaut-get-commands-code) nil t))

(defun djangonaut-get-command-definitions ()
  (json-read-from-string (djangonaut-call djangonaut-get-command-definitions-code)))

(defun djangonaut-get-command-arguments (command)
  (json-read-from-string (djangonaut-call djangonaut-get-command-arguments-code command)))

(defun djangonaut-get-app-paths ()
  (json-read-from-string (djangonaut-call djangonaut-get-app-paths-code)))

(defun djangonaut-get-admin-classes ()
  (json-read-from-string (djangonaut-call djangonaut-get-admin-classes-code)))

(defun djangonaut-get-models ()
  (json-read-from-string (djangonaut-call djangonaut-get-models-code)))

(defun djangonaut-get-model-managers ()
  (json-read-from-string (djangonaut-call djangonaut-get-model-managers-code)))

(defun djangonaut-get-sql-functions ()
  (json-read-from-string (djangonaut-call djangonaut-get-sql-functions-code)))

(defun djangonaut-get-signal-receivers ()
  (json-read-from-string (djangonaut-call djangonaut-get-signal-receivers-code)))

(defun djangonaut-get-drf-serializers ()
  (json-read-from-string (djangonaut-call djangonaut-get-drf-serializers-code)))

(defun djangonaut-get-drf-permissions ()
  (json-read-from-string (djangonaut-call djangonaut-get-drf-permissions-code)))

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
  (interactive (split-string (completing-read "Command: " (djangonaut-get-commands) nil nil nil 'djangonaut-commands-history) " " t))
  (let* ((buffer (get-buffer-create "*Django*"))
         (process (get-buffer-process buffer)))
    (when (and process (process-live-p process))
      (setq buffer (generate-new-buffer "*Django*")))
    (with-current-buffer buffer
      (start-pythonic :process "djangonaut"
                      :buffer buffer
                      :args (append (list "-m" "django") command)
                      :cwd (djangonaut-get-project-root))
      (erase-buffer)
      (comint-mode)
      (pop-to-buffer buffer))))

(defun djangonaut-run-popup-management-command (command)
  (interactive (list (completing-read "Command: " (djangonaut-get-commands) nil t nil 'djangonaut-commands-history)))
  (let* ((arguments (djangonaut-get-command-arguments command))
         (func-name (intern (concat "djangonaut-run-" (s-replace "_" "-" command) "-popup")))
         (popup `(magit-define-popup ,func-name ""
                   :switches ',(mapcar (lambda (x) (list (elt (elt x 0) 0) (elt x 1) (elt x 2)))
                                       (cdr (assoc 'switches arguments)))
                   :options ',(mapcar (lambda (x) (list (elt (elt x 0) 0) (elt x 1) (elt x 2)))
                                      (cdr (assoc 'options arguments)))))
         (func (eval popup)))
    (funcall func)))

(defun djangonaut-dired-installed-apps ()
  (interactive)
  (djangonaut-find-file #'dired "App: " (djangonaut-get-app-paths) 'djangonaut-app-paths-history))

(defun djangonaut-dired-installed-apps-other-window ()
  (interactive)
  (djangonaut-find-file #'dired-other-window "App: " (djangonaut-get-app-paths) 'djangonaut-app-paths-history))

(defun djangonaut-find-management-command ()
  (interactive)
  (djangonaut-find-file-and-line #'find-file "Command: " (djangonaut-get-command-definitions) 'djangonaut-commands-history))

(defun djangonaut-find-management-command-other-window ()
  (interactive)
  (djangonaut-find-file-and-line #'find-file-other-window "Command: " (djangonaut-get-command-definitions) 'djangonaut-commands-history))

(defun djangonaut-find-admin-class ()
  (interactive)
  (djangonaut-find-file-and-line #'find-file "Admin Class: " (djangonaut-get-admin-classes) 'djangonaut-admin-classes-history))

(defun djangonaut-find-admin-class-other-window ()
  (interactive)
  (djangonaut-find-file-and-line #'find-file-other-window "Admin Class: " (djangonaut-get-admin-classes) 'djangonaut-admin-classes-history))

(defun djangonaut-find-model ()
  (interactive)
  (djangonaut-find-file-and-line #'find-file "Model: " (djangonaut-get-models) 'djangonaut-models-history))

(defun djangonaut-find-model-other-window ()
  (interactive)
  (djangonaut-find-file-and-line #'find-file-other-window "Model: " (djangonaut-get-models) 'djangonaut-models-history))

(defun djangonaut-find-model-manager ()
  (interactive)
  (djangonaut-find-file-and-line #'find-file "Model Manager: " (djangonaut-get-model-managers) 'djangonaut-model-managers-history))

(defun djangonaut-find-model-manager-other-window ()
  (interactive)
  (djangonaut-find-file-and-line #'find-file-other-window "Model Manager: " (djangonaut-get-model-managers) 'djangonaut-model-managers-history))

(defun djangonaut-find-sql-function ()
  (interactive)
  (djangonaut-find-file-and-line #'find-file "SQL Function: " (djangonaut-get-sql-functions) 'djangonaut-sql-functions-history))

(defun djangonaut-find-sql-function-other-window ()
  (interactive)
  (djangonaut-find-file-and-line #'find-file-other-window "SQL Function: " (djangonaut-get-sql-functions) 'djangonaut-sql-functions-history))

(defun djangonaut-find-signal-receiver ()
  (interactive)
  (djangonaut-find-file-and-line #'find-file "Signal Receiver: " (djangonaut-get-signal-receivers) 'djangonaut-signal-receivers-history))

(defun djangonaut-find-signal-receiver-other-window ()
  (interactive)
  (djangonaut-find-file-and-line #'find-file-other-window "Signal Receiver: " (djangonaut-get-signal-receivers) 'djangonaut-signal-receivers-history))

(defun djangonaut-find-drf-serializer ()
  (interactive)
  (djangonaut-find-file-and-line #'find-file "Serializer: " (djangonaut-get-drf-serializers) 'djangonaut-drf-serializers-history))

(defun djangonaut-find-drf-serializer-other-window ()
  (interactive)
  (djangonaut-find-file-and-line #'find-file-other-window "Serializer: " (djangonaut-get-drf-serializers) 'djangonaut-drf-serializers-history))

(defun djangonaut-find-drf-permission ()
  (interactive)
  (djangonaut-find-file-and-line #'find-file "Permission: " (djangonaut-get-drf-permissions) 'djangonaut-drf-permissions-history))

(defun djangonaut-find-drf-permission-other-window ()
  (interactive)
  (djangonaut-find-file-and-line #'find-file-other-window "Permission: " (djangonaut-get-drf-permissions) 'djangonaut-drf-permissions-history))

(defun djangonaut-find-view ()
  (interactive)
  (djangonaut-find-file-and-line #'find-file "View: " (djangonaut-get-views) 'djangonaut-views-history))

(defun djangonaut-find-view-other-window ()
  (interactive)
  (djangonaut-find-file-and-line #'find-file-other-window "View: " (djangonaut-get-views) 'djangonaut-views-history))

(defun djangonaut-find-template ()
  (interactive)
  (djangonaut-find-file #'find-file "Template: " (djangonaut-get-templates) 'djangonaut-templates-history))

(defun djangonaut-find-template-other-window ()
  (interactive)
  (djangonaut-find-file #'find-file-other-window "Template: " (djangonaut-get-templates) 'djangonaut-templates-history))

(defun djangonaut-find-template-tag ()
  (interactive)
  (djangonaut-find-file-and-line #'find-file "Template Tag: " (djangonaut-get-template-tags) 'djangonaut-template-tags-history))

(defun djangonaut-find-template-tag-other-window ()
  (interactive)
  (djangonaut-find-file-and-line #'find-file-other-window "Template Tag: " (djangonaut-get-template-tags) 'djangonaut-template-tags-history))

(defun djangonaut-find-template-filter ()
  (interactive)
  (djangonaut-find-file-and-line #'find-file "Template Filter: " (djangonaut-get-template-filters) 'djangonaut-template-filters-history))

(defun djangonaut-find-template-filter-other-window ()
  (interactive)
  (djangonaut-find-file-and-line #'find-file-other-window "Template Filter: " (djangonaut-get-template-filters) 'djangonaut-template-filters-history))

(defun djangonaut-find-static-file ()
  (interactive)
  (djangonaut-find-file #'find-file "Static File: " (djangonaut-get-static-files) 'djangonaut-static-files-history))

(defun djangonaut-find-static-file-other-window ()
  (interactive)
  (djangonaut-find-file #'find-file-other-window "Static File: " (djangonaut-get-static-files) 'djangonaut-static-files-history))

(defun djangonaut-find-settings-module ()
  (interactive)
  (let ((filename (djangonaut-get-settings-path)))
    (when (pythonic-remote-p)
      (setq filename (concat (pythonic-tramp-connection) filename)))
    (find-file filename)))

(defun djangonaut-find-settings-module-other-window ()
  (interactive)
  (let ((filename (djangonaut-get-settings-path)))
    (when (pythonic-remote-p)
      (setq filename (concat (pythonic-tramp-connection) filename)))
    (find-file-other-window filename)))

(defvar djangonaut-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c r !") 'djangonaut-run-management-command)
    (define-key map (kbd "C-c r i") 'djangonaut-dired-installed-apps)
    (define-key map (kbd "C-c r c") 'djangonaut-find-management-command)
    (define-key map (kbd "C-c r a") 'djangonaut-find-admin-class)
    (define-key map (kbd "C-c r m") 'djangonaut-find-model)
    (define-key map (kbd "C-c r M") 'djangonaut-find-model-manager)
    (define-key map (kbd "C-c r q") 'djangonaut-find-sql-function)
    (define-key map (kbd "C-c r r") 'djangonaut-find-signal-receiver)
    (define-key map (kbd "C-c r s") 'djangonaut-find-drf-serializer)
    (define-key map (kbd "C-c r p") 'djangonaut-find-drf-permission)
    (define-key map (kbd "C-c r v") 'djangonaut-find-view)
    (define-key map (kbd "C-c r t") 'djangonaut-find-template)
    (define-key map (kbd "C-c r g") 'djangonaut-find-template-tag)
    (define-key map (kbd "C-c r f") 'djangonaut-find-template-filter)
    (define-key map (kbd "C-c r j") 'djangonaut-find-static-file)
    (define-key map (kbd "C-c r S") 'djangonaut-find-settings-module)
    (define-key map (kbd "C-c r 4 i") 'djangonaut-dired-installed-apps-other-window)
    (define-key map (kbd "C-c r 4 c") 'djangonaut-find-management-command-other-window)
    (define-key map (kbd "C-c r 4 a") 'djangonaut-find-admin-class-other-window)
    (define-key map (kbd "C-c r 4 m") 'djangonaut-find-model-other-window)
    (define-key map (kbd "C-c r 4 M") 'djangonaut-find-model-manager-other-window)
    (define-key map (kbd "C-c r 4 q") 'djangonaut-find-sql-function-other-window)
    (define-key map (kbd "C-c r 4 r") 'djangonaut-find-signal-receiver-other-window)
    (define-key map (kbd "C-c r 4 s") 'djangonaut-find-drf-serializer-other-window)
    (define-key map (kbd "C-c r 4 p") 'djangonaut-find-drf-permission-other-window)
    (define-key map (kbd "C-c r 4 v") 'djangonaut-find-view-other-window)
    (define-key map (kbd "C-c r 4 t") 'djangonaut-find-template-other-window)
    (define-key map (kbd "C-c r 4 g") 'djangonaut-find-template-tag-other-window)
    (define-key map (kbd "C-c r 4 f") 'djangonaut-find-template-filter-other-window)
    (define-key map (kbd "C-c r 4 j") 'djangonaut-find-static-file-other-window)
    (define-key map (kbd "C-c r 4 S") 'djangonaut-find-settings-module-other-window)
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
