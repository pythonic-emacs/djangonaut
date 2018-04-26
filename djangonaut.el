;;; djangonaut.el --- Minor mode to interact with Django projects  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/proofit404/djangonaut
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.2") (magit-popup "2.6.0") (pythonic "0.1.0") (f "0.20.0") (s "1.12.0"))

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
(require 'ansi-color)
(require 'pythonic)
(require 'compile)
(require 'comint)
(require 'json)
(require 'f)
(require 's)

(defvar djangonaut-keymap-prefix (kbd "C-c '")
  "Djangonaut keymap prefix.")

(defvar djangonaut-get-pythonpath-code "
from __future__ import print_function

from sys import path

print('\\n'.join(path))
" "Python source code to get PYTHONPATH.")

(defvar djangonaut-get-project-root-code "
from __future__ import print_function

import os, sys
stdout = sys.stdout
sys.stdout = open(os.devnull, 'w')
sys.stderr = open(os.devnull, 'w')

from importlib import import_module
from os import environ
from os.path import dirname

settings_module = environ['DJANGO_SETTINGS_MODULE']
package_name = settings_module.split('.', 1)[0]
package = import_module(package_name)
project_root = dirname(dirname(package.__file__))

print(project_root, end='', file=stdout)
" "Python source code to get project root.")

(defvar djangonaut-get-commands-code "
from __future__ import print_function

import os, sys
stdout = sys.stdout
sys.stdout = open(os.devnull, 'w')
sys.stderr = open(os.devnull, 'w')

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from json import dumps

from django.core.management import get_commands

commands = list(get_commands().keys())

print(dumps(commands), end='', file=stdout)
" "Python source code to get commands.")

(defvar djangonaut-get-command-definitions-code "
from __future__ import print_function

import os, sys
stdout = sys.stdout
sys.stdout = open(os.devnull, 'w')
sys.stderr = open(os.devnull, 'w')

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from importlib import import_module
from inspect import findsource, getsourcefile
from json import dumps

from django.core.management import get_commands

commands = {}
for command_name, module_name in get_commands().items():
    module = import_module(module_name + '.management.commands.' + command_name)
    command = module.Command
    commands[command_name] = [getsourcefile(command), findsource(command)[1]]

print(dumps(commands), end='', file=stdout)
" "Python source code to get command definitions.")

(defvar djangonaut-get-command-arguments-code "
from __future__ import print_function

import os, sys
stdout = sys.stdout
sys.stdout = open(os.devnull, 'w')
sys.stderr = open(os.devnull, 'w')

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

        assert 0 < len(args) < 3, 'Unsupported arguments: {0} {1}'.format(args, kwargs)

        if kwargs.get('action') in ('store_true', 'store_false'):
            target = arguments.setdefault('switches', [])
            get_option = lambda x, end: x
        else:
            target = arguments.setdefault('options', [])
            get_option = lambda x, end: x + end

        data = {'optional': None, 'short': None, 'positional': None}

        for arg in args:
            if arg.startswith('--'):
                data['optional'] = arg
            elif arg.startswith('-'):
                data['short'] = arg
            else:
                data['positional'] = arg

        if data['positional']:
            key = '='
            option = ''
            suffix = ''
        elif data['short']:
            key = data['short'][1]
            option = data['optional'] or data['short']
            suffix = '=' if data['optional'] else ' '
        else:
            key = data['optional'][2]
            option = data['optional']
            suffix = '='

        name = get_option(option, suffix)
        shortcut = get_free_shortcut(key)
        description = kwargs.get('help') or data['positional'] or data['optional'] or data['short']
        target.append([shortcut, description, name])

command_name = argv[-1]
module_name = get_commands()[command_name]
module = import_module(module_name + '.management.commands.' + command_name)
command = module.Command()
command.add_arguments(Parser)

print(dumps(arguments), end='', file=stdout)
" "Python source code to get command arguments.")

(defvar djangonaut-get-app-paths-code "
from __future__ import print_function

import os, sys
stdout = sys.stdout
sys.stdout = open(os.devnull, 'w')
sys.stderr = open(os.devnull, 'w')

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from json import dumps

paths = {app.label: app.path for app in apps.get_app_configs()}

print(dumps(paths), end='', file=stdout)
" "Python source code to get app paths.")

(defvar djangonaut-get-admin-classes-code "
from __future__ import print_function

import os, sys
stdout = sys.stdout
sys.stdout = open(os.devnull, 'w')
sys.stderr = open(os.devnull, 'w')

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from inspect import findsource, getsourcefile
from json import dumps

try:
    from django.contrib.admin.sites import all_sites
except ImportError:
    from gc import get_objects
    from django.contrib.admin.sites import AdminSite
    all_sites = []
    for obj in get_objects():
        if isinstance(obj, AdminSite):
            all_sites.append(obj)

admin_classes = {}
for site in all_sites:
    for admin_instance in site._registry.values():
        admin_class = admin_instance.__class__
        admin_classes[str(admin_instance)] = [getsourcefile(admin_class), findsource(admin_class)[1]]

print(dumps(admin_classes), end='', file=stdout)
" "Python source code to get admin classes.")

(defvar djangonaut-get-models-code "
from __future__ import print_function

import os, sys
stdout = sys.stdout
sys.stdout = open(os.devnull, 'w')
sys.stderr = open(os.devnull, 'w')

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from inspect import findsource, getsourcefile
from json import dumps

models = {model._meta.app_label + '.' + model.__name__: [getsourcefile(model), findsource(model)[1]] for model in apps.get_models()}

print(dumps(models), end='', file=stdout)
" "Python source code to get models.")

(defvar djangonaut-get-model-managers-code "
from __future__ import print_function

import os, sys
stdout = sys.stdout
sys.stdout = open(os.devnull, 'w')
sys.stderr = open(os.devnull, 'w')

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from gc import get_objects
from inspect import findsource, getsourcefile, getmodule, isclass
from json import dumps

from django.db.models import Manager

managers = {}
for obj in get_objects():
    if isclass(obj) and issubclass(obj, Manager):
        name = getmodule(obj).__name__ + '.' + obj.__name__
        managers[name] = [getsourcefile(obj), findsource(obj)[1]]

print(dumps(managers), end='', file=stdout)
" "Python source code to get model managers.")

(defvar djangonaut-get-migrations-code "
from __future__ import print_function

import os, sys
stdout = sys.stdout
sys.stdout = open(os.devnull, 'w')
sys.stderr = open(os.devnull, 'w')

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from inspect import findsource, getsourcefile
from json import dumps

from django.db.migrations.loader import MigrationLoader

loader = MigrationLoader(connection=None, load=False)
loader.load_disk()

migrations = {}
for (label, module_name), migration in sorted(loader.disk_migrations.items()):
    name = label + '.' + module_name
    Migration = migration.__class__
    migrations[name] = [getsourcefile(Migration), findsource(Migration)[1]]

print(dumps(migrations), end='', file=stdout)
" "Python source code to get migrations.")

(defvar djangonaut-get-sql-functions-code "
from __future__ import print_function

import os, sys
stdout = sys.stdout
sys.stdout = open(os.devnull, 'w')
sys.stderr = open(os.devnull, 'w')

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from gc import get_objects
from inspect import findsource, getsourcefile, getmodule, isclass
from json import dumps

from django.db.models import Func

functions = {}
for obj in get_objects():
    if isclass(obj) and issubclass(obj, Func):
        name = getmodule(obj).__name__ + '.' + obj.__name__
        functions[name] = [getsourcefile(obj), findsource(obj)[1]]

print(dumps(functions), end='', file=stdout)
" "Python source code to get sql functions.")

(defvar djangonaut-get-signal-receivers-code "
from __future__ import print_function

import os, sys
stdout = sys.stdout
sys.stdout = open(os.devnull, 'w')
sys.stderr = open(os.devnull, 'w')

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from gc import get_objects
from inspect import findsource, getsourcefile, getmodule
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
            receivers[name] = [getsourcefile(receiver), findsource(receiver)[1]]

print(dumps(receivers), end='', file=stdout)
" "Python source code to get signal receivers.")

(defvar djangonaut-get-drf-serializers-code "
from __future__ import print_function

import os, sys
stdout = sys.stdout
sys.stdout = open(os.devnull, 'w')
sys.stderr = open(os.devnull, 'w')

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from gc import get_objects
from importlib import import_module
from inspect import findsource, getsourcefile, getmodule, isclass
from json import dumps

from rest_framework.serializers import Serializer

import_module(settings.ROOT_URLCONF)

serializers = {}
for obj in get_objects():
    if isclass(obj) and issubclass(obj, Serializer):
        name = getmodule(obj).__name__ + '.' + obj.__name__
        serializers[name] = [getsourcefile(obj), findsource(obj)[1]]

print(dumps(serializers), end='', file=stdout)
" "Python source code to get drf serializers.")

(defvar djangonaut-get-drf-permissions-code "
from __future__ import print_function

import os, sys
stdout = sys.stdout
sys.stdout = open(os.devnull, 'w')
sys.stderr = open(os.devnull, 'w')

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from gc import get_objects
from importlib import import_module
from inspect import findsource, getsourcefile, getmodule, isclass
from json import dumps

from rest_framework.permissions import BasePermission

import_module(settings.ROOT_URLCONF)

permissions = {}
for obj in get_objects():
    if isclass(obj) and issubclass(obj, BasePermission):
        name = getmodule(obj).__name__ + '.' + obj.__name__
        permissions[name] = [getsourcefile(obj), findsource(obj)[1]]

print(dumps(permissions), end='', file=stdout)
" "Python source code to get drf permissions.")

(defvar djangonaut-get-views-code "
from __future__ import print_function

import os, sys
stdout = sys.stdout
sys.stdout = open(os.devnull, 'w')
sys.stderr = open(os.devnull, 'w')

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from inspect import findsource, getsourcefile, getmodule, ismethod
from json import dumps

try:
    from django.urls import get_resolver, get_urlconf
except ImportError:
    from django.core.urlresolvers import get_resolver, get_urlconf

try:
    from django.urls.resolvers import LocalePrefixPattern, RegexPattern, RoutePattern, URLPattern, URLResolver
    pattern_classes = (LocalePrefixPattern, RegexPattern, RoutePattern, URLPattern)
    resolver_classes = (URLResolver,)
except ImportError:
    try:
        from django.urls import RegexURLPattern, RegexURLResolver
        pattern_classes = (RegexURLPattern,)
        resolver_classes = (RegexURLResolver,)
    except ImportError:
        from django.core.urlresolvers import RegexURLPattern, RegexURLResolver
        pattern_classes = (RegexURLPattern,)
        resolver_classes = (RegexURLResolver,)

try:
    from inspect import unwrap
except ImportError:
    def unwrap(func):
        while hasattr(func, '__wrapped__'):
            func = func.__wrapped__
        return func

views = {}

def collect_views(resolver):
    for pattern in resolver.url_patterns:
        if isinstance(pattern, resolver_classes):
            collect_views(pattern)
        elif isinstance(pattern, pattern_classes):
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
            views[name] = [getsourcefile(view), findsource(view)[1]]

collect_views(get_resolver(get_urlconf()))

print(dumps(views), end='', file=stdout)
" "Python source code to get views.")

(defvar djangonaut-get-url-modules-code "
from __future__ import print_function

import os, sys
stdout = sys.stdout
sys.stdout = open(os.devnull, 'w')
sys.stderr = open(os.devnull, 'w')

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from json import dumps
from types import ModuleType

try:
    from django.urls import get_resolver, get_urlconf
except ImportError:
    from django.core.urlresolvers import get_resolver, get_urlconf

try:
    from django.urls import URLResolver
    resolver_classes = (URLResolver,)
except ImportError:
    try:
        from django.urls import RegexURLResolver
        resolver_classes = (RegexURLResolver,)
    except ImportError:
        from django.core.urlresolvers import RegexURLResolver
        resolver_classes = (RegexURLResolver,)

url_modules = {}

def collect_url_modules(conf):
    name = conf.urlconf_name
    if isinstance(name, ModuleType):
        name = name.__name__
    url_modules[name] = conf.urlconf_module.__file__
    for pattern in conf.url_patterns:
        if isinstance(pattern, resolver_classes) and not isinstance(pattern.urlconf_module, list):
            collect_url_modules(pattern)

collect_url_modules(get_resolver(get_urlconf()))

print(dumps(url_modules), end='', file=stdout)
" "Python source code to get url modules.")

(defvar djangonaut-get-templates-code "
from __future__ import print_function

import os, sys
stdout = sys.stdout
sys.stdout = open(os.devnull, 'w')
sys.stderr = open(os.devnull, 'w')

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from json import dumps
from os import walk
from os.path import join

from django.contrib.staticfiles.utils import matches_patterns
from django.template import engines
from django.template.backends.django import DjangoTemplates
from django.template.loaders.filesystem import Loader as FileSystemLoader
from django.template.loaders.app_directories import Loader as AppDirectoriesLoader
from django.template.utils import get_app_template_dirs

ignore_patterns = ['CVS', '.*', '*~']

templates = {}

for engine in engines.all():
    if isinstance(engine, DjangoTemplates):
        for loader in engine.engine.template_loaders:
            if isinstance(loader, (FileSystemLoader, AppDirectoriesLoader)):
                try:
                    dirs = loader.get_dirs()
                except AttributeError:
                    if isinstance(loader, AppDirectoriesLoader):
                        dirs = get_app_template_dirs('templates')
                    else:
                        dirs = loader.engine.dirs
                for template_directory in dirs:
                    for root, _, files in walk(template_directory):
                        for template in files:
                            template_path = join(root, template)
                            if not matches_patterns(template_path, ignore_patterns):
                                templates.setdefault(template_path[len(template_directory) + 1:], template_path)

print(dumps(templates), end='', file=stdout)
" "Python source code to get templates.")

(defvar djangonaut-get-template-tags-code "
from __future__ import print_function

import os, sys
stdout = sys.stdout
sys.stdout = open(os.devnull, 'w')
sys.stderr = open(os.devnull, 'w')

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from importlib import import_module
from inspect import findsource, getsourcefile
from json import dumps

try:
    from inspect import unwrap
except ImportError:
    def unwrap(func):
        while hasattr(func, '__wrapped__'):
            func = func.__wrapped__
        return func

libraries = {}
libraries['builtin'] = import_module('django.template.defaulttags').register

try:
    from django.template.backends.django import get_installed_libraries

    for library_name, library_path in get_installed_libraries().items():
        libraries[library_name] = import_module(library_path).register
except ImportError:
    from pkgutil import walk_packages
    from django.template.base import get_templatetags_modules

    for package_name in get_templatetags_modules():
        package = import_module(package_name)
        if hasattr(package, '__path__'):
            for entry in walk_packages(package.__path__, package.__name__ + '.'):
                module = import_module(entry[1])
                if hasattr(module, 'register'):
                    libraries[entry[1][len(package_name) + 1:]] = module.register

template_tags = {}
for library_name, library in libraries.items():
    for tag_name, tag in library.tags.items():
        tag = unwrap(tag)
        try:
            template_tags[library_name + '.' + tag_name] = [getsourcefile(tag), findsource(tag)[1]]
        except TypeError:
            # This is Django 1.8 and we met functools.partial result.  We take class defined
            # in the decorator from bound keyword arguments.  This class has a method with a
            # closure where we can find decorated function.
            tag = tag.keywords['node_class'].render.__closure__[-1].cell_contents
            template_tags[library_name + '.' + tag_name] = [getsourcefile(tag), findsource(tag)[1]]

print(dumps(template_tags), end='', file=stdout)
" "Python source code to get template tags.")

(defvar djangonaut-get-template-filters-code "
from __future__ import print_function

import os, sys
stdout = sys.stdout
sys.stdout = open(os.devnull, 'w')
sys.stderr = open(os.devnull, 'w')

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from importlib import import_module
from inspect import findsource, getsourcefile
from json import dumps

try:
    from inspect import unwrap
except ImportError:
    def unwrap(func):
        while hasattr(func, '__wrapped__'):
            func = func.__wrapped__
        return func

libraries = {}
libraries['builtin'] = import_module('django.template.defaulttags').register

try:
    from django.template.backends.django import get_installed_libraries

    for library_name, library_path in get_installed_libraries().items():
        libraries[library_name] = import_module(library_path).register
except ImportError:
    from pkgutil import walk_packages
    from django.template.base import get_templatetags_modules

    for package_name in get_templatetags_modules():
        package = import_module(package_name)
        if hasattr(package, '__path__'):
            for entry in walk_packages(package.__path__, package.__name__ + '.'):
                module = import_module(entry[1])
                if hasattr(module, 'register'):
                    libraries[entry[1][len(package_name) + 1:]] = module.register

template_filters = {}
for library_name, library in libraries.items():
    for filter_name, filter in library.filters.items():
        filter = unwrap(filter)
        template_filters[library_name + '.' + filter_name] = [getsourcefile(filter), findsource(filter)[1]]

print(dumps(template_filters), end='', file=stdout)
" "Python source code to get template filters.")

(defvar djangonaut-get-static-files-code "
from __future__ import print_function

import os, sys
stdout = sys.stdout
sys.stdout = open(os.devnull, 'w')
sys.stderr = open(os.devnull, 'w')

from django.apps import apps
from django.conf import settings
apps.populate(settings.INSTALLED_APPS)

from json import dumps

from django.contrib.staticfiles.finders import get_finders

ignore_patterns = ['CVS', '.*', '*~']

staticfiles = {}
for finder in get_finders():
    for path, storage in finder.list(ignore_patterns):
        staticfiles.setdefault(path, storage.path(path))

print(dumps(staticfiles), end='', file=stdout)
" "Python source code to get static files.")

(defvar djangonaut-get-settings-path-code "
from __future__ import print_function

import os, sys
stdout = sys.stdout
sys.stdout = open(os.devnull, 'w')
sys.stderr = open(os.devnull, 'w')

from importlib import import_module
from inspect import getsourcefile
from os import environ

settings_module = environ['DJANGO_SETTINGS_MODULE']
module = import_module(settings_module)
settings_path = getsourcefile(module)

print(settings_path, end='', file=stdout)
" "Python source code to get settings path.")

(defvar djangonaut-app-paths-history nil)

(defvar djangonaut-commands-history nil)

(defvar djangonaut-admin-classes-history nil)

(defvar djangonaut-models-history nil)

(defvar djangonaut-model-managers-history nil)

(defvar djangonaut-migrations-history nil)

(defvar djangonaut-sql-functions-history nil)

(defvar djangonaut-signal-receivers-history nil)

(defvar djangonaut-drf-serializers-history nil)

(defvar djangonaut-drf-permissions-history nil)

(defvar djangonaut-views-history nil)

(defvar djangonaut-url-modules-history nil)

(defvar djangonaut-templates-history nil)

(defvar djangonaut-template-tags-history nil)

(defvar djangonaut-template-filters-history nil)

(defvar djangonaut-static-files-history nil)

(defun djangonaut-get-pythonpath ()
  "Execute and parse python code to get PYTHONPATH."
  (split-string
   (with-output-to-string
     (with-current-buffer
         standard-output
       (hack-dir-local-variables-non-file-buffer)
       (call-pythonic :buffer standard-output
                      :args (list "-c" djangonaut-get-pythonpath-code))))
   nil t))

(defun djangonaut-get-project-root ()
  "Execute and parse python code to get project root."
  (with-output-to-string
    (with-current-buffer
        standard-output
      (hack-dir-local-variables-non-file-buffer)
      (call-pythonic :buffer standard-output
                     :args (list "-c" djangonaut-get-project-root-code)))))

(defun djangonaut-call (code &rest args)
  "Execute python CODE with ARGS.  Show errors if occurs."
  (let (exit-code output)
    (setq output
          (with-output-to-string
            (with-current-buffer
                standard-output
              (hack-dir-local-variables-non-file-buffer)
              (setq exit-code
                    (call-pythonic :buffer standard-output :args (append (list "-c" code) args))))))
    (when (not (zerop exit-code))
      (djangonaut-show-error output (format "Python exit with status code %d" exit-code)))
    output))

(defun djangonaut-read (str)
  "Read JSON from Python process output STR."
  (condition-case err
      (let ((result (json-read-from-string str)))
        (unless (listp result)
          (signal 'json-error nil))
        result)
    ((json-error wrong-type-argument)
     (djangonaut-show-error str (error-message-string err)))))

(defun djangonaut-show-error (output error-message)
  "Prepare and show OUTPUT in the ERROR-MESSAGE buffer."
  (let* ((buffer (get-buffer-create "*Django*"))
         (process (get-buffer-process buffer)))
    (when (and process (process-live-p process))
      (setq buffer (generate-new-buffer "*Django*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (fundamental-mode)
      (insert output)
      (goto-char (point-min))
      (compilation-minor-mode 1)
      (pop-to-buffer buffer)
      (error error-message))))

(defun djangonaut-find-file (func prompt collection hist)
  "Ask user to select some name and open its definition.

FUNC is function to open file.  PROMPT and COLLECTION stands for
user input.  HIST is a variable to store history of choices."
  (let* ((key (intern (completing-read prompt (mapcar 'symbol-name (mapcar 'car collection)) nil t nil hist)))
         (value (cdr (assoc key collection))))
    (when (pythonic-remote-p)
      (setq value (concat (pythonic-tramp-connection) value)))
    (apply func value nil)))

(defun djangonaut-find-file-and-line (func prompt collection hist)
  "Ask user to select some name and open its definition at the line number.

FUNC is function to open file.  PROMPT and COLLECTION stands for
user input.  HIST is a variable to store history of choices."
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
  "Execute and parse python code to get commands."
  (let ((json-array-type 'list))
    (djangonaut-read (djangonaut-call djangonaut-get-commands-code))))

(defun djangonaut-get-command-definitions ()
  "Execute and parse python code to get command definitions."
  (djangonaut-read (djangonaut-call djangonaut-get-command-definitions-code)))

(defun djangonaut-get-command-arguments (command)
  "Execute and parse python code to get COMMAND arguments."
  (djangonaut-read (djangonaut-call djangonaut-get-command-arguments-code command)))

(defun djangonaut-get-app-paths ()
  "Execute and parse python code to get app paths."
  (djangonaut-read (djangonaut-call djangonaut-get-app-paths-code)))

(defun djangonaut-get-admin-classes ()
  "Execute and parse python code to get admin classes."
  (djangonaut-read (djangonaut-call djangonaut-get-admin-classes-code)))

(defun djangonaut-get-models ()
  "Execute and parse python code to get models."
  (djangonaut-read (djangonaut-call djangonaut-get-models-code)))

(defun djangonaut-get-model-managers ()
  "Execute and parse python code to get model managers."
  (djangonaut-read (djangonaut-call djangonaut-get-model-managers-code)))

(defun djangonaut-get-migrations ()
  "Execute and parse python code to get migrations."
  (djangonaut-read (djangonaut-call djangonaut-get-migrations-code)))

(defun djangonaut-get-sql-functions ()
  "Execute and parse python code to get sql functions."
  (djangonaut-read (djangonaut-call djangonaut-get-sql-functions-code)))

(defun djangonaut-get-signal-receivers ()
  "Execute and parse python code to get signal receivers."
  (djangonaut-read (djangonaut-call djangonaut-get-signal-receivers-code)))

(defun djangonaut-get-drf-serializers ()
  "Execute and parse python code to get drf serializers."
  (djangonaut-read (djangonaut-call djangonaut-get-drf-serializers-code)))

(defun djangonaut-get-drf-permissions ()
  "Execute and parse python code to get drf permissions."
  (djangonaut-read (djangonaut-call djangonaut-get-drf-permissions-code)))

(defun djangonaut-get-views ()
  "Execute and parse python code to get views."
  (djangonaut-read (djangonaut-call djangonaut-get-views-code)))

(defun djangonaut-get-url-modules ()
  "Execute and parse python code to get url modules."
  (djangonaut-read (djangonaut-call djangonaut-get-url-modules-code)))

(defun djangonaut-get-templates ()
  "Execute and parse python code to get templates."
  (djangonaut-read (djangonaut-call djangonaut-get-templates-code)))

(defun djangonaut-get-template-tags ()
  "Execute and parse python code to get template tags."
  (djangonaut-read (djangonaut-call djangonaut-get-template-tags-code)))

(defun djangonaut-get-template-filters ()
  "Execute and parse python code to get template filters."
  (djangonaut-read (djangonaut-call djangonaut-get-template-filters-code)))

(defun djangonaut-get-static-files ()
  "Execute and parse python code to get static files."
  (djangonaut-read (djangonaut-call djangonaut-get-static-files-code)))

(defun djangonaut-get-settings-path ()
  "Execute and parse python code to get settings path."
  (djangonaut-call djangonaut-get-settings-path-code))

(defun djangonaut-run-management-command-dwim ()
  "Run management command."
  (interactive)
  (call-interactively
   (if current-prefix-arg
       'djangonaut-run-popup-management-command
     'djangonaut-run-management-command)))

(defun djangonaut-run-management-command (&rest command)
  "Run management COMMAND in the comint buffer."
  (interactive (split-string (completing-read "Command: " (djangonaut-get-commands) nil nil nil 'djangonaut-commands-history) " " t))
  (let* ((buffer (get-buffer-create "*Django*"))
         (process (get-buffer-process buffer)))
    (when (and process (process-live-p process))
      (setq buffer (generate-new-buffer "*Django*")))
    (with-current-buffer buffer
      (hack-dir-local-variables-non-file-buffer)
      (start-pythonic :process "djangonaut"
                      :buffer buffer
                      :args (append (list "-m" "django") command)
                      :cwd (djangonaut-get-project-root)
                      :filter (lambda (process string)
                                (comint-output-filter process (ansi-color-apply string))))
      (let ((inhibit-read-only t))
        (erase-buffer))
      (comint-mode)
      (setq-local comint-prompt-read-only t)
      (pop-to-buffer buffer))))

(defun djangonaut-run-popup-management-command (command)
  "Run management COMMAND with arguments specified in the popup buffer."
  (interactive (list (completing-read "Popup Command: " (djangonaut-get-commands) nil t nil 'djangonaut-commands-history)))
  (let* ((arguments (djangonaut-get-command-arguments command))
         (func-name (intern (concat "djangonaut-run-" (s-replace "_" "-" command) "-popup")))
         (args-name (intern (concat "djangonaut-run-" (s-replace "_" "-" command) "-arguments")))
         (popup `(magit-define-popup ,func-name ""
                   :switches ',(mapcar (lambda (x) (list (elt (elt x 0) 0) (elt x 1) (elt x 2)))
                                       (cdr (assoc 'switches arguments)))
                   :options ',(mapcar (lambda (x) (list (elt (elt x 0) 0) (elt x 1) (elt x 2)))
                                      (cdr (assoc 'options arguments)))
                   :actions '((?\  "Run" (lambda ()
                                           (interactive)
                                           (apply 'djangonaut-run-management-command ,command (,args-name)))))))
         (func (eval popup)))
    (funcall func)))

(defun djangonaut-dired-installed-apps ()
  "Open application directory in the dired buffer."
  (interactive)
  (djangonaut-find-file #'dired "App: " (djangonaut-get-app-paths) 'djangonaut-app-paths-history))

(defun djangonaut-dired-installed-apps-other-window ()
  "Open application directory in the dired buffer in the other window."
  (interactive)
  (djangonaut-find-file #'dired-other-window "App: " (djangonaut-get-app-paths) 'djangonaut-app-paths-history))

(defun djangonaut-find-management-command ()
  "Open definition of the Django management command."
  (interactive)
  (djangonaut-find-file-and-line #'find-file "Command: " (djangonaut-get-command-definitions) 'djangonaut-commands-history))

(defun djangonaut-find-management-command-other-window ()
  "Open definition of the Django management command in other window."
  (interactive)
  (djangonaut-find-file-and-line #'find-file-other-window "Command: " (djangonaut-get-command-definitions) 'djangonaut-commands-history))

(defun djangonaut-find-admin-class ()
  "Open definition of the Django admin class."
  (interactive)
  (djangonaut-find-file-and-line #'find-file "Admin Class: " (djangonaut-get-admin-classes) 'djangonaut-admin-classes-history))

(defun djangonaut-find-admin-class-other-window ()
  "Open definition of the Django admin class in the other window."
  (interactive)
  (djangonaut-find-file-and-line #'find-file-other-window "Admin Class: " (djangonaut-get-admin-classes) 'djangonaut-admin-classes-history))

(defun djangonaut-find-model ()
  "Open definition of the Django model."
  (interactive)
  (djangonaut-find-file-and-line #'find-file "Model: " (djangonaut-get-models) 'djangonaut-models-history))

(defun djangonaut-find-model-other-window ()
  "Open definition of the Django model in the other window."
  (interactive)
  (djangonaut-find-file-and-line #'find-file-other-window "Model: " (djangonaut-get-models) 'djangonaut-models-history))

(defun djangonaut-find-model-manager ()
  "Open definition of the Django model manager."
  (interactive)
  (djangonaut-find-file-and-line #'find-file "Model Manager: " (djangonaut-get-model-managers) 'djangonaut-model-managers-history))

(defun djangonaut-find-model-manager-other-window ()
  "Open definition of the Django model manager in the other window."
  (interactive)
  (djangonaut-find-file-and-line #'find-file-other-window "Model Manager: " (djangonaut-get-model-managers) 'djangonaut-model-managers-history))

(defun djangonaut-find-migration ()
  "Open definition of the Django migration."
  (interactive)
  (djangonaut-find-file-and-line #'find-file "Migration: " (djangonaut-get-migrations) 'djangonaut-migrations-history))

(defun djangonaut-find-migration-other-window ()
  "Open definition of the Django migration in the other window."
  (interactive)
  (djangonaut-find-file-and-line #'find-file-other-window "Migration: " (djangonaut-get-migrations) 'djangonaut-migrations-history))

(defun djangonaut-find-sql-function ()
  "Open definition of the Django sql function."
  (interactive)
  (djangonaut-find-file-and-line #'find-file "SQL Function: " (djangonaut-get-sql-functions) 'djangonaut-sql-functions-history))

(defun djangonaut-find-sql-function-other-window ()
  "Open definition of the Django sql function in the other window."
  (interactive)
  (djangonaut-find-file-and-line #'find-file-other-window "SQL Function: " (djangonaut-get-sql-functions) 'djangonaut-sql-functions-history))

(defun djangonaut-find-signal-receiver ()
  "Open definition of the Django signal receiver."
  (interactive)
  (djangonaut-find-file-and-line #'find-file "Signal Receiver: " (djangonaut-get-signal-receivers) 'djangonaut-signal-receivers-history))

(defun djangonaut-find-signal-receiver-other-window ()
  "Open definition of the Django signal receiver in the other window."
  (interactive)
  (djangonaut-find-file-and-line #'find-file-other-window "Signal Receiver: " (djangonaut-get-signal-receivers) 'djangonaut-signal-receivers-history))

(defun djangonaut-find-drf-serializer ()
  "Open definition of the Django drf serializer."
  (interactive)
  (djangonaut-find-file-and-line #'find-file "Serializer: " (djangonaut-get-drf-serializers) 'djangonaut-drf-serializers-history))

(defun djangonaut-find-drf-serializer-other-window ()
  "Open definition of the Django drf serializer in the other window."
  (interactive)
  (djangonaut-find-file-and-line #'find-file-other-window "Serializer: " (djangonaut-get-drf-serializers) 'djangonaut-drf-serializers-history))

(defun djangonaut-find-drf-permission ()
  "Open definition of the Django drf permission."
  (interactive)
  (djangonaut-find-file-and-line #'find-file "Permission: " (djangonaut-get-drf-permissions) 'djangonaut-drf-permissions-history))

(defun djangonaut-find-drf-permission-other-window ()
  "Open definition of the Django drf permission in the other window."
  (interactive)
  (djangonaut-find-file-and-line #'find-file-other-window "Permission: " (djangonaut-get-drf-permissions) 'djangonaut-drf-permissions-history))

(defun djangonaut-find-view ()
  "Open definition of the Django view."
  (interactive)
  (djangonaut-find-file-and-line #'find-file "View: " (djangonaut-get-views) 'djangonaut-views-history))

(defun djangonaut-find-view-other-window ()
  "Open definition of the Django view in the other window."
  (interactive)
  (djangonaut-find-file-and-line #'find-file-other-window "View: " (djangonaut-get-views) 'djangonaut-views-history))

(defun djangonaut-find-url-module ()
  "Open definition of the Django url module."
  (interactive)
  (djangonaut-find-file #'find-file "URL Module: " (djangonaut-get-url-modules) 'djangonaut-url-modules-history))

(defun djangonaut-find-url-module-other-window ()
  "Open definition of the Django url module in the other window."
  (interactive)
  (djangonaut-find-file #'find-file-other-window "URL Module: " (djangonaut-get-url-modules) 'djangonaut-url-modules-history))

(defun djangonaut-find-template ()
  "Open definition of the Django template."
  (interactive)
  (djangonaut-find-file #'find-file "Template: " (djangonaut-get-templates) 'djangonaut-templates-history))

(defun djangonaut-find-template-other-window ()
  "Open definition of the Django template in the other window."
  (interactive)
  (djangonaut-find-file #'find-file-other-window "Template: " (djangonaut-get-templates) 'djangonaut-templates-history))

(defun djangonaut-find-template-tag ()
  "Open definition of the Django template tag."
  (interactive)
  (djangonaut-find-file-and-line #'find-file "Template Tag: " (djangonaut-get-template-tags) 'djangonaut-template-tags-history))

(defun djangonaut-find-template-tag-other-window ()
  "Open definition of the Django template tag in the other window."
  (interactive)
  (djangonaut-find-file-and-line #'find-file-other-window "Template Tag: " (djangonaut-get-template-tags) 'djangonaut-template-tags-history))

(defun djangonaut-find-template-filter ()
  "Open definition of the Django template filter."
  (interactive)
  (djangonaut-find-file-and-line #'find-file "Template Filter: " (djangonaut-get-template-filters) 'djangonaut-template-filters-history))

(defun djangonaut-find-template-filter-other-window ()
  "Open definition of the Django template filter in the other window."
  (interactive)
  (djangonaut-find-file-and-line #'find-file-other-window "Template Filter: " (djangonaut-get-template-filters) 'djangonaut-template-filters-history))

(defun djangonaut-find-static-file ()
  "Open definition of the Django static file."
  (interactive)
  (djangonaut-find-file #'find-file "Static File: " (djangonaut-get-static-files) 'djangonaut-static-files-history))

(defun djangonaut-find-static-file-other-window ()
  "Open definition of the Django static file in the other window."
  (interactive)
  (djangonaut-find-file #'find-file-other-window "Static File: " (djangonaut-get-static-files) 'djangonaut-static-files-history))

(defun djangonaut-find-settings-module ()
  "Open definition of the Django settings module."
  (interactive)
  (let ((filename (djangonaut-get-settings-path)))
    (when (pythonic-remote-p)
      (setq filename (concat (pythonic-tramp-connection) filename)))
    (find-file filename)))

(defun djangonaut-find-settings-module-other-window ()
  "Open definition of the Django settings module in the other window."
  (interactive)
  (let ((filename (djangonaut-get-settings-path)))
    (when (pythonic-remote-p)
      (setq filename (concat (pythonic-tramp-connection) filename)))
    (find-file-other-window filename)))

(defvar djangonaut-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "!") 'djangonaut-run-management-command-dwim)
    (define-key map (kbd "i") 'djangonaut-dired-installed-apps)
    (define-key map (kbd "c") 'djangonaut-find-management-command)
    (define-key map (kbd "a") 'djangonaut-find-admin-class)
    (define-key map (kbd "m") 'djangonaut-find-model)
    (define-key map (kbd "M") 'djangonaut-find-model-manager)
    (define-key map (kbd "n") 'djangonaut-find-migration)
    (define-key map (kbd "q") 'djangonaut-find-sql-function)
    (define-key map (kbd "r") 'djangonaut-find-signal-receiver)
    (define-key map (kbd "s") 'djangonaut-find-drf-serializer)
    (define-key map (kbd "p") 'djangonaut-find-drf-permission)
    (define-key map (kbd "v") 'djangonaut-find-view)
    (define-key map (kbd "u") 'djangonaut-find-url-module)
    (define-key map (kbd "t") 'djangonaut-find-template)
    (define-key map (kbd "g") 'djangonaut-find-template-tag)
    (define-key map (kbd "f") 'djangonaut-find-template-filter)
    (define-key map (kbd "j") 'djangonaut-find-static-file)
    (define-key map (kbd "S") 'djangonaut-find-settings-module)
    (define-key map (kbd "4 i") 'djangonaut-dired-installed-apps-other-window)
    (define-key map (kbd "4 c") 'djangonaut-find-management-command-other-window)
    (define-key map (kbd "4 a") 'djangonaut-find-admin-class-other-window)
    (define-key map (kbd "4 m") 'djangonaut-find-model-other-window)
    (define-key map (kbd "4 M") 'djangonaut-find-model-manager-other-window)
    (define-key map (kbd "4 n") 'djangonaut-find-migration-other-window)
    (define-key map (kbd "4 q") 'djangonaut-find-sql-function-other-window)
    (define-key map (kbd "4 r") 'djangonaut-find-signal-receiver-other-window)
    (define-key map (kbd "4 s") 'djangonaut-find-drf-serializer-other-window)
    (define-key map (kbd "4 p") 'djangonaut-find-drf-permission-other-window)
    (define-key map (kbd "4 v") 'djangonaut-find-view-other-window)
    (define-key map (kbd "4 u") 'djangonaut-find-url-module-other-window)
    (define-key map (kbd "4 t") 'djangonaut-find-template-other-window)
    (define-key map (kbd "4 g") 'djangonaut-find-template-tag-other-window)
    (define-key map (kbd "4 f") 'djangonaut-find-template-filter-other-window)
    (define-key map (kbd "4 j") 'djangonaut-find-static-file-other-window)
    (define-key map (kbd "4 S") 'djangonaut-find-settings-module-other-window)
    map))

(defvar djangonaut-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map djangonaut-keymap-prefix djangonaut-command-map)
    map))

(defvar djangonaut-mode-lighter " Django")

;;;###autoload
(define-minor-mode djangonaut-mode
  "Minor mode to interact with Django project.

\\{djangonaut-mode-map}"
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
              (djangonaut-mode 1)))))))
  :require 'djangonaut)

(provide 'djangonaut)

;;; djangonaut.el ends here
