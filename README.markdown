# <img align="right" src="pics/django.png" height="150" width="150"> Djangonaut [![Emacs](https://img.shields.io/badge/Emacs-25-8e44bd.svg)](https://www.gnu.org/software/emacs/) [![Build Status](https://travis-ci.org/proofit404/djangonaut.svg?branch=master)](https://travis-ci.org/proofit404/djangonaut)

Emacs minor mode for Django.

This package is based on the introspection of your Django project.  It
executes Python code to get all necessary information about model,
views, and templates inside `INSTALLED_APPS`.  Works the same way for
applications written by you and for third-party code installed with
`pip`.

No more broken code conventions.  No more flaky guesses about your
project layout.  If Django can understand it, Djangonaut can
understand it.

## Features

<div align="center"><img src="pics/find-model-and-view.gif"></div>

Djangonaut mode provides following features

* intelligent navigation in a Django project
* management commands execution
* works transparently with vagrant, docker and remote hosts

## Supported Python Versions

2.7, 3.4, 3.5, 3.6

## Supported Django Versions

1.8, 1.9, 1.10, 1.11, 2.0

## Installation

All you need to do is install the package from
[Melpa](https://melpa.org/)

    M-x package-install RET djangonaut RET

## Configuration

Djangonaut minor mode available in all buffers related to current
django project (python and html files, dired buffers).  First of all
enable minor mode:

    M-x global-djangonaut-mode RET

Now you need to configure Emacs environment to run your django
project.  For example, `run-python` command should be able to run
django shell.  Emacs require to know three things:

* path to the python interpreter or virtual environment
* location of the project
* django settings module of your project

#### Path to the interpreter

Django itself is a python package you usually install with `pip`.
If you install Django into your system globally with `apt` or `yum`
you can skip this section.  But most of times python packages are
installed somewhere else like virtual environment on your host, inside
Docker container or virtual machine orchestrated by Vagrant.

If you setup project inside virtual environment, use this command to
tell Emacs where it can find an interpreter:

    M-x pythonic-activate RET /path/to/your/venv/ RET

If you use Docker or Docker Compose for development, open any of the
project files using tramp.  Optionally you can specify interpreter
location inside container.  Also you need to install
[docker-tramp](https://github.com/emacs-pe/docker-tramp.el) package to
use remote interpreter this way.

    C-x C-f /docker:root@container:/app/config/urls.py
    ;; Optionally...
    M-x set-variable RET python-shell-interpreter RET "/usr/local/bin/python"

If you use Vagrant for development, first of all add your ssh key to
the trusted list in your VM, so you will not be annoyed with password
prompt pops up frequently.  Then open any project file in the running
virtual machine.

    ;; SSH config (done once).
    ssh-copy-id vagrant@localhost -p 2222
    ;; Open project file (each time at the begging).
    C-x C-f /ssh:vagrant@localhost#2222:/app/config/urls.py

Optionally point Emacs to the remote interpreter this way

    M-x set-variable RET python-shell-interpreter RET "/usr/bin/python" RET

#### Path to the project

The key point here - provided python interpreter should be able to
import modules from your project.  There is a lot of options here.
You can use [setuptools](https://setuptools.readthedocs.io/en/latest/)
to wrap your project into proper python package and install it into
editable mode

    pip install -e .

You can use `pth` file to include project location into interpreter
import path

    echo $PWD > venv/lib/python3.7/site-packages/project.pth

Or you can use old plain `PYTHONPATH` environment variable to tell
Emacs where to look for your project

    M-x set-variable RET python-shell-extra-pythonpaths RET '("/path/to/the/project/")

In case you use Docker you can also set this variable directly inside
container either with command line arguments or via
[environment](https://docs.docker.com/compose/compose-file/) key of
the docker compose file

    docker run -e PYTHONPATH=/code/ web django-admin runserver 0.0.0.0:8000

#### Settings module

Also Emacs needs to know your django settings module.  We can provide
it the same way as we do with project path via environment variable

    M-x set-variable RET python-shell-process-environment RET '("DJANGO_SETTINGS_MODULE=project.settings")

And in the case of docker you can setup this variable inside container
directly or with compose file environment key

    docker run -e DJANGO_SETTINGS_MODULE=project.settings web ...

## Usage

If you open file or directory related to the project, you should see
`Django` minor mode is activated for this buffer.  Note, you should
open project files over tramp method, if you use remote interpreter.
For example, open `/docker:root@container:/code/manage.py` instead of
`manage.py` on the local host.

#### Project navigation

| Key                  | Command                            |
|----------------------|------------------------------------|
| <kbd>C-c ' M</kbd>   | djangonaut-find-model-manager      |
| <kbd>C-c ' S</kbd>   | djangonaut-find-settings-module    |
| <kbd>C-c ' a</kbd>   | djangonaut-find-admin-class        |
| <kbd>C-c ' c</kbd>   | djangonaut-find-management-command |
| <kbd>C-c ' f</kbd>   | djangonaut-find-template-filter    |
| <kbd>C-c ' g</kbd>   | djangonaut-find-template-tag       |
| <kbd>C-c ' i</kbd>   | djangonaut-dired-installed-apps    |
| <kbd>C-c ' j</kbd>   | djangonaut-find-static-file        |
| <kbd>C-c ' m</kbd>   | djangonaut-find-model              |
| <kbd>C-c ' n</kbd>   | djangonaut-find-migration          |
| <kbd>C-c ' p</kbd>   | djangonaut-find-drf-permission     |
| <kbd>C-c ' q</kbd>   | djangonaut-find-sql-function       |
| <kbd>C-c ' r</kbd>   | djangonaut-find-signal-receiver    |
| <kbd>C-c ' s</kbd>   | djangonaut-find-drf-serializer     |
| <kbd>C-c ' t</kbd>   | djangonaut-find-template           |
| <kbd>C-c ' u</kbd>   | djangonaut-find-url-module         |
| <kbd>C-c ' v</kbd>   | djangonaut-find-view               |
| <kbd>C-c ' d</kbd>   | djangonaut-find-middleware         |

All navigation commands can open definitions in the other window.  For
example use <kbd>C-c ' 4 m</kbd> to open model definition in the other
window.

#### Run management commands

Use <kbd>C-c ' !</kbd> to run management command in the comint buffer.
You can call it with prefix argument `C-u` to set command arguments
via interactive menu.

<div align="center"><img src="pics/run-management-command-popup.gif"></div>
