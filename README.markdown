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

If you use Docker or Docker Compose for development, setup path to the
interpreter directly.  Also you need to install
[docker-tramp](https://github.com/emacs-pe/docker-tramp.el) package to
use remote interpreter this way.

    M-x set-variable RET python-shell-interpreter RET "/docker:root@container:/usr/local/bin/python"

If you use Vagrant for development, first of all add your ssh key to
the trusted list in your VM, so you will not be annoyed with password
prompt pops up frequently.

    ssh-copy-id vagrant@localhost -p 2222

Now you can point Emacs to the remote interpreter this way

    M-x set-variable RET python-shell-interpreter RET "/ssh:vagrant@localhost#2222:/usr/bin/python" RET

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

#### Run management command

![Screencast of run management command](pics/run-management-command.gif)

![Screencast of run management command popup](pics/run-management-command-popup.gif)

#### Dired installed apps

![Screencast of dired installed apps command](pics/dired-installed-apps.gif)

#### Find management command

![Screencast of find management command command](pics/find-management-command.gif)

#### Find model

![Screencast of find model command](pics/find-model.gif)

#### Find model manager

![Screencast of find model manager command](pics/find-model-manager.gif)

#### Find migration

![Screencast of find migration command](pics/find-migration.gif)

#### Find sql function

![Screencast of find sql function command](pics/find-sql-function.gif)

#### Find signal receiver

![Screencast of find signal receiver command](pics/find-signal-receiver.gif)

#### Find view

![Screencast of find view command](pics/find-view.gif)

#### Find url module

![Screencast of find url module command](pics/find-url-module.gif)

#### Find template

![Screencast of find template command](pics/find-template.gif)

#### Find template tag

![Screencast of find template tag command](pics/find-template-tag.gif)

#### Find template filter

![Screencast of find template filter command](pics/find-template-filter.gif)

#### Find static file

![Screencast of find static file command](pics/find-static-file.gif)

#### Find settings module

![Screencast of find settings module command](pics/find-settings-module.gif)
