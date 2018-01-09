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
(require 'dash)
(require 's)

(defvar djangonaut-get-commands-code "
from __future__ import print_function
from django.core.management import get_commands
print('\\n'.join(get_commands().keys()))
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
    (define-key map (kbd "C-c r") 'djangonaut-hydra/body)
    map))

(defvar djangonaut-mode-lighter " Djangonaut")

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
