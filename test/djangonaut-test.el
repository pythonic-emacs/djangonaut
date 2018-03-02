;;; djangonaut-test.el --- Tests for djangonaut

;;; Commentary:

;;; Code:

(require 'ert)
(require 'djangonaut)

(dolist (args '((djangonaut-get-pythonpath)
                (djangonaut-get-project-root)
                (djangonaut-get-commands)
                (djangonaut-get-command-definitions)
                (djangonaut-get-command-arguments "startapp")
                (djangonaut-get-app-paths)
                (djangonaut-get-admin-classes)
                (djangonaut-get-models)
                (djangonaut-get-model-managers)
                (djangonaut-get-migrations)
                (djangonaut-get-sql-functions)
                (djangonaut-get-signal-receivers)
                (djangonaut-get-drf-serializers)
                (djangonaut-get-drf-permissions)
                (djangonaut-get-views)
                (djangonaut-get-url-modules)
                (djangonaut-get-templates)
                (djangonaut-get-template-tags)
                (djangonaut-get-template-filters)
                (djangonaut-get-static-files)
                (djangonaut-get-settings-path)))
  (let* ((name (format "test-%s" (car args)))
         (symbol (intern name)))
    (eval
     `(ert-deftest ,symbol ()
        (condition-case err
            (should-not (null (,@args)))
          (error
           (with-current-buffer "*Django*"
             (message (buffer-substring-no-properties (point-min) (point-max))))
           (signal (car err) (cdr err))))))))

;;; djangonaut-test.el ends here
