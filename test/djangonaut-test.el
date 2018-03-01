;;; djangonaut-test.el --- Tests for djangonaut

;;; Commentary:

;;; Code:

(require 'ert)
(require 'djangonaut)

(ert-deftest test-djangonaut-get-pythonpath ()
  (condition-case err
      (should-not (null (djangonaut-get-pythonpath)))
    (error
     (with-current-buffer "*Django*"
       (message (buffer-substring-no-properties (point-min) (point-max))))
     (signal (car err) (cdr err)))))

(ert-deftest test-djangonaut-get-project-root ()
  (condition-case err
      (should-not (null (djangonaut-get-project-root)))
    (error
     (with-current-buffer "*Django*"
       (message (buffer-substring-no-properties (point-min) (point-max))))
     (signal (car err) (cdr err)))))

(ert-deftest test-djangonaut-get-commands ()
  (condition-case err
      (should-not (null (djangonaut-get-commands)))
    (error
     (with-current-buffer "*Django*"
       (message (buffer-substring-no-properties (point-min) (point-max))))
     (signal (car err) (cdr err)))))

(ert-deftest test-djangonaut-get-command-definitions ()
  (condition-case err
      (should-not (null (djangonaut-get-command-definitions)))
    (error
     (with-current-buffer "*Django*"
       (message (buffer-substring-no-properties (point-min) (point-max))))
     (signal (car err) (cdr err)))))

(ert-deftest test-djangonaut-get-command-arguments ()
  (condition-case err
      (should-not (null (djangonaut-get-command-arguments "startapp")))
    (error
     (with-current-buffer "*Django*"
       (message (buffer-substring-no-properties (point-min) (point-max))))
     (signal (car err) (cdr err)))))

(ert-deftest test-djangonaut-get-app-paths ()
  (condition-case err
      (should-not (null (djangonaut-get-app-paths)))
    (error
     (with-current-buffer "*Django*"
       (message (buffer-substring-no-properties (point-min) (point-max))))
     (signal (car err) (cdr err)))))

(ert-deftest test-djangonaut-get-admin-classes ()
  (condition-case err
      (should-not (null (djangonaut-get-admin-classes)))
    (error
     (with-current-buffer "*Django*"
       (message (buffer-substring-no-properties (point-min) (point-max))))
     (signal (car err) (cdr err)))))

(ert-deftest test-djangonaut-get-models ()
  (condition-case err
      (should-not (null (djangonaut-get-models)))
    (error
     (with-current-buffer "*Django*"
       (message (buffer-substring-no-properties (point-min) (point-max))))
     (signal (car err) (cdr err)))))

(ert-deftest test-djangonaut-get-model-managers ()
  (condition-case err
      (should-not (null (djangonaut-get-model-managers)))
    (error
     (with-current-buffer "*Django*"
       (message (buffer-substring-no-properties (point-min) (point-max))))
     (signal (car err) (cdr err)))))

(ert-deftest test-djangonaut-get-migrations ()
  (condition-case err
      (should-not (null (djangonaut-get-migrations)))
    (error
     (with-current-buffer "*Django*"
       (message (buffer-substring-no-properties (point-min) (point-max))))
     (signal (car err) (cdr err)))))

(ert-deftest test-djangonaut-get-sql-functions ()
  (condition-case err
      (should-not (null (djangonaut-get-sql-functions)))
    (error
     (with-current-buffer "*Django*"
       (message (buffer-substring-no-properties (point-min) (point-max))))
     (signal (car err) (cdr err)))))

(ert-deftest test-djangonaut-get-signal-receivers ()
  (condition-case err
      (should-not (null (djangonaut-get-signal-receivers)))
    (error
     (with-current-buffer "*Django*"
       (message (buffer-substring-no-properties (point-min) (point-max))))
     (signal (car err) (cdr err)))))

(ert-deftest test-djangonaut-get-drf-serializers ()
  (condition-case err
      (should-not (null (djangonaut-get-drf-serializers)))
    (error
     (with-current-buffer "*Django*"
       (message (buffer-substring-no-properties (point-min) (point-max))))
     (signal (car err) (cdr err)))))

(ert-deftest test-djangonaut-get-drf-permissions ()
  (condition-case err
      (should-not (null (djangonaut-get-drf-permissions)))
    (error
     (with-current-buffer "*Django*"
       (message (buffer-substring-no-properties (point-min) (point-max))))
     (signal (car err) (cdr err)))))

(ert-deftest test-djangonaut-get-views ()
  (condition-case err
      (should-not (null (djangonaut-get-views)))
    (error
     (with-current-buffer "*Django*"
       (message (buffer-substring-no-properties (point-min) (point-max))))
     (signal (car err) (cdr err)))))

(ert-deftest test-djangonaut-get-url-modules ()
  (condition-case err
      (should-not (null (djangonaut-get-url-modules)))
    (error
     (with-current-buffer "*Django*"
       (message (buffer-substring-no-properties (point-min) (point-max))))
     (signal (car err) (cdr err)))))

(ert-deftest test-djangonaut-get-templates ()
  (condition-case err
      (should-not (null (djangonaut-get-templates)))
    (error
     (with-current-buffer "*Django*"
       (message (buffer-substring-no-properties (point-min) (point-max))))
     (signal (car err) (cdr err)))))

(ert-deftest test-djangonaut-get-template-tags ()
  (condition-case err
      (should-not (null (djangonaut-get-template-tags)))
    (error
     (with-current-buffer "*Django*"
       (message (buffer-substring-no-properties (point-min) (point-max))))
     (signal (car err) (cdr err)))))

(ert-deftest test-djangonaut-get-template-filters ()
  (condition-case err
      (should-not (null (djangonaut-get-template-filters)))
    (error
     (with-current-buffer "*Django*"
       (message (buffer-substring-no-properties (point-min) (point-max))))
     (signal (car err) (cdr err)))))

(ert-deftest test-djangonaut-get-static-files ()
  (condition-case err
      (should-not (null (djangonaut-get-static-files)))
    (error
     (with-current-buffer "*Django*"
       (message (buffer-substring-no-properties (point-min) (point-max))))
     (signal (car err) (cdr err)))))

(ert-deftest test-djangonaut-get-settings-path ()
  (condition-case err
      (should-not (null (djangonaut-get-settings-path)))
    (error
     (with-current-buffer "*Django*"
       (message (buffer-substring-no-properties (point-min) (point-max))))
     (signal (car err) (cdr err)))))

;;; djangonaut-test.el ends here
