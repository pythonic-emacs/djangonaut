;;; djangonaut-test.el --- Tests for djangonaut

;;; Commentary:

;;; Code:

(require 'ert)
(require 'djangonaut)

(ert-deftest test-djangonaut-get-pythonpath ()
  (should-not (null (djangonaut-get-pythonpath))))

(ert-deftest test-djangonaut-get-project-root ()
  (should-not (null (djangonaut-get-project-root))))

(ert-deftest test-djangonaut-get-commands ()
  (should-not (null (djangonaut-get-commands))))

(ert-deftest test-djangonaut-get-command-definitions ()
  (should-not (null (djangonaut-get-command-definitions))))

(ert-deftest test-djangonaut-get-command-arguments ()
  (should-not (null (djangonaut-get-command-arguments))))

(ert-deftest test-djangonaut-get-app-paths ()
  (should-not (null (djangonaut-get-app-paths))))

(ert-deftest test-djangonaut-get-admin-classes ()
  (should-not (null (djangonaut-get-admin-classes))))

(ert-deftest test-djangonaut-get-models ()
  (should-not (null (djangonaut-get-models))))

(ert-deftest test-djangonaut-get-model-managers ()
  (should-not (null (djangonaut-get-model-managers))))

(ert-deftest test-djangonaut-get-migrations ()
  (should-not (null (djangonaut-get-migrations))))

(ert-deftest test-djangonaut-get-sql-functions ()
  (should-not (null (djangonaut-get-sql-functions))))

(ert-deftest test-djangonaut-get-signal-receivers ()
  (should-not (null (djangonaut-get-signal-receivers))))

(ert-deftest test-djangonaut-get-drf-serializers ()
  (should-not (null (djangonaut-get-drf-serializers))))

(ert-deftest test-djangonaut-get-drf-permissions ()
  (should-not (null (djangonaut-get-drf-permissions))))

(ert-deftest test-djangonaut-get-views ()
  (should-not (null (djangonaut-get-views))))

(ert-deftest test-djangonaut-get-url-modules ()
  (should-not (null (djangonaut-get-url-modules))))

(ert-deftest test-djangonaut-get-templates ()
  (should-not (null (djangonaut-get-templates))))

(ert-deftest test-djangonaut-get-template-tags ()
  (should-not (null (djangonaut-get-template-tags))))

(ert-deftest test-djangonaut-get-template-filters ()
  (should-not (null (djangonaut-get-template-filters))))

(ert-deftest test-djangonaut-get-static-files ()
  (should-not (null (djangonaut-get-static-files))))

(ert-deftest test-djangonaut-get-settings-path ()
  (should-not (null (djangonaut-get-settings-path))))

;;; djangonaut-test.el ends here
