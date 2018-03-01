;;; test-helper.el --- Helpers for djangonaut-test.el

;;; Commentary:

;;; Code:

(require 'cask)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory))

;;; test-helper.el ends here
