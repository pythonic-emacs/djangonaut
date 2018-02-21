;;; init.el --- minimal djangonaut configuration

;;; Commentary:

;;; Code:

(require 'cask)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory)
  (setq gif-screencast-output-directory (f-join source-directory "pics")))

(require 'djangonaut)

(global-djangonaut-mode)

;;; init.el ends here
