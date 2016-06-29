(require 'noflet)

(let ((current-directory (file-name-directory load-file-name)))
  (setq rlc-test-test-path (f-expand "." current-directory)
        rlc-test-root-path (f-expand ".." current-directory)))

(add-to-list 'load-path rlc-test-root-path)

(if (featurep 'readline-complete)
    (unload-feature 'readline-complete t))

(require 'readline-complete)
