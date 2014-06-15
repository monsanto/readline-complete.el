;; -*- lexical-binding: t -*-

(defvar rlc-test-prompt "bash$ ")

(defvar rlc-test-entries
  '(("~/.em" . ".em")
    ("/usr/lo" . "lo")
    ("origin/ma" . "ma")
    ("rebase --ab" . "--ab")
    ("git che" . "che")
    ("--arg=val" . "val")))

(defmacro with-rlc-entry (entry body)
  `(with-temp-buffer
     (insert ,rlc-test-prompt)
     (insert ,entry)
     (let ((end-of-prompt (length rlc-test-prompt)))
       (noflet ((comint-bol () (move-to-column end-of-prompt)))
               ,body))))

(ert-deftest rlc-shell-prefix-test ()
  "Each test entry returns the given prefix."
  (mapc
   (lambda (test-entry)
     (with-rlc-entry (car test-entry)
                     (should (string= (rlc-prefix-chars) (cdr test-entry)))))
   rlc-test-entries))
