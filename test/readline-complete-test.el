;; -*- lexical-binding: t -*-
(require 'f)
(require 'readline-complete)

(let ((output-file "rlc-test-output")
      (ps-name "rlc-test-ps")
      (prompt "bash$ "))

  (defmacro with-rlc-env (buffer-content process-output &rest body)
    `(progn
       (when (f-exists? output-file)
         (f-delete output-file))
       (f-touch output-file)
       (with-temp-buffer
         (make-comint-in-buffer
          ps-name (current-buffer) "tail" nil "-f" output-file)
         (set-process-query-on-exit-flag
          (get-buffer-process (current-buffer)) nil)
         (insert ,buffer-content)
         (f-write ,process-output 'utf-8 output-file)
         (let ((candidates (rlc-candidates))
               (ps (get-buffer-process (current-buffer))))
           ,@body))
       (f-delete output-file)))

  (describe
   "rlc-candidates, given a process buffer"

   (before-each
    (spy-on 'set-process-filter :and-call-through))

   (it
    "redirects buffer output to its filter"
    (with-rlc-env
     "" ""
     (expect 'set-process-filter :to-have-been-called-with
             ps 'rlc-filter)))

   (describe
    "when the buffer contains a term"

    (before-each
     (spy-on 'process-send-string :and-call-through))

    (let ((term "wh"))

      (it
       "sends the term to the buffer's process,
triggering completion, dismissing prompts and deleting the control characters"
       (with-rlc-env
        term ""
        (expect 'process-send-string :to-have-been-called-with
                ps (concat term "\e?" "n*" "\C-h\C-h\C-h\C-h"))))

      (describe
       "when the process returns output"

       (before-each
        (spy-on 'string-match :and-call-through))

       (describe
        "when the output is empty"

        (let ((output ""))

          (it
           "doesn't match the output"
           (with-rlc-env
            term output
            (expect (spy-calls-count 'string-match) :to-be rlc-attempts)))

          (it
           "returns no candidates, and reinstates the process filter"
           (with-rlc-env
            term output
            (expect candidates :to-be nil)
            (expect 'set-process-filter :to-have-been-called-with
                    ps 'comint-output-filter)))))

       (describe
        "when the output is a `Display all' dialog"

        (let ((output (concat term
                              "\C-m\n"
                              "Display all 50 possibilities? (y or n)"
                              "\C-m\n"
                              prompt
                              term
                              "*\C-h \C-h\C-h \C-h\C-h \C-h\C-g")))

          (it
           "matches the output"
           (with-rlc-env
            term output
            (expect (spy-calls-count 'string-match) :to-be-less-than
                    rlc-attempts)))

          (it
           "returns no candidates, and reinstates the process filter"
           (with-rlc-env
            term output
            (expect candidates :to-be nil)
            (expect 'set-process-filter :to-have-been-called-with
                    ps 'comint-output-filter)))))

       (describe
        "when the output is a `More' dialog"

        (let ((output (concat term
                              "\C-m\n"
                              "line1\nline2\nline3\n"
                              "--More--\C-m\C-m"
                              prompt
                              term
                              "*\C-h \C-h\C-h \C-h\C-h \C-h\C-g")))
          (it
           "matches the output"
           (with-rlc-env
            term output
            (expect (spy-calls-count 'string-match) :to-be-less-than
                    rlc-attempts)))

          (it
           "returns no candidates, and reinstates the process filter"
           (with-rlc-env
            term output
            (expect candidates :to-be nil)
            (expect 'set-process-filter :to-have-been-called-with
                    ps 'comint-output-filter)))))

       (describe
        "when output contains completions"

        (let ((output (concat term
                              "\C-m\n"
                              "what whatis\nwhereis which\n"
                              prompt
                              term
                              "n*\C-h \C-h\C-h \C-h\C-h \C-h\C-h \C-h")))

          (it "returns candidates, and reinstates the process filter"
              (with-rlc-env
               term output
               (expect candidates :to-equal
                       '("what" "whatis" "whereis" "which"))
               (expect 'set-process-filter :to-have-been-called-with
                       ps 'comint-output-filter))))))))))
