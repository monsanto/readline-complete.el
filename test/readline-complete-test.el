;; -*- lexical-binding: t -*-
(require 'f)
(require 'readline-complete)

;; Probably best to test the behaviour --
;; given a Display all or More dialog, it should return control
;; to the process (so the filter should be the default).
;; Given a completions dialog, it should return candidates, and return
;; control to the process.

(defvar output-file "rlc-test")

(defmacro with-comint (buffer-content process-output &rest body)
  `(progn
     (when (f-exists? output-file)
       (f-delete output-file))
     (f-touch output-file)
     (with-temp-buffer
       (make-comint-in-buffer "test" (current-buffer) "tail" nil "-f" output-file)
       (set-process-query-on-exit-flag
        (get-buffer-process (current-buffer)) nil)
       (insert ,buffer-content)
       (f-write ,process-output 'utf-8 output-file)
       ,@body)
     (f-delete output-file)))

(describe
 "rlc-candidates, given a process buffer"

 (before-each
  (spy-on 'set-process-filter :and-call-through))

 (it
  "redirects buffer output to its filter"
  (with-comint "" ""
               (rlc-candidates)
               (expect 'set-process-filter
                       :to-have-been-called-with
                       (get-buffer-process (current-buffer)) 'rlc-filter)))

 (describe
  "when the buffer contains a term"

  (before-each
   (spy-on 'process-send-string :and-call-through))

  (let ((term "wh"))

    (it
     "sends the term to the buffer's process,
triggering completion, dismissing prompts and deleting the control characters"
     (with-comint
      term ""
      (rlc-candidates)
      (expect 'process-send-string :to-have-been-called-with
              (get-buffer-process (current-buffer))
              (concat term "\e?" "n*" "\C-h\C-h\C-h\C-h"))))

    (describe
     "when the process shows a `Display all' dialog"

     (let ((output (concat term
                           "\n"
                           "Display all 50 possibilities? (y or n)"
                           "\n"
                           term
                           "*\C-h \C-h\C-h \C-h\C-h \C-h")))

       (it
        "returns no candidates, and reinstates the process filter"
        (with-comint
         term output
         (expect (rlc-candidates) :to-equal nil)
         (expect 'set-process-filter :to-have-been-called-with
                 (get-buffer-process (current-buffer))
                 'comint-output-filter)))))

    (describe
     "when the process shows a `More' dialog"

     (let ((output (concat term
                           "\n\n"
                           "--More--\C-m\C-m"
                           term
                           "*"
                           "\C-h \C-h\C-h \C-h\C-h \C-h")))

       (it
        "returns no candidates, and reinstates the process filter"
        (with-comint
         term output
         (expect (rlc-candidates) :to-equal nil)
         (expect 'set-process-filter :to-have-been-called-with
                 (get-buffer-process (current-buffer))
                 'comint-output-filter)))))
    (describe
     "when the process returns completions"

    (let ((output (concat term
                          "\n"
                          "what whatis whereis which"
                          "\n"
                          term
                          "n"
                          "*"
                          "\C-h \C-h\C-h \C-h\C-h \C-h\C-h \C-h")))

      (it "returns candidates, and reinstates the process filter"
          (with-comint
           term output
           (expect (rlc-candidates) :to-equal
                   '("what" "whatis" "whereis" "which"))
           (expect 'set-process-filter :to-have-been-called-with
                 (get-buffer-process (current-buffer))
                 'comint-output-filter))))))))
