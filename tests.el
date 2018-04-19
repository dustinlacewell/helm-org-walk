;;
;; These tests can be run like so:
;;
;; emacs -batch -l ert -l tests.el -f ert-run-tests-batch-and-exit
;;



;; Add the current path to load paths so we can load org-olp from here
(add-to-list 'load-path ".")

;;;; Use a custom version of org-mode
;; (add-to-list 'load-path "~/sources/olp/org-mode/lisp")

(require 'ert)
(require 'org)
(require 'org-olp)

;; Print the org-mode version
(message (concat "[DEBUG] Org version => " (prin1-to-string org-version)))

;; Test if an element is moved correctly under a same-level parent heading
;; as the original one.
(ert-deftest refiling-test-same-level ()
  (progn
    (interactive)
    (let ((input "* a1
** a12
content12
** a13
content14
* a2
")
          (expected "* a1
** a13
content14
* a2
** a12
content12

"))
      ;; write input data on disk
      (with-temp-buffer (insert input) (write-file "/tmp/f1.org" nil))
      ;; run our refiling routine
      (org-olp-refile "/tmp/f1.org" (list "a1" "a12") (list "a2"))
      ;; save modified org file to disk in the same location
      (save-buffer)
      
      ;; check to see if we got the expected result
      (let ((output
             (with-temp-buffer
               (insert-file-contents "/tmp/f1.org")
               (buffer-string)
               )))
        (should (equal output expected)))
      )))



