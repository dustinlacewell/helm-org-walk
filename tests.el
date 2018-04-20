;;
;; These tests can be run like so:
;;
;; emacs -batch -l ert -l tests.el -f ert-run-tests-batch-and-exit
;;

;; Add the current path to load paths so we can load org-olp from here
(add-to-list 'load-path ".")
;;;; Use a custom version of org-mode
(add-to-list 'load-path "~/sources/olp/org-mode/lisp")

(require 'ert)
(require 'org)
(require 'org-olp)

;; This is actually in place to make the tests pass
;; even though a bug surfaced in org-mode itself.
;; The bug is present in 9.1.6 but not in 8.2.10
;;
;; Bug description: the org-paste-subtree function inserts leading whitespace
;; into the contents of the subtree. The reasonable expectation here is that
;; org-paste-subtree should only paste the original content extracted by
;; org-cut-subtree and not alter it in any way.
;;
(defun remove-leading-whitespace (s)
  (replace-regexp-in-string "^\s*" "" s)
  )

(defun test-refile-helper (input out-file olp-src olp-dst)
  (with-temp-buffer (insert input)
                    ;; write input data on disk
                    (write-file out-file nil)
                    ;; run our refiling routine
                    (org-olp-refile out-file olp-src olp-dst)
                    ;; save modified org file to disk in the same location
                    (save-buffer)
                    ;; return the modified file contents after refiling
                    (let ((modified-contents
                           (with-temp-buffer
                             (insert-file-contents out-file)
                             (buffer-string)
                             )))
                      ;; modified-contents
                      (remove-leading-whitespace modified-contents)
                      )))

;; Print the org-mode version
(message (concat "[DEBUG] Org version => " (prin1-to-string org-version)))



;; (a12 moves under a2 ; same level)
(ert-deftest refiling-test-same-level ()
  (progn
    (interactive)
    (let* ((input "* a1
** a12
content12
** a13
content14
* a2
a2
")
           (expected "* a1
** a13
content14
* a2
a2

** a12
content12
")
           (output (test-refile-helper input "/tmp/f1.org" (list "a1" "a12") (list "a2")))
           )
      (should (equal output expected))
      )))

;; (a151 moves under a1 ; higher level)
(ert-deftest refiling-test-higher-level ()
  (progn
    (interactive)
    (let* ((input "* a1
** a12
content12
** a13
content13
** a14
content14
** a15
content15
*** a151
content151
*** a152
content152
*** a153
content153
*** a154
content154
")
           (expected "* a1
** a12
content12
** a13
content13
** a14
content14
** a15
content15
*** a152
content152
*** a153
content153
*** a154
content154

** a151
content151
")
           (output (test-refile-helper input "/tmp/f1.org" (list "a1" "a15" "a151") (list "a1")))
           )
      (should (equal output expected))
      )))



;; (a14 moves under a153 ; lower level)
(ert-deftest refiling-test-lower-level ()
  (progn
    (interactive)
    (let* ((input "* a1
** a12
content12
** a13
content13
** a14
content14
** a15
content15
*** a151
content151
*** a152
content152
*** a153
content153
*** a154
content154
")
           (expected "* a1
** a12
content12
** a13
content13
** a15
content15
*** a151
content151
*** a152
content152
*** a153
content153

**** a14
content14
*** a154
content154
")
           (output (test-refile-helper input "/tmp/f1.org" (list "a1" "a14") (list "a1" "a15" "a153")))
           )
      (should (equal output expected))
      )))

