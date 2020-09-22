;;
;; These tests can be run like so:
;;
;; emacs -batch -l ert -l tests.el -f ert-run-tests-batch-and-exit
;;
(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage)
  (setq straight-vc-git-default-clone-depth 1))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

(use-package ert)
(use-package org)
(use-package helm)

;; Add the current path to load paths so we can load org-olp from here
(add-to-list 'load-path ".")
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
  (replace-regexp-in-string "^\s*" "" s))

(setq org-content "* a1
content1
content1
content1
** a11
content11
*** a111
content111
*** a112
content112
** a12
content12
*** a121
content121
*** a122
content122
* a2
content2
** a21
content21
*** a211
content211
*** a212
content212
** a22
content22
*** a221
content221
*** a222
content222")

(defmacro with-org-buffer (&rest forms)
  `(with-temp-buffer
     (org-mode)
     (insert org-content)
     (goto-char 1)
     ,@forms))

(defmacro with-org-file (&rest forms)
  `(let ((file-name (concat temporary-file-directory "temp.org")))
     (save-excursion
       (with-temp-buffer
         (insert org-content)
         (write-file file-name))
       (let ((result (progn ,@forms)))
         (kill-this-buffer)
         result))))

(defun test-refile-helper (olp-src olp-dst)
  (with-org-buffer
   ;; run our refiling routine
   (org-olp-refile nil olp-src nil olp-dst)
   ;; modified-contents
   (org-olp--olp-subheadings nil olp-dst)))

(ert-deftest matches--finds-match ()
  (let ((pattern (rx bol "* " (group (one-or-more (or alnum " "))) eol)))
    (with-org-buffer
     (should (equal '("* a1" "* a2") (org-olp--matches nil pattern)))
     (should (equal '("a1" "a2") (org-olp--matches nil pattern :which 1)))
     )))

(ert-deftest subheadings-at-point--returns-subheading ()
  (with-org-buffer
   (should (equal '("a11" "a12") (org-olp--subheadings-at-point)))
   (should (equal '("a1" "a11" "a111" "a112" "a12" "a121" "a122") (org-olp--subheadings-at-point t)))))

(ert-deftest olp-subheadings ()
  (with-org-buffer
   (should (equal '("a211" "a212") (org-olp--olp-subheadings nil '("a2" "a21"))))
   (should (equal '("a2" "a21" "a211" "a212" "a22" "a221" "a222") (org-olp--olp-subheadings nil '("a2") t)))))

(ert-deftest file-olp-subheadings ()
  (with-org-file
   (should (equal '("a11" "a12") (org-olp--olp-subheadings file-name '("a1"))))
   (should (equal '("a1" "a11" "a111" "a112" "a12" "a121" "a122") (org-olp--olp-subheadings file-name '("a1") t)))))

(ert-deftest goto-end ()
  (with-org-buffer
   (should (equal 147 (org-olp--goto-end))))
  (with-org-buffer
   (search-forward "a11")
   (should (equal 90 (org-olp--goto-end)))))

;; (a12 moves under a2 ; same level)
(ert-deftest refiling-test-same-level ()
  (should (equal '("a21" "a22" "a12") (test-refile-helper '("a1" "a12") '("a2")))))

;; (a151 moves under a1 ; higher level)
(ert-deftest refiling-test-higher-level ()
  (should (equal '("a11" "a12" "a122") (test-refile-helper '("a1" "a12" "a122") '("a1")))))

;; (a14 moves under a153 ; lower level)
(ert-deftest refiling-test-lower-level ()
  (should (equal '("a11") (test-refile-helper '("a1" "a11") '("a2" "a22" "a222")))))

