;;; org-olp.el --- Helpful olp functions

;; Author: Dustin Lacewell <dlacewell@gmail.com>
;; Version: 0.1.0
;; Keywords: org-mode olp

;; This is free and unencumbered software released into the public domain.

;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.

;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;; For more information, please refer to <http://unlicense.org>

;;; Commentary:

;; Helpful olp functions
;;
;; See documentation at https://github.com/dustinlacewell/org-olp#functions

;;; Code:

(cl-defun org-olp--matches-in-buffer (regexp &key buffer (which 0))
  "Return a list of matches of REGEXP in BUFFER or the current buffer if not given."
  (let ((matches))
    (save-match-data
      (save-excursion
        (with-current-buffer (or buffer (current-buffer))
          (save-restriction
            (widen)
            (goto-char 1)
            (while (search-forward-regexp regexp nil t 1)
              (push (match-string which) matches)))))
      (reverse matches))))

(defun org-olp--top-level-heading ()
  (org-olp--matches-in-buffer "^\\*[ ]+\\(.+\\)$" :which 1))

(defun org-olp--pick-top-level-heading ()
  `(,(completing-read "Select heading" (org-olp--top-level-heading))))

(defun org-olp--subheadings-at-point (&optional recursive)
  "Return a list of subheadings. If RECURSIVE is truthy return a
   list of all headings in subheading subtrees."
  (org-save-outline-visibility t
    (save-excursion
      (let ((pred (lambda () (org-entry-get nil "ITEM"))))
        (if recursive
            (org-map-entries pred nil 'tree)
          (progn
            (org-back-to-heading t)
            (org-show-subtree)
            (if (org-goto-first-child)
                (cl-loop collect (funcall pred)
                         until (let ((pos (point)))
                                 (null (org-forward-heading-same-level nil t))
                                 (eq pos (point)))))))))))

(defun org-olp--olp-subheadings (olp &optional recursive)
  (goto-char (org-find-olp olp 't))
  (org-olp--subheadings-at-point recursive))

(defun org-olp--file-olp-subheadings (file-name olp &optional recursive)
  (with-temp-buffer
    (insert-file-contents (expand-file-name file-name))
    (org-mode)
    (org-olp--olp-subheadings olp recursive)))

(defun org-olp--goto-end ()
  "Either go to the end of line or to the end of the content for that element"
  (let ((cend (org-element-property :contents-end (org-element-at-point))))
    (goto-char (if cend cend (point-at-eol)))
    ))

(defun org-olp--run-pick (&optional olp children)
  (-let* ((olp (if olp olp (org-olp--pick-top-level-heading)))
            (children (if children children (org-olp--olp-subheadings olp)))
            (actions `(("Default" . (lambda (c) `(nil ,c)))
                       ("Visit" . (lambda (c) `(t ,c)))
                       ("Kill" . (lambda (c) '(t nil)))))
            (sources (helm-build-sync-source "Select heading"
                       :candidates children
                       :action actions))

            ((abort selection) (helm :sources sources))
            (olp (if selection (append olp (list selection)) olp))
            (children (if selection (org-olp--olp-subheadings olp) children)))

      (if (and children (not abort))
          (org-olp--run-pick olp children)
        olp)))

(cl-defun org-olp--pick-olp (&key file-name olp children)
  (if file-name
      (with-temp-buffer
        (insert-file-contents file-name)
        (org-mode)
        (org-olp--run-pick olp children))
    (org-olp--run-pick olp children)))

;; (org-olp--pick-olp :file-name "/home/ldlework/org/notes.org")

(defun org-olp--select-agenda-file (&optional prompt)
  (completing-read (or prompt "Select file: ") org-agenda-files))

(defun org-olp--resolve-file-name (arg file-name)
  (if file-name file-name
    (if (and arg (listp arg))
        (org-olp--select-agenda-file)
      nil)))

(cl-defun org-olp-visit (olp &optional file-name)
  "Visit the heading in FILE-NAME denoted by OLP"
  (let ((marker (if file-name
                    (org-find-olp `(,file-name ,@olp))
                  (org-find-olp olp t))))
    (switch-to-buffer (marker-buffer marker))
    (goto-char marker)
    (call-interactively 'recenter-top-bottom)))

(cl-defun org-olp-select (&key file-name olp)
  "Select headings from FILE-NAME, from OLP or top-level, until
     a heading with no children is reached. The resulting olp is
     returned."
  (org-olp--pick-olp :file-name file-name :olp olp))

(defun org-olp-refile (src-file-name olp-src dst-file-name olp-dst)
  "This function takes a filename and two olp paths it uses the
org-element api to remove the heading specified by the first olp and
then inserts the element *under* the heading pointed to by the second olp
"

  (progn
    (org-olp-visit olp-src src-file-name)
    (let ((src-level (org-element-property :level (org-element-at-point))))
      (org-cut-subtree)
      (org-olp-visit olp-dst dst-file-name)
      (let ((dst-level (org-element-property :level (org-element-at-point)))
            (dst-contents-end (org-element-property :contents-end (org-element-at-point))))
        (cond ((= src-level (+ dst-level 1)) (progn
                                               (org-olp--goto-end)
                                               (insert "\n")
                                               (org-paste-subtree (+ dst-level 1))
                                               ))
              ((> src-level (+ dst-level 1)) (progn
                                               (org-olp--goto-end)
                                               (insert "\n")
                                               (org-paste-subtree (+ dst-level 1))
                                               ))
              ((< src-level (+ dst-level 1)) (progn
                                               (org-olp--goto-end)
                                               (insert "\n")
                                               (org-paste-subtree (+ dst-level 1))
                                               ))
              )
        ))
    ))

(cl-defun org-olp-find (arg &key file-name olp)
  "Run org-olp-recursive-select on FILE-NAME, starting from OLP
or top-level, then visit the selected heading."
  (interactive "P")
  (let* ((file-name (org-olp--resolve-file-name arg file-name))
         (olp (org-olp-select :file-name file-name :olp olp)))
    (org-olp-visit olp file-name)))

(defun org-olp-at-point ()
  (interactive)
  (org-get-outline-path t t))

(defun org-olp-refile-this (arg)
  (interactive "P")
  (let* ((src-file-name (buffer-file-name))
         (src-olp (org-olp-at-point))
         (dst-file-name (if (and arg (listp arg))
                            (org-olp--select-agenda-file)
                          src-file-name))
         (dst-olp (org-olp-select :file-name dst-file-name)))
    (org-olp-refile src-file-name src-olp dst-file-name dst-olp)))

(provide 'org-olp)
