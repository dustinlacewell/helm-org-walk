
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

(defun org-olp--matches-in-buffer (regexp &optional buffer)
  "Return a list of matches of REGEXP in BUFFER or the current buffer if not given."
  (let ((matches))
    (save-match-data
      (save-excursion
        (with-current-buffer (or buffer (current-buffer))
          (save-restriction
            (widen)
            (goto-char 1)
            (while (search-forward-regexp regexp nil t 1)
              (push (match-string 1) matches)))))
      matches)))

(defun org-olp--subheadings-at-point (&optional recursive)
  "Return a list of subheadings.
If RECURSIVE is truthy return a list of all headings in
subheading subtrees."
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

(defun org-olp--olp-subheadings (olp)
  (goto-char (org-find-olp olp 't))
  (org-olp--subheadings-at-point))

(defun org-olp--file-olp-subheadings (file-name olp)
  (with-temp-buffer
    (insert-file-contents (expand-file-name file-name))
    (org-mode)
    (org-olp--olp-subheadings olp)))

(defun org-olp--pick-olp (file-name children olp)
  (with-temp-buffer
      (insert-file-contents file-name)
      (org-mode)
      (let* ((selection (completing-read "Select header:" children))
             (olp (append olp (list selection)))
             (children (org-olp--olp-subheadings olp)))
        (if children
            (org-olp--pick-olp file-name children olp)
          olp))))

(defun org-olp--goto-end ()
  "Either go to the end of line or to the end of the content for that element"
  (let ((cend (org-element-property :contents-end (org-element-at-point))))
    (goto-char (if cend cend (point-at-eol)))
    ))

(defun org-olp-visit (file-name olp)
  "Visit the heading in FILE-NAME denoted by OLP"
  (find-file (expand-file-name file-name))
  (org-set-startup-visibility)
  (org-cycle '(64))
  (goto-char (org-find-olp olp t))
  (org-cycle '(4))
  (call-interactively 'org-cycle)
  (call-interactively 'recenter-top-bottom))

(defun org-olp-select (file olp)
  "Select and return olp of child of heading pointed to by OLP in FILE-NAME"
  (let* ((file-name (expand-file-name file))
         (candidates (org-olp--file-olp-subheadings file olp))
         (prompt (s-join " " olp))
         (selection (completing-read prompt candidates)))
    (append olp (list selection))))

(defun org-olp-select-then-visit (file-name olp)
  "Run org-olp-select then visit the resulting olp in FILE-NAME"
  (let ((selected-olp (org-olp-select file-name olp)))
    (org-olp-visit file-name selected-olp)))

(defun org-olp-recursive-select (file-name &rest olp)
  "Select headings from FILE-NAME, from OLP or top-level, until
     a heading with no children is reached. The resulting olp is
     returned."
  (let* ((file-name (expand-file-name file-name)))
    (with-temp-buffer
      (insert-file-contents file-name)
      (org-mode)
      (if olp
          (let ((children (org-olp--olp-subheadings olp)))
            (org-olp--pick-olp file-name children olp))
        (let* ((top-headers (org-olp--matches-in-buffer "^\\*[ ]+\\(.+\\)$"))
               (first-header (completing-read "Select header:" top-headers))
               (olp (list first-header))
               (children (org-olp--olp-subheadings olp)))
          (org-olp--pick-olp file-name children olp))))))

(defun org-olp-find (file-name &rest olp)
  "Run org-olp-recursive-select on FILE-NAME, starting from OLP
or top-level, then visit the selected heading."
  (let ((file-name (expand-file-name file-name))
        (olp (apply 'org-olp-recursive-select file-name olp)))
    (org-olp-visit file-name olp)))

(defun org-olp-refile (file-name olp-src olp-dst)
  "This function takes a filename and two olp paths it uses the
org-element api to remove the heading specified by the first olp and
then inserts the element *under* the heading pointed to by the second olp
"

  (progn
    (org-olp-visit file-name olp-src)
    (let ((src-level (org-element-property :level (org-element-at-point))))
      (org-cut-subtree)
      (org-olp-visit file-name olp-dst)
      (let ((dst-level (org-element-property :level (org-element-at-point)))
            (dst-contents-end (org-element-property :contents-end (org-element-at-point)))
            )
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

(provide 'org-olp)
