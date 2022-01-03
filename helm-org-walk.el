;;; helm-org-walk.el --- Helpful olp functions

;; Author: Dustin Lacewell <dlacewell@gmail.com>
;; Version: 0.1.0
;; Keywords: org-mode olp
;; Package-Requires: ((emacs "24") (helm "0") (org "0"))
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

;; Helpful olp commands
;;
;; See documentation at https://github.com/dustinlacewell/helm-org-walk

;;; Code:
(require 'cl-lib)
(require 'helm)
(require 'org)

(defmacro helm-org-walk--with-buffer (file-name &rest body)
  "Open a temporary buffer with the contents of FILE-NAME and
execute BODY forms."
  (declare (indent defun))
  `(save-excursion
     (if ,file-name
         (let ((full-path (expand-file-name ,file-name)))
           (with-temp-buffer
             (insert-file-contents full-path)
             (org-mode)
             ,@body))
       (progn ,@body))))

(cl-defun helm-org-walk--matches (file-name regexp &key (which 0))
  "Return a list of matches of REGEXP in FILE-NAME or the current buffer if nil."
  (let ((matches))
    (save-match-data
      (save-excursion
        (helm-org-walk--with-buffer file-name
                              (save-restriction
                                (widen)
                                (goto-char 1)
                                (while (search-forward-regexp regexp nil t 1)
                                  (push (match-string which) matches)))))
      (reverse matches))))

(defun helm-org-walk--top-level-headings (file-name)
  "Return top-level headings in FILE-NAME."
  (helm-org-walk--matches file-name "^\\*[ ]+\\(.+\\)$" :which 1))

(defun helm-org-walk--subheadings-at-point (&optional recursive)
  "Return a list of subheadings. If RECURSIVE, return a list of
   all headings in subheading subtrees."
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

(defun helm-org-walk--subheadings (file-name olp &optional recursive)
  "Return subheadings of OLP in FILE-NAME, recursing if RECURSIVE."
  (helm-org-walk--with-buffer file-name
                        (goto-char (org-find-olp olp 't))
                        (helm-org-walk--subheadings-at-point recursive)))

(defun helm-org-walk--goto-end ()
  "Either go to the end of line or to the end of the content for that element"
  (let ((cend (org-element-property :contents-end (org-element-at-point))))
    (goto-char (if cend cend (point-at-eol)))
    ))

(cl-defun helm-org-walk--select-next-action ((path pick))
  (let* ((full-path (f-join org-directory (eval `(apply 'f-join (list ,@path ,pick))))))
    (if (f-exists? full-path)
        (if (f-directory? full-path)
            (helm-org-walk--select-file (append path (list pick)))
          full-path)
      full-path)))

(cl-defun helm-org-walk--select-previous-action ((path pick))
  (let ((path (butlast path)))
    (helm-org-walk--select-file path)))

(defun helm-org-walk--select-abort-action (_)
  (setq helm-input nil))

(cl-defun helm-org-walk--select-open-action ((path pick))
  (find-file (concat org-directory "/" (car path) "/" pick))
  nil)

(setq helm-org-walk--select-actions
      '(("Select" . helm-org-walk--select-next-action)
        ("Previous" . helm-org-walk--select-previous-action)
        ("Open" . helm-org-walk--select-open-action)
        ("Abort" . helm-org-walk--select-abort-action)))

(defun helm-org-walk--select-next ()
  (interactive
   (helm-exit-and-execute-action 'helm-org-walk--select-next-action)))

(defun helm-org-walk--select-previous ()
  (interactive
   (helm-exit-and-execute-action 'helm-org-walk--select-previous-action)))

(defun helm-org-walk--select-abort ()
  (interactive)
  (helm-exit-and-execute-action 'helm-org-walk--select-abort-action))

(defun helm-org-walk--select-open ()
  (interactive)
  (helm-exit-and-execute-action 'helm-org-walk--select-open-action))

(setq helm-org-walk-select-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-<backspace>") 'helm-org-walk--select-previous)
    (define-key map (kbd "C-<return>") 'helm-org-walk--select-open)
    (define-key map (kbd "C-g") 'helm-org-walk--select-abort)
    map))

(defun helm-org-walk--select-file (&optional start-path)
  (interactive)
  (let* ((root-path (apply 'f-join org-directory start-path))
         (paths (f-glob "*" root-path))
         (directories (--filter (and (f-directory? it)
                                     (not (s-starts-with? "." (f-base it))))
                                paths))
         (directory-candidates (--map (cons (concat (f-base it) "/")
                                            (list start-path (f-base it)))
                                      directories))
         (files (-filter 'f-file? paths))
         (file-candidates (--map (cons (f-filename it)
                                       (list start-path (f-filename it)))
                                 files))
         (candidates (append directory-candidates file-candidates))
         (sources (helm-build-sync-source root-path
                    :candidates candidates
                    :action helm-org-walk--select-actions
                    :keymap helm-org-walk-select-map)))
    (or (helm :sources sources) (when helm-input (f-join root-path helm-input)))))

(cl-defun helm-org-walk--pick-next-action ((file-name olp pick))
  (helm-org-walk-pick file-name `(,@olp ,pick)))

(cl-defun helm-org-walk--pick-previous-action ((file-name olp pick))
  (if olp
      (helm-org-walk-pick file-name (butlast olp))
    (if file-name
        (-when-let (selected-file (helm-org-walk--select-file
                                   (f-split (f-dirname file-name))))
          (helm-org-walk selected-file))
      (helm-org-walk-pick file-name))))

(cl-defun helm-org-walk--pick-visit-action ((file-name olp pick))
  `(,@olp ,pick))

(defun helm-org-walk--pick-abort-action (_) nil)

(defvar helm-org-walk-helm-actions
  '(("Select" . helm-org-walk--pick-next-action)
    ("Previous" . helm-org-walk--pick-previous-action)
    ("Visit" . helm-org-walk--pick-visit-action)
    ("Abort" . helm-org-walk--pick-abort-action)))

(defun helm-org-walk--next-pick ()
  (interactive)
  (helm-exit-and-execute-action 'helm-org-walk--pick-next-action))

(defun helm-org-walk--previous-pick ()
  (interactive)
  (helm-exit-and-execute-action 'helm-org-walk--pick-previous-action))

(defun helm-org-walk--pick-visit ()
  (interactive)
  (helm-exit-and-execute-action 'helm-org-walk--pick-visit-action))

(defun helm-org-walk--pick-abort ()
  (interactive)
  (helm-exit-and-execute-action 'helm-org-walk--pick-abort-action))

(defun helm-org-walk--pick-go (file-name olp)
  (-when-let (olp (helm-org-walk-pick file-name olp))
    (helm-org-walk-visit file-name olp)
    (beginning-of-line)
    (org-reveal)
    (org-show-entry)))

(setq helm-org-walk-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-<backspace>") 'helm-org-walk--previous-pick)
    (define-key map (kbd "C-<return>") 'helm-org-walk--pick-visit)
    (define-key map (kbd "C-g") 'helm-org-walk--pick-abort)
    map))

(defun helm-org-walk-pick (file-name &optional olp)
  "Use helm to pick headings from FILE-NAME, starting at OLP, to form a new olp path."
  (helm-org-walk--with-buffer file-name
    (-let* ((children (if olp (helm-org-walk--subheadings file-name olp)
                        (helm-org-walk--top-level-headings file-name))))
      (if (not children) olp
        (-let* ((candidates (--map (cons it `(,file-name ,olp ,it)) children))
                (actions helm-org-walk-helm-actions)
                (sources (helm-build-sync-source (s-join "/" olp)
                           :keymap helm-org-walk-map
                           :candidates candidates
                           :action actions)))
          (helm :sources sources))))))

(cl-defun helm-org-walk-visit (file-name olp)
  "Visit the heading in FILE-NAME denoted by OLP"
  (let ((marker (if file-name
                    (org-find-olp `(,file-name ,@olp))
                  (org-find-olp olp t))))
    (switch-to-buffer (marker-buffer marker))
    (goto-char marker)
    (call-interactively 'recenter-top-bottom)))

(defun helm-org-walk-refile (src-file-name olp-src dst-file-name olp-dst)
  "This function takes a filename and two olp paths it uses the
org-element api to remove the heading specified by the first olp and
then inserts the element *under* the heading pointed to by the second olp
"

  (helm-org-walk-visit src-file-name olp-src)
  (let ((src-level (org-element-property :level (org-element-at-point))))
    (org-cut-subtree)
    (helm-org-walk-visit dst-file-name olp-dst)
    (outline-show-all)
    (let ((dst-level (org-element-property :level (org-element-at-point)))
          (dst-contents-end (org-element-property :contents-end (org-element-at-point))))
      (cond ((= src-level (+ dst-level 1)) (progn
                                             (helm-org-walk--goto-end)
                                             (org-paste-subtree (+ dst-level 1))))
            ((> src-level (+ dst-level 1)) (progn
                                             (helm-org-walk--goto-end)
                                             (org-paste-subtree (+ dst-level 1))))
            ((< src-level (+ dst-level 1)) (progn
                                             (helm-org-walk--goto-end)
                                             (org-paste-subtree (+ dst-level 1))))))
    (org-content 1)
    (setq current-prefix-arg '(8))
    (org-reveal t)
    (call-interactively 'org-cycle)))

(cl-defun helm-org-walk (file-name &optional olp)
    "Run helm-org-walk-recursive-select on FILE-NAME, starting from OLP
  or top-level, then visit the selected heading. Create selected
  file if it does not exist."
    (interactive "P")
    (let* ((is-org-buffer (and (not file-name) (eq 'org-mode major-mode)))
           (is-prefixed (and file-name (listp file-name))))
      (if is-org-buffer
          (helm-org-walk--pick-go file-name olp)
        (if is-prefixed
            (-when-let (file-name (helm-org-walk--select-file) helm-input)
              (when (not (file-directory-p file-name))
                (helm-org-walk--pick-go file-name olp)))
          (when file-name
            (helm-org-walk--pick-go file-name olp))))))

(defun helm-org-walk-refile-this (arg)
  (interactive "P")
  (let* ((src-file-name nil)
         (src-olp (org-get-outline-path t t))
         (dst-file-name (if (and arg (listp arg))
                            (helm-org-walk--select-agenda-file)
                          src-file-name))
         (dst-olp (helm-org-walk-pick dst-file-name)))
    (helm-org-walk-refile src-file-name src-olp dst-file-name dst-olp)))

(provide 'helm-org-walk)
