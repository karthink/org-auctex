;;; org-auctex.el --- Generate latex previews in org using AucTeX  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: tex, convenience
;; Version: 0.10
;; Package-Requires: ((emacs "26.1") (auctex "13.1"))
;; Keywords: convenience
;; URL: https://github.com/karthink/popper

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A drop-in replacement for org-mode's latex preview using Auctex. Runs much faster.

;;; Code:

(require 'org)
(require 'preview)
(require 'seq)

;; Plumbing
(defun org-auctex-region-create (file header content trailer master-file begin-offset end-offset)
  "Create region tex file for org-auctex previews.

FILE is the file to use. HEADER, CONTENT and TRAILER are the
latex header, content of the region to be previewed and the
TRAILER text respectively.

MASTER-FILE is the Org file where the previews will be placed.

BEGIN-OFFSET and END-OFFSET are used to locate the preview
overlay positions."
  (let* ((file-buffer (let (;; Don't query for master file
                            (TeX-transient-master t)
                            ;; Don't choose a special mode (and call its hooks)
                            (auto-mode-alist nil)
                            (magic-mode-alist nil)
                            (enable-local-variables nil)
                            ;; Don't run any f-f hooks
                            (find-file-hook nil))
                        (find-file-noselect file t)))
         original-content
         header-offset trailer-offset
         (master-buffer (find-buffer-visiting master-file))
         (master-name (file-name-nondirectory master-file)))

    (with-current-buffer file-buffer
      (setq original-content (buffer-string))
      (setq buffer-undo-list t)
      (erase-buffer)
      (setq buffer-file-coding-system
            (buffer-local-value 'buffer-file-coding-system master-buffer))
      (insert "\\message{ !name(" master-name ")}"
              header
              TeX-region-extra
              "\n\\begin{document}\n"
              "\n\\message{ !name(" master-name ") !offset(")
      (setq header-offset (int-to-string (- begin-offset (1+ (TeX-current-offset)))))
      (insert header-offset ") }\n" content)
      (insert "\n\\message{ !name(" master-name ") !offset(")
      (setq trailer-offset (int-to-string (- end-offset (1+ (TeX-current-offset)))))
      (insert trailer-offset ") }\n" trailer)
      (setq TeX-region-orig-buffer master-buffer)
      (run-hooks 'TeX-region-hook)
      (if (string-equal (buffer-string) original-content)
          (set-buffer-modified-p nil)
        (save-buffer 0)))))

(defun org-auctex-generate-preview (file command)
  "Generate a preview.
FILE the file (without default extension), COMMAND is the command
to use.

It returns the started process."
  (let* ((geometry (preview-get-geometry))
         (commandbuff (current-buffer))
         (pr-file (cons
                   #'TeX-active-master
                   (file-name-nondirectory file)))
         (master (buffer-file-name))
         (master-file (expand-file-name master))
         (dumped-cons (assoc master-file
                             preview-dumped-alist))
         process)
    (unless dumped-cons
      (push (setq dumped-cons (cons master-file
                                    (if (eq preview-auto-cache-preamble 'ask)
                                        (y-or-n-p "Cache preamble? ")
                                      preview-auto-cache-preamble)))
            preview-dumped-alist))
    (when (cdr dumped-cons)
      (let* (TeX-current-process-region-p)
        (setq process (preview-cache-preamble dumped-cons))
        (if process
            ;; FIXME: Use `add-function'.
            (setq TeX-sentinel-function
                  (let ((prev-fun TeX-sentinel-function))
                    (lambda (process string)
                      (funcall prev-fun process string)
                      (TeX-inline-preview-internal
                       command file
                       pr-file commandbuff
                       dumped-cons
                       master
                       geometry
                       (buffer-string))))))))
    (or process
        (TeX-inline-preview-internal command file
                                     pr-file commandbuff
                                     dumped-cons master
                                     geometry))))

(defun org-auctex-sanitize (buf beg end)
  "Sanitize Org markup for Latex compilation.

Text in buffer BUF from BEG to END is sanitized and returned as a
string."
  (with-temp-buffer
    (insert-buffer-substring-no-properties buf beg end)
    (goto-char (point-min))
    (while (search-forward-regexp "^#\\+" nil t)
      (replace-match "% #+"))
    (buffer-string)))

;; Preview Commands
(defun org-auctex-preview-region (beg end)
  "Run preview on region between BEG and END."
  (interactive "r")
  (let* ((TeX-region-extra
         ;; Write out counter information to region.
         (concat (preview--counter-information beg)
                 TeX-region-extra))
        (processing-info (alist-get org-preview-latex-default-process
                                    org-preview-latex-process-alist
                                    nil nil #'equal))
        (latex-header
         (or (plist-get processing-info :latex-header)
	     (org-latex-make-preamble
	      (org-export-get-environment (org-export-get-backend 'latex))
	      org-format-latex-header
	      'snippet)))
        (latex-trailer "\n\\end{document}\n")
        (region-file (TeX-region-file TeX-default-extension))
        (latex-content (org-auctex-sanitize (current-buffer) beg end)))
    (org-auctex-region-create
     region-file
     latex-header
     latex-content
     latex-trailer
     (buffer-file-name)
     (TeX-current-offset beg)
     (TeX-current-offset (save-excursion
                           (save-restriction
                             (widen)
                             (goto-char (point-max))
                             (re-search-backward "[\r\n]" nil t)
                             (point))))))
  (setq TeX-current-process-region-p t)
  (org-auctex-generate-preview (TeX-region-file)
                               (preview-do-replacements
                                (TeX-command-expand
                                 (preview-string-expand preview-LaTeX-command))
                                preview-LaTeX-command-replacements)))

(defun org-auctex-preview-at-point ()
  "What it says on the tin."
  (interactive)
  (if (use-region-p)
      (org-auctex-preview-region (region-beginning) (region-end))
    (let ((datum (org-element-context)))
            (if (memq (org-element-type datum) '(latex-environment latex-fragment))
	        ;; Preview or clearout at point
                (let ((beg (org-element-property :begin datum))
		      (end (org-element-property :end datum)))
	          (if (seq-find (lambda (ov) (overlay-get ov 'preview-state))
                                (overlays-in (or beg (point-min))
                                             (or end (point-max))))
                      (org-auctex-preview-clearout-at-point)
	            (message "Creating LaTeX preview at point")
	            (org-auctex-preview-region beg end))
                  t)
              (message "No latex fragment at point")
              nil))))

(defun org-auctex-preview-section ()
  "What it says on the tin."
  (interactive)
  (let ((beg (if (org-before-first-heading-p) (point-min)
		 (save-excursion
		   (org-with-limited-levels (org-back-to-heading t) (point)))))
	  (end (org-with-limited-levels (org-entry-end-position))))
    (org-auctex-preview-region beg end)))

(defun org-auctex-preview-buffer ()
  "What it says on the tin."
  (interactive)
  (org-auctex-preview-region (point-min) (point-max)))

(defun org-auctex-preview-dwim (&optional arg)
  "Drop-in replacement for `org-latex-preview'.

See its documentation for the behavior of ARG."
  (interactive "P")
  (if (use-region-p)
      (org-auctex-preview-region (region-beginning) (region-end))
    (pcase arg
      ('(64) (org-auctex-preview-clearout-buffer))
      ('(16) (org-auctex-preview-buffer))
      ('(4)  (org-auctex-preview-clearout-section))
      (t  (let ((datum (org-element-context)))
            (unless (org-auctex-preview-at-point)
              ;; Preview current section.
              (let ((beg (if (org-before-first-heading-p) (point-min)
		           (save-excursion
		             (org-with-limited-levels (org-back-to-heading t) (point)))))
	            (end (org-with-limited-levels (org-entry-end-position))))
                (message "Creating LaTeX previews in section...")
                (org-auctex-preview-section))))))))

;; Preview clearout commands

(defalias 'org-auctex-preview-clearout 'preview-clearout)
(defalias 'org-auctex-preview-clearout-at-point 'preview-clearout-at-point)
(defalias 'org-auctex-preview-clearout-buffer 'preview-clearout-buffer)
(defun org-auctex-preview-clearout-section ()
  "What it says on the tin."
  (interactive)
  (preview-clearout 
   (if (org-before-first-heading-p) (point-min)
     (save-excursion
       (org-with-limited-levels (org-back-to-heading t) (point))))
   (org-with-limited-levels (org-entry-end-position))))


(defvar org-auctex-file-extensions nil)

;;;###autoload
(define-minor-mode org-auctex-mode
  "Minor mode to use preview-latex for org-previews."
  :global nil
  :lighter nil
  :keymap '(([remap org-latex-preview] . org-auctex-preview-dwim))
  (if org-auctex-mode
      (progn
        (setq org-auctex-file-extensions TeX-file-extensions)
        (add-to-list 'TeX-file-extensions "org")
        (add-hook 'pre-command-hook 'preview-mark-point nil t)
        (add-hook 'post-command-hook 'preview-move-point nil t))
    (setq TeX-file-extensions org-auctex-file-extensions)
    (remove-hook 'pre-command-hook 'preview-mark-point t)
    (remove-hook 'post-command-hook 'preview-move-point t)))


(provide 'org-auctex)
;;; org-auctex.el ends here
