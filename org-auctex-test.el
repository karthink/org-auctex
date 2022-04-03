;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'seq)
(load-file "org-auctex.el")

(ert-deftest org-auctex-test-prereqs ()
  "Test if prerequisites for latex preview are available when using
org-auctex."
  (should (>= emacs-major-version 26))
  (should (locate-library "latex"))
  (should (locate-library "preview"))
  (should (locate-library "org"))
  (should (executable-find "latex")))

(defvar org-auctex-test-doc-text
  "#+title: Test file for Org-Auctex

* Heading 1
This is a test of \\LaTeX preview in org documents.

Some inline math: \\(\\int_a^b \\mathrm{d}\\omega = \\partial_{\\omega} f\\)

And some display math

\\begin{equation}
  \\frac{d \\varphi}{d t} = \\dot{\\phi} = \\ln(t^2)
\\end{equation}

* Heading 2
The probability of an initial subsequence of length \\(n\\) is \\(2^{-n}\\). For example, the probability of an initial subsequence of length \\(3\\).
"
  "Test org text for org-auctex.")

(defvar org-auctex-test-mutex (make-mutex))
(defvar org-auctex-test-proceed-p (make-condition-variable org-auctex-test-mutex))
(defvar org-auctex-test-preview-done nil)
(defvar org-auctex-test-thread nil)

(defun org-auctex-test-wait-for-preview ()
  "Check if preview is done with this buffer so org-auctex's tests
can proceed."
  (while (member "Preview-LaTeX" compilation-in-progress)
    (sleep-for 1))
  (sleep-for 2)
  (with-mutex org-auctex-test-mutex
    (setq org-auctex-test-preview-done t)
    (condition-notify org-auctex-test-proceed-p)))

(ert-deftest org-auctex-test-preview-overlays ()
  "Test if preview overlays are generated and placed correctly when
using `org-auctex-preview-buffer'. "
  (cl-letf* ((oa-test-file (make-temp-file "oa-test" nil ".org"))
             ((default-value 'find-file-hook)
              (remove 'recentf-track-opened-file
                          (default-value 'find-file-hook)))
             (oa-test-buf (find-file-noselect oa-test-file 'nowarn))
             (tmp-dir (temporary-file-directory)))
    (unwind-protect
        (progn 
          (with-current-buffer oa-test-buf
            (insert org-auctex-test-doc-text)
            (save-buffer)
            (org-auctex-mode 1)
            
            (message "Current buffer is %s" (current-buffer))

            (let* ((preview-auto-cache-preamble))
              (org-auctex-preview-buffer))
            
            (setq org-auctex-test-thread
                  (make-thread #'org-auctex-test-wait-for-preview))
            
            (while compilation-in-progress (sleep-for 1))
            
            (with-mutex org-auctex-test-mutex 
              (while (not org-auctex-test-preview-done)
                (condition-wait org-auctex-test-proceed-p))))
          
          (with-current-buffer oa-test-buf
            (let ((previews
                   (seq-difference 
                    (overlays-in (point-min) (point-max))
                    (overlays-in (point-min) (point-min)))))
              (message "%s" previews)
              (should (= (length previews) 5))
              (should (seq-set-equal-p
                       (mapcar
                        (lambda (ov) (cons (overlay-start ov) (overlay-end ov)))
                        previews)
                       '((425 . 430)
                         (348 . 358)
                         (339 . 344)
                         (194 . 273)
                         (118 . 169))))
              (dolist (ov previews)
                (should (eq (overlay-get ov 'category)
                            'preview-overlay))
                (should (file-exists-p
                         (nth 1 (overlay-get ov 'preview-image))))))))
      ;; Unwind forms
      (kill-buffer oa-test-buf)
      (delete-file oa-test-file)
      (dolist (f '("_region_.log" "_region_.tex" "_region_.synctex.gz" "_region_.pdf"))
        (delete-file (expand-file-name f tmp-dir)))
      (let ((prv-dir (file-name-as-directory
                      (expand-file-name "_region_.prv" tmp-dir))))
        (if (file-directory-p prv-dir) (delete-directory prv-dir t))))))
