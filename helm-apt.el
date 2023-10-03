;;; helm-apt.el --- Helm interface for Debian/Ubuntu packages (apt-*) -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2023 Thierry Volpiatto <thierry.volpiatto@gmail.com>
;; Author:      Thierry Volpiatto <thievol@posteo.net>

;; URL: https://github.com/emacs-helm/helm-apt
;; Package-Requires: ((helm "3.9.5") (emacs "25.1"))
;; Version: 1.1

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
;; Helm interface to apt package manager. 

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-utils)
(require 'helm-external)
(require 'helm-help)

(declare-function term-line-mode "term")
(declare-function term-char-mode "term")
(declare-function term-send-input "term")

;;; Internals vars
(defvar helm-apt-show-command "apt-cache show '%s'")
(defvar helm-apt-installed-packages nil)
(defvar helm-apt-term-buffer nil)
(defvar helm-apt-default-archs nil)


(defgroup helm-apt nil
  "Apt related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-apt-cache-show-function 'helm-apt-cache-show-1
  "Function of one argument used to show apt package.
Default is `helm-apt-cache-show-1' but you can use `apt-utils-show-package-1'
from `apt-utils.el' to have something more enhanced.
If nil default `helm-apt-cache-show-1' will be used."
  :type 'function
  :group 'helm-apt)

(defcustom helm-apt-actions
  '(("Show package description" . helm-apt-cache-show)
    ("Install package(s)" . helm-apt-install)
    ("Reinstall package(s)" . helm-apt-reinstall)
    ("Remove package(s)" . helm-apt-uninstall)
    ("Purge package(s)" . helm-apt-purge))
  "Actions for helm apt."
  :type '(alist :key-type string :value-type function))

(defface helm-apt-installed
    '((t (:inherit font-lock-type-face)))
  "Face used for apt installed candidates."
  :group 'helm-apt)

(defface helm-apt-deinstalled
    '((t (:inherit font-lock-doc-face)))
  "Face used for apt deinstalled candidates."
  :group 'helm-apt)


;;; A mode to show package descriptions
;;
(defvar helm-apt-show-current-package nil)
(define-derived-mode helm-apt-show-mode
    special-mode "helm-apt-show"
    "Mode to display infos on apt packages."
    (font-lock-add-keywords nil '(("^\\(.*: \\).*" 1 '((:foreground "Darkslategray1")))))
    (font-lock-add-keywords nil '(("^\\(.*: \\)\\(.*\\)" 2 '((:foreground "DarkOrange")))))
    (font-lock-add-keywords nil '(("\\(https?://\\)\\(.*\\)" 0 '((:foreground "#73d216" :weight bold :underline t)))))
    (goto-char (point-min))
    (let ((map (make-sparse-keymap))
          (inhibit-read-only t))
      ;; TODO Add mouse support as well.
      (define-key map (kbd "RET") 'browse-url-at-point)
      (define-key map [mouse-1] 'helm-apt-browse-url)
      (while (re-search-forward "https?://.*" nil t)
        (add-text-properties
         (match-beginning 0) (match-end 0)
         `(keymap ,map
           help-echo "Mouse-1:Browse url"
           mouse-face highlight)))))

(defun helm-apt-cache-show (package)
  "Show information on apt package PACKAGE."
  (if (and (get-buffer-window "*helm apt show*" 'visible)
           (string= package (buffer-local-value
                             'helm-apt-show-current-package
                             (get-buffer "*helm apt show*"))))
      (kill-buffer "*helm apt show*")
    (if (and (functionp helm-apt-cache-show-function)
             (not (eq helm-apt-cache-show-function
                      'helm-apt-cache-show)))
        ;; A function, call it.
        (funcall helm-apt-cache-show-function package)
      ;; nil or whatever use default.
      (helm-apt-cache-show-1 package))))

(defun helm-apt-cache-show-1 (package)
  "[INTERNAL] Called by `helm-apt-cache-show' with PACKAGE as arg."
  (let* ((command (format helm-apt-show-command package))
         (buf     (get-buffer-create "*helm apt show*")))
    (display-buffer buf)
    (set-buffer buf)
    (unless (string= package helm-apt-show-current-package)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (save-excursion
          (call-process-shell-command
           command nil (current-buffer) t))))
    (helm-apt-show-mode)
    (set (make-local-variable 'helm-apt-show-current-package)
         package)))


;;; Other actions
;;
(defun helm-apt-persistent-action (candidate)
  "Run persistent action on CANDIDATE for APT source."
  (helm-apt-cache-show candidate))

(defun helm-apt-browse-url (_event)
  "browse-url-at-point on mouse click."
  (interactive "e")
  (browse-url-at-point))

(defun helm-apt-install (_package)
  "Run \"apt-get install\" shell command."
  (helm-apt-generic-action :action 'install))

(defun helm-apt-reinstall (_package)
  "Run \"apt-get install --reinstall\" shell command."
  (helm-apt-generic-action :action 'reinstall))

(defun helm-apt-uninstall (_package)
  "Run \"apt-get remove\" shell command."
  (helm-apt-generic-action :action 'uninstall))

(defun helm-apt-purge (_package)
  "Run \"apt-get purge\" shell command."
  (helm-apt-generic-action :action 'purge))

(defvar term-char-mode-buffer-read-only)
(defvar term-char-mode-point-at-process-mark)

(cl-defun helm-apt-generic-action (&key action)
  "Run \"apt-get ACTION\".
Support install, remove and purge actions."
  ;; Reproduce the Emacs-25 behavior to be able to edit and send
  ;; command in term buffer.
  (let (term-char-mode-buffer-read-only       ; Emacs-25 behavior.
        term-char-mode-point-at-process-mark) ; Emacs-25 behavior.
    (if (and helm-apt-term-buffer
             (buffer-live-p (get-buffer helm-apt-term-buffer)))
        (switch-to-buffer helm-apt-term-buffer)
      (ansi-term (getenv "SHELL") "term apt")
      (setq helm-apt-term-buffer (buffer-name)))
    (term-line-mode)
    (let* ((command   (cl-case action
                        (install   "sudo apt-get install ")
                        (reinstall "sudo apt-get install --reinstall ")
                        (uninstall "sudo apt-get remove ")
                        (purge     "sudo apt-get purge ")
                        (t          (error "Unknown action"))))
           (cands     (helm-marked-candidates))
           (cand-list (mapconcat (lambda (x) (format "'%s'" x)) cands " "))
           (inhibit-read-only t))
      (save-window-excursion
        (with-helm-display-marked-candidates
          "*apt candidates*"
          cands
          (when (y-or-n-p (format "%s package(s)? " (symbol-name action)))
            (with-current-buffer helm-apt-term-buffer
              (goto-char (process-mark (get-buffer-process (current-buffer))))
              (delete-region (point) (point-max))
              (insert (concat command cand-list))
              (setq helm-external-commands-list nil)
              (setq helm-apt-installed-packages nil)
              (term-char-mode)
              (term-send-input))))))))

;;; helm-apt-search
;;
(defsubst helm-apt--installed-package-name (name)
  "Return non nil if package named NAME is installed."
  (cl-loop for arch in helm-apt-default-archs
           thereis (or (assoc-default
                        name helm-apt-installed-packages)
                       (assoc-default
                        (format "%s:%s" name arch)
                        helm-apt-installed-packages))))

(defun helm-apt-search-init ()
  "Initialize async process for `helm-apt-search'."
  (let* ((patterns (helm-mm-split-pattern helm-pattern t))
         (pipe-cmd "grep")
         (cmd (helm-aif (cdr patterns)
                  (format "apt-cache search %s %s"
                          (car patterns)
                          (cl-loop for p in it concat
                                   (format " | %s -- %s"
                                           pipe-cmd
                                           (shell-quote-argument p))))
                (format "apt-cache search %s" (shell-quote-argument helm-pattern))))
         (proc (start-process-shell-command
                "Apt-async" nil cmd)))
    proc))

(defun helm-apt-search-transformer (candidates _source)
  "The filtered-candidate-transformer for `helm-apt-search'."
  (cl-loop for cand in candidates
           for split = (split-string cand " - ")
           for name = (car split)
           for iname = (helm-apt--installed-package-name name)
           for deinstall = (string= iname "deinstall")
           for install = (string= iname "install")
           for disp1 = (cond (deinstall
                              (propertize name 'face 'helm-apt-deinstalled))
                             (install
                              (propertize name 'face 'helm-apt-installed))
                             (t name))
           for desc = (cadr split)
           for sep = (helm-make-separator name 40)
           collect (cons (concat
                          disp1
                          sep
                          (propertize
                           desc 'face 'font-lock-warning-face))
                         name)))

;;;###autoload
(defun helm-apt-search ()
  "Search in pkg names and their whole description asynchronously."
  (interactive)
  (unless helm-apt-default-archs
    (setq helm-apt-default-archs
          (append (split-string
                   (shell-command-to-string
                    "dpkg --print-architecture")
                   "\n" t)
                  (split-string
                   (shell-command-to-string
                    "dpkg --print-foreign-architectures")
                   "\n" t))))
  (unless helm-apt-installed-packages
    (setq helm-apt-installed-packages
          (with-temp-buffer
            (call-process-shell-command "dpkg --get-selections"
                                        nil (current-buffer))
            (cl-loop for i in (split-string (buffer-string) "\n" t)
                     for p = (split-string i)
                     collect (cons (car p) (cadr p))))))
  (helm :sources (helm-build-async-source "Apt async"
                   :candidates-process #'helm-apt-search-init
                   :filtered-candidate-transformer #'helm-apt-search-transformer
                   :action 'helm-apt-actions
                   :requires-pattern 2)
        :buffer "*helm apt async*"))

(provide 'helm-apt)

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-apt.el ends here
