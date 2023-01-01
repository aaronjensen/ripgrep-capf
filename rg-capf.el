;;; rg-capf.el --- rg completion-at-point function -*- lexical-binding: t -*-

;; Copyright (C) 2023 Aaron Jensen

;; Author: Aaron Jensen
;; URL: https://github.com/aaronjensen/rg-capf
;;
;; Version: 1.0
;; Keywords: company
;; Package-Requires: ((emacs "28.1") (dash "2.2.0"))

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <https://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:
;; A completion-at-point function that uses rg.

;;; Code:

(defgroup rg-capf ()
  "Rg completion-at-point function."
  :group 'convenience
  :prefix "rg-capf")

;; ## Reconsider whether or not I want to use executable-find or just set it to
;; ## "rg" by default - Aaron, Sat Dec 31 2022
(defcustom rg-capf--rg-executable
  (if-let ((executable (executable-find "rg")))
      executable
    (warn "No rg executable found in PATH.")
    "rg")
  "The PATH of the `rg' executable.
A warning is issued if it can't be found on loading."
  :group 'rg-capf
  :type 'string)

(defcustom rg-capf-timeout-seconds 0.75
  "Timeout for rg, the process will be killed if it runs longer."
  :group 'rg-capf
  :type 'float)

(defvar rg-capf-directory nil
  "Default directory used when runing rg

If nil, use default-directory. If set to a function, evaluate
that function. If set to a string, use that directory.")

(defvar rg-capf-error-buffer-name "*rg-capf errors*")
(defvar rg-capf-symbol-characters "[:alnum:]_-")

(defvar rg-capf--process nil)

(defun rg-capf--kill-process (process)
  "Kill running process as safely as possible."
  (when (and process
             (eq (process-status process) 'run))
    (interrupt-process process)
    (kill-process process)))

(defun rg-capf--command (prefix)
  (concat
   (shell-quote-argument
    rg-capf--rg-executable)
   " --ignore-case --only-matching --no-filename --no-line-number "
   (shell-quote-argument
    (concat "(^|[^" rg-capf-symbol-characters "])" prefix "([" rg-capf-symbol-characters "]|::)*"))
   ;; Specify a directory to prevent rg from searching stdin
   " ."
   " | awk '{print $1}' | sed " (shell-quote-argument (concat "s/^[^" rg-capf-symbol-characters "]//"))
   " | sort | uniq -c | sort -rn | awk '{print $2}'"))

;; zim zimmy zimii zimmmms
(defun rg-capf--directory ()
  (or
   (if (functionp rg-capf-directory)
       (funcall rg-capf-directory)
     rg-capf-directory)
   default-directory))

(defun rg-capf--accept-output (process output)
  "Receive output into a process property"
  (push output (process-get process 'rg-capf-output)))

(defun rg-capf--parse-output (output)
  (split-string output "\n"))

(defun rg-capf--get-output (process)
  "Get the complete output of PROCESS."
  (with-demoted-errors "Error while retrieving process output: %S"
    (let ((output (process-get process 'rg-capf-output)))
      (apply #'concat (nreverse output)))))

(defun rg-capf--create-error-buffer ()
  "Get an empty error buffer."
  (let ((error-buffer (get-buffer-create rg-capf-error-buffer-name)))
    (with-current-buffer error-buffer
      (erase-buffer))
    error-buffer))

(defun rg-capf--process (prefix)
  (let ((current-prefix (and rg-capf--process
                             (process-get rg-capf--process 'rg-capf-prefix))))
    (if (and current-prefix
             (string-prefix-p current-prefix prefix))
        rg-capf--process
      (rg-capf--kill-process rg-capf--process)
      (let* ((default-directory (rg-capf--directory))
             (command (rg-capf--command prefix))
             (error-buffer (rg-capf--create-error-buffer))
             (process
              (make-process :name "rg-capf"
                            :buffer nil
                            :stderr error-buffer
                            :connection-type 'pipe
                            :noquery t
                            :command (list shell-file-name shell-command-switch command)
                            :filter #'rg-capf--accept-output)))

        (setq rg-capf--process process)
        (process-put process 'rg-capf-prefix prefix)

        (run-with-timer rg-capf-timeout-seconds nil
                        'rg-capf--kill-process process)

        process))))

(defun rg-capf--completions (prefix)
  (let ((process (rg-capf--process prefix)))
    (while (process-live-p process)
      (sit-for 0.005))

    (when (eq (process-status process) 'exit)
      (let ((output (rg-capf--get-output process)))
        (rg-capf--parse-output output)))))

(defun rg-capf ()
  "Completion function that uses ripgrep for `completion-at-point-functions'"
  (save-excursion
    (when (thing-at-point-looking-at "\\(?:\\sw\\|\\s_\\)+")
      (let ((beg (match-beginning 0))
            (end (match-end 0))
            (prefix (match-string 0)))
        (list beg
              end
              (rg-capf--completions prefix)
              :exclusive 'no)))))

(provide 'rg-capf)
;;; rg-capf.el ends here
