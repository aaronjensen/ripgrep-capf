;;; ripgrep-capf.el --- rg completion-at-point function -*- lexical-binding: t -*-

;; Copyright (C) 2023 Aaron Jensen

;; Author: Aaron Jensen
;; URL: https://github.com/aaronjensen/ripgrep-capf
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

(defgroup ripgrep-capf ()
  "Rg completion-at-point function."
  :group 'convenience
  :prefix "ripgrep-capf")

(defcustom ripgrep-capf--rg-executable
  (if-let ((executable (executable-find "rg")))
      executable
    (warn "No rg executable found in PATH.")
    "rg")
  "The PATH of the `rg' executable.
A warning is issued if it can't be found on loading."
  :group 'ripgrep-capf
  :type 'string)

(defcustom ripgrep-capf-timeout-seconds 0.75
  "Timeout for rg, the process will be killed if it runs longer."
  :group 'ripgrep-capf
  :type 'float)

(defvar ripgrep-capf-directory nil
  "Default directory used when runing rg

If nil, use default-directory. If set to a function, evaluate
that function. If set to a string, use that directory.")

(defvar ripgrep-capf-error-buffer-name "*ripgrep-capf errors*")
(defvar ripgrep-capf-symbol-characters "[:alnum:]_-")

(defvar ripgrep-capf--process nil)

(defun ripgrep-capf--kill-process (process)
  "Kill running process as safely as possible."
  (when (and process
             (eq (process-status process) 'run))
    (interrupt-process process)
    (kill-process process)))

(defun ripgrep-capf--command (prefix)
  (concat
   (shell-quote-argument
    ripgrep-capf--rg-executable)
   " --ignore-case --only-matching --no-filename --no-line-number --replace '$1' --regexp "
   (shell-quote-argument
    ;; ## The :: is a hack for Ruby, it should be made more configurable - Aaron, Sun Jan 01 2023
    (concat "(?:^|[^" ripgrep-capf-symbol-characters "])(" prefix "(:?[" ripgrep-capf-symbol-characters "]|::)*)"))
   ;; Specify a directory to prevent rg from searching stdin
   " ."
   " | sort | uniq -c | sort -rn | awk '{print $2}'"))

(defun ripgrep-capf--directory ()
  (or
   (if (functionp ripgrep-capf-directory)
       (funcall ripgrep-capf-directory)
     ripgrep-capf-directory)
   default-directory))

(defun ripgrep-capf--accept-output (process output)
  "Receive output into a process property"
  (push output (process-get process 'ripgrep-capf-output)))

(defun ripgrep-capf--parse-output (output)
  (split-string output "\n"))

(defun ripgrep-capf--get-output (process)
  "Get the complete output of PROCESS."
  (with-demoted-errors "Error while retrieving process output: %S"
    (let ((output (process-get process 'ripgrep-capf-output)))
      (apply #'concat (nreverse output)))))

(defun ripgrep-capf--create-error-buffer ()
  "Get an empty error buffer."
  (let ((error-buffer (get-buffer-create ripgrep-capf-error-buffer-name)))
    (with-current-buffer error-buffer
      (erase-buffer))
    error-buffer))

(defun ripgrep-capf--process (prefix)
  (let ((current-prefix (and ripgrep-capf--process
                             (process-get ripgrep-capf--process 'ripgrep-capf-prefix))))
    (if (and current-prefix
             (string-prefix-p current-prefix prefix))
        ripgrep-capf--process
      (ripgrep-capf--kill-process ripgrep-capf--process)
      (let* ((default-directory (ripgrep-capf--directory))
             (command (ripgrep-capf--command prefix))
             (error-buffer (ripgrep-capf--create-error-buffer))
             (process
              (make-process :name "ripgrep-capf"
                            :buffer nil
                            :stderr error-buffer
                            :connection-type 'pipe
                            :noquery t
                            :command (list shell-file-name shell-command-switch command)
                            :filter #'ripgrep-capf--accept-output)))

        (setq ripgrep-capf--process process)
        (process-put process 'ripgrep-capf-prefix prefix)

        (run-with-timer ripgrep-capf-timeout-seconds nil
                        'ripgrep-capf--kill-process process)

        process))))

(defun ripgrep-capf--completions (prefix)
  (let ((process (ripgrep-capf--process prefix)))
    (while (process-live-p process)
      (sit-for 0.001))

    (when (eq (process-status process) 'exit)
      (let ((output (ripgrep-capf--get-output process)))
        (ripgrep-capf--parse-output output)))))

;;;###autoload
(defun ripgrep-capf ()
  "Completion function that uses ripgrep for `completion-at-point-functions'"
  (save-excursion
    (when (thing-at-point-looking-at "\\(?:\\sw\\|\\s_\\)+")
      (let ((beg (match-beginning 0))
            (end (match-end 0))
            (prefix (match-string 0)))
        (list beg
              end
              (ripgrep-capf--completions prefix)
              :exclusive 'no)))))

(provide 'ripgrep-capf)
;;; ripgrep-capf.el ends here
