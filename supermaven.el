;;; supermaven.el --- Supermaven for Support -*- lexical-binding: t; -*-


;; Author: Brayden Moon<crazywolf132@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.4"))
;; Keywords: completion, ai
;; URL: https://github.com/crazywolf132/supermaven.el

;;; Commentary:

;; This is a port of the Supermaven official NVIM plugin.
;; Supermaven is an AI-powered code completion plugin for Emacs.

;;; Code:

(require 'cl-lib)
(require 'json)

(require 'supermaven-binary)
(require 'supermaven-completion)
(require 'supermaven-config)
(require 'supermaven-logger)
(require 'supermaven-util)

(defvar supermaven--process nil
  "The Supermaven process.")

(defvar supermaven--buffer " *supermaven*"
  "The name of the Supermaven process buffer.")

(defun supermaven-start ()
  "Start the Supermaven process."
  (interactive)
  (unless supermaven--process
    (supermaven--ensure-binary)
    (setq supermaven--process
          (make-process
           :name "supermaven"
           :buffer supermaven--buffer
           :command (list supermaven-binary-path "stdio")
           :filter #'supermaven--process-filter
           :sentinel #'supermaven--process-sentinel))
    (supermaven--send-greeting)))

(defun supermaven-stop ()
  "Stop the Supermaven process."
  (interactive)
  (when supermaven--process
    (delete-process supermaven--process)
    (setq supermaven--process nil)))

(defun supermaven-restart ()
  "Restart the Supermaven process."
  (interactive)
  (supermaven-stop)
  (supermaven-start))

(defun supermaven-toggle ()
  "Toggle the Supermaven process."
  (interactive)
  (if supermaven--process
      (supermaven-stop)
    (supermaven-start)))

(defun supermaven--process-filter (process output)
  "Filter function for the Supermaven process."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (goto-char (point-max))
      (insert output))
    (supermaven--handle-output output)))

(defun supermaven--process-sentinel (process event)
  "Sentinel function for the Supermaven process."
  (supermaven-log-info (format "Supermaven process %s" event))
  (when (memq (process-status process) '(exit signal))
    (setq supermaven--process nil)))

(defun supermaven--send-greeting ()
  "Send greeting message to the Supermaven process."
  (supermaven--send-message
   `((kind . "greeting")
     (allowGitignore . :json-false))))

(defun supermaven--send-message (message)
  "Send MESSAGE to the Supermaven process."
  (when supermaven--process
    (let ((json-string (json-encode message)))
      (supermaven-log-debug (format "Sending message: %s" json-string))
      (process-send-string
       supermaven--process
       (concat json-string "\n")))))


;;(defun supermaven--send-message (message)
;;  "Send MESSAGE to the Supermaven process."
;;  (when supermaven--process
;;    (process-send-string
;;     supermaven--process
;;     (concat (json-encode message) "\n"))))

(defun supermaven--handle-output (output)
  "Handle OUTPUT from the Supermaven process."
  (dolist (line (split-string output "\n" t))
    (when (string-prefix-p "SM-MESSAGE " line)
      (let* ((json-string (substring line 11))
             (message (json-read-from-string json-string)))
        (supermaven--process-message message)))))

(defun supermaven--process-message (message)
  "Process MESSAGE received from Supermaven."
  (pcase (alist-get 'kind message)
    ("response" (supermaven--handle-response message))
    ("metadata" (supermaven--handle-metadata message))
    ("activation_request" (supermaven--handle-activation-request message))
    ("activation_success" (supermaven--handle-activation-success message))
    ("service_tier" (supermaven--handle-service-tier message))
    (_ (supermaven-log-debug (format "Unknown message kind: %s" (alist-get 'kind message))))))

(defun supermaven--handle-response (message)
  "Handle response MESSAGE from Supermaven."
  (let ((state-id (alist-get 'stateId message))
        (items (alist-get 'items message)))
    (supermaven--update-completion-state state-id items)))

(defun supermaven--handle-metadata (message)
  "Handle metadata MESSAGE from Supermaven."
  (when-let ((dust-strings (alist-get 'dustStrings message)))
    (setq supermaven-dust-strings dust-strings)))

(defun supermaven--handle-activation-request (message)
  "Handle activation request MESSAGE from Supermaven."
  (let ((activate-url (alist-get 'activateUrl message)))
    (setq supermaven-activate-url activate-url)
    (supermaven-log-info (format "Visit %s to activate Supermaven Pro" activate-url))))

(defun supermaven--handle-activation-success (_message)
  "Handle activation success MESSAGE from Supermaven."
  (setq supermaven-activate-url nil)
  (supermaven-log-info "Supermaven was activated successfully."))

(defun supermaven--handle-service-tier (message)
  "Handle service tier MESSAGE from Supermaven."
  (when-let ((display (alist-get 'display message)))
    (supermaven-log-info (format "Supermaven %s is running." display))))

(defun supermaven-use-free-version ()
  "Switch to the free version of Supermaven."
  (interactive)
  (supermaven--send-message '((kind . "use_free_version"))))

(defun supermaven-use-pro ()
  "Switch to the pro version of Supermaven."
  (interactive)
  (if supermaven-activate-url
      (browse-url supermaven-activate-url)
    (supermaven-log-error "Could not find an activation URL")))

(defun supermaven-logout ()
  "Log out from Supermaven."
  (interactive)
  (supermaven--send-message '((kind . "logout"))))

(provide 'supermaven)

;;; supermaven.el ends here
