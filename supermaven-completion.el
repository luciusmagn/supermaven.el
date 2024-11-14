;;; supermaven-completion.el --- Supermaven completion management -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains functions for managing Supermaven completions and suggestions.

;;; Code:

(require 'supermaven-config)
(require 'supermaven-util)

(defvar-local supermaven--current-overlay nil
  "Overlay for displaying the current suggestion.")

(defvar supermaven--completion-state (make-hash-table :test 'equal)
  "Hash table to store completion states.")

(defun supermaven--display-suggestion (suggestion)
  "Display SUGGESTION as an overlay."
  (supermaven--clear-suggestion)
  (let ((ov (make-overlay (point) (point))))
    (overlay-put ov 'after-string (propertize suggestion 'face 'shadow))
    (setq supermaven--current-overlay ov)))

(defun supermaven--clear-suggestion ()
  "Clear the current suggestion overlay."
  (when supermaven--current-overlay
    (delete-overlay supermaven--current-overlay)
    (setq supermaven--current-overlay nil)))

(defun supermaven-accept-suggestion ()
  "Accept the current suggestion."
  (interactive)
  (when supermaven--current-overlay
    (let ((suggestion (overlay-get supermaven--current-overlay 'after-string)))
      (delete-overlay supermaven--current-overlay)
      (insert suggestion))))

(defun supermaven-clear-suggestion ()
  "Clear the current suggestion."
  (interactive)
  (supermaven--clear-suggestion))

(defun supermaven--update-completion-state (state-id items)
  "Update completion state with STATE-ID and ITEMS."
  (puthash state-id items supermaven--completion-state)
  (supermaven--process-completion (gethash state-id supermaven--completion-state)))

(defun supermaven--process-completion (items)
  "Process completion ITEMS received from Supermaven."
  (when items
    (let* ((first-item (aref items 0))
           (text-edit (cdr (assoc 'textEdit first-item)))
           (new-text (cdr (assoc 'newText text-edit))))
      (when new-text
        (supermaven--display-suggestion new-text)))))

(defun supermaven--get-buffer-text ()
  "Get the current buffer text."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun supermaven--get-cursor-prefix ()
  "Get the text before the cursor."
  (buffer-substring-no-properties (point-min) (point)))

(defun supermaven--get-cursor-suffix ()
  "Get the text after the cursor."
  (buffer-substring-no-properties (point) (point-max)))

(defun supermaven--send-file-update ()
  "Send a file update message to Supermaven."
  (when-let ((file-name (buffer-file-name)))
    (supermaven--send-message
     `((kind . "file_update")
       (path . ,file-name)
       (content . ,(supermaven--get-buffer-text))))))

(defun supermaven--send-cursor-update ()
  "Send a cursor update message to Supermaven."
  (when-let ((file-name (buffer-file-name)))
    (supermaven--send-message
     `((kind . "cursor_update")
       (path . ,file-name)
       (offset . ,(1- (point)))))))


(defun supermaven--send-state-update ()
  "Send a state update message with file and cursor updates."
  (when-let ((file-name (buffer-file-name)))
    (let ((state-id (supermaven--generate-state-id)))
      (supermaven--send-message
       `((kind . "state_update")
         (newId . ,state-id)
         (updates . [
                     ((kind . "file_update")
                      (path . ,file-name)
                      (content . ,(supermaven--get-buffer-text)))
                     ((kind . "cursor_update")
                      (path . ,file-name)
                      (offset . ,(1- (point))))]))))))


(defun supermaven--on-change (&rest _)
  "Handle buffer changes."
  (when (and supermaven-mode (buffer-file-name))
    (supermaven-log-debug "Sending state update")
    (condition-case err
        (supermaven--send-state-update)
      (error
       (supermaven-log-error
        (format "Error in on-change: %s" (error-message-string err)))))))

(defun supermaven--on-post-command ()
  "Handle post-command events."
  (when (and supermaven-mode (buffer-file-name))
    (supermaven-log-debug "Sending state update on post-command")
    (condition-case err
        (supermaven--send-state-update)
      (error
       (supermaven-log-error
        (format "Error in post-command: %s" (error-message-string err)))))))

(define-minor-mode supermaven-mode
  "Minor mode for Supermaven."
  :lighter " Supermaven"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "TAB") #'supermaven-accept-suggestion)
            (define-key map (kbd "C-]") #'supermaven-clear-suggestion)
            map)
  (if supermaven-mode
      (progn
        (add-hook 'after-change-functions #'supermaven--on-change nil t)
        (add-hook 'post-command-hook #'supermaven--on-post-command nil t)
        (supermaven-start))
    (remove-hook 'after-change-functions #'supermaven--on-change t)
    (remove-hook 'post-command-hook #'supermaven--on-post-command t)
    (supermaven-stop)))

(provide 'supermaven-completion)

;;; supermaven-completion.el ends here
