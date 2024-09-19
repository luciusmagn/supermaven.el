;;; supermaven-binary.el --- Supermaven binary management -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains functions for managing the Supermaven binary.

;;; Code:

(require 'url)
(require 'json)

(defcustom supermaven-binary-path nil
  "Path to the Supermaven binary."
  :type 'string
  :group 'supermaven)

(defun supermaven--fetch-binary ()
  "Fetch the Supermaven binary."
  (let* ((platform (pcase system-type
                     ('darwin "macosx")
                     ('gnu/linux "linux")
                     ('windows-nt "windows")
                     (_ (error "Unsupported platform"))))
         (arch (pcase system-configuration
                 ((or "x86_64-apple-darwin" "x86_64-pc-linux-gnu" "x86_64-w64-mingw32") "x86_64")
                 ((or "aarch64-apple-darwin" "aarch64-pc-linux-gnu") "aarch64")
                 (_ (error "Unsupported architecture"))))
         (url (format "https://supermaven.com/api/download-path?platform=%s&arch=%s&editor=emacs" platform arch))
         (response (with-current-buffer
                       (url-retrieve-synchronously url)
                     (goto-char (point-min))
                     (re-search-forward "\n\n")
                     (json-read)))
         (download-url (cdr (assoc 'downloadUrl response)))
         (binary-dir (expand-file-name ".supermaven/binary/v15" (getenv "HOME")))
         (binary-path (expand-file-name (format "sm-agent%s" (if (eq system-type 'windows-nt) ".exe" "")) binary-dir)))

    (unless (file-exists-p binary-dir)
      (make-directory binary-dir t))

    (url-copy-file download-url binary-path t)

    (set-file-modes binary-path #o755)

    (setq supermaven-binary-path binary-path)
    binary-path))

(defun supermaven--ensure-binary ()
  "Ensure the Supermaven binary is available."
  (unless (and supermaven-binary-path (file-exists-p supermaven-binary-path))
    (setq supermaven-binary-path (supermaven--fetch-binary))))

(provide 'supermaven-binary)

;;; supermaven-binary.el ends here
