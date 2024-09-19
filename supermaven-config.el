;;; supermaven-config.el --- Supermaven configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains configuration options for Supermaven.

;;; Code:

(defgroup supermaven nil
  "Supermaven settings."
  :group 'completion
  :prefix "supermaven-")

(defcustom supermaven-ignore-filetypes '()
  "List of filetypes to ignore."
  :type '(repeat string)
  :group 'supermaven)

(defcustom supermaven-disable-inline-completion nil
  "Whether to disable inline completion."
  :type 'boolean
  :group 'supermaven)

(defcustom supermaven-keymaps
  '((accept-suggestion . "TAB")
    (clear-suggestion . "C-]")
    (accept-word . "C-j"))
  "Keymaps for Supermaven actions."
  :type '(alist :key-type symbol :value-type string)
  :group 'supermaven)

(defcustom supermaven-condition (lambda () nil)
  "Function to determine if Supermaven should be enabled."
  :type 'function
  :group 'supermaven)

(defcustom supermaven-log-level 'info
  "Log level for Supermaven."
  :type '(choice (const :tag "Off" off)
          (const :tag "Error" error)
          (const :tag "Warn" warn)
          (const :tag "Info" info)
          (const :tag "Debug" debug)
          (const :tag "Trace" trace))
  :group 'supermaven)

(defvar supermaven-dust-strings nil
  "List of dust strings used by Supermaven.")

(defvar supermaven-activate-url nil
  "URL for activating Supermaven Pro.")

(provide 'supermaven-config)

;;; supermaven-config.el ends here
