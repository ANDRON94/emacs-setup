;;; my-load.el --- Controls loading of setup files.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; It's a mini package that provides an interface
;; for controlling loading of setup files.  Setup file is the file
;; that contains code to download/configure/enable package(s).
;; 'my-load' is a central registry of such files.  User can add
;; setup file to it by specifying unique identifier of file,
;; path to file and names of differents customization functions.
;; Each setup file will call(if it can) customization functions
;; in load process.  It allows to inject specific customization
;; code into common setup file.
;; Also, user can specify which files from registry should be loaded
;; and which should not.

;;; Code:

;; TODO: choose better names!

(require 'my-utility (expand-file-name
                      "./my-utility.el"
                      (file-name-directory load-file-name)))

;; Implementation

(defvar my-load--central-registry nil
  "Property list of setup files that can be loaded.
Key is unique identifier of setup file and value is also a property
list of different options for such file.  Supported options are
:PATH - path to configuration file;
:KEYBINDINGS-FUNC - name of function to setup keybindings;
:CUSTOMIZATION-FUNC- name of function to setup various of options.")

(defun my-load--get-setup-option (unique-key option-name)
  "Get option of setup file with UNIQUE-KEY identifier.
OPTION-NAME is the name of desired option."
  (let ((setup-options (plist-get my-load--central-registry unique-key)))
    (plist-get setup-options option-name)))

(defun my-load--set-setup-option (unique-key option-name option-value)
  "Set option for setup file with UNIQUE-KEY identifier.
OPTION-NAME is the name of desired option.
OPTION-VALUE is the value of desired option."
  (let ((setup-options (plist-get my-load--central-registry unique-key)))
    (setq setup-options
          (plist-put setup-options option-name option-value))
    (setq my-load--central-registry
          (plist-put my-load--central-registry unique-key setup-options))))

(defconst my-load--success-load-format-msg
  "Load setup file with identifier '%s'."
  "This message is shown to user when 'my-load' can load setup file.")

(defconst my-load--failure-load-format-msg
  "Can't load setup file with identifier '%s' \
because it doesn't exist in central registry!"
  "This message is shown to user when 'my-load' can't load setup file.")

(defconst my-load--skip-load-format-msg
  "Skip loading of setup file with identifier '%s' \
because it's in the exception list."
  "This message is shown to user when 'my-load' skips loading of setup file.")

;; Interface

(defun my-load-make-setup-options (file-path &optional keybindings-func
                                             customization-func)
  "Create property list of setup file options.
FILE-PATH is a path to the setup file.
KEYBINDINGS-FUNC is the name of function to configure keybindings.
CUSTOMIZATION-FUNC is the name of function to configure various of options."
  (list :path file-path
        :keybindings-func keybindings-func
        :customization-func customization-func))

(defun my-load-add-setup-file (unique-key setup-options)
  "Add setup file to the central registry.
UNIQUE-KEY is the setup file unique identifier and
SETUP-OPTIONS is the property list of setup file options."
  (setq my-load--central-registry
        (plist-put my-load--central-registry
                   unique-key setup-options)))

(defun my-load-add-setup-files (setup-files)
  "Add many setup files to the central registry.
SETUP-FILES is a property list where key is unique identifier and
value is the setup file options."
  (setq my-load--central-registry
        (append my-load--central-registry setup-files)))

(defun my-load-get-keybindings-func (unique-key)
  "Get keybindings customization function of setup file.
UNIQUE-KEY is the identifier of desired setup file."
  (my-load--get-setup-option unique-key :keybindings-func))

(defun my-load-set-keybindings-func (unique-key keybindings-func)
  "Set keybindings customization function for setup file.
UNIQUE-KEY is the identifier of desired setup file.
KEYBINDINGS-FUNC is the name of desired keybindings customization function."
  (my-load--set-setup-option unique-key :keybindings-func keybindings-func))

(defun my-load-get-customization-func (unique-key)
  "Get general customization function for setup file.
UNIQUE-KEY is the identifier of desired setup file."
  (my-load--get-setup-option unique-key :customization-func))

(defun my-load-set-customization-func (unique-key customization-func)
  "Set general customization function for setup file.
UNIQUE-KEY is the identifier of desired setup file.
CUSTOMIZATION-FUNC is the name of desired customization function."
  (my-load--set-setup-option unique-key :customization-func customization-func))

(defun my-load-load-only (only-keys)
  "Load setup files from central registry which identifiers exist in ONLY-KEYS.
For all entries from ONLY-KEYS list that ain't exist in central registry print
warning message to user."
  (dolist (key only-keys)
    (let ((path-to-setup-file (my-load--get-setup-option key :path)))
      (if path-to-setup-file
          (progn
            (message my-load--success-load-format-msg key)
            (load-file path-to-setup-file))
        (display-warning 'my-load
                         (format my-load--failure-load-format-msg key))))))

(defun my-load-load-all ()
  "Load all setup files from central registry."
  (my-doplist setup-identifier setup-options my-load--central-registry
              (message my-load--success-load-format-msg setup-identifier)
              (load-file (plist-get setup-options :path))))

(defun my-load-load-except (except-keys)
  "Load setup files from central registry that ain't in EXCEPT-KEYS list."
  (my-doplist setup-identifier setup-options my-load--central-registry
              (if (not (member setup-identifier except-keys))
                  (progn
                    (message my-load--success-load-format-msg setup-identifier)
                    (load-file (plist-get setup-options :path)))
                (message my-load--skip-load-format-msg setup-identifier))))

(provide 'my-load)

;;; my-load.el ends here
