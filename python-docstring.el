;;; python-docstring.el --- Insert a python docstring -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Alex Day
;;
;; Author: Alex Day <http://github/AlexanderDavid>
;; Maintainer: Alex Day <alexday135@gmail.com>
;; Created: May 11, 2020
;; Modified: May 11, 2020
;; Version: 0.0.1
;; Keywords: Python, Docstring, Google-Style
;; Homepage: https://github.com/alexexanderdavid/python-docstring
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; To use this file, put something like the following in your ~/.emacs:
;;
;; (add-to-list 'load-path /path/to/python-docstring)
;; (require 'python-docstring)
;;
;; And then bind the (python-docstring/generate-docstring) to a certain
;; key like so:
;;
;; (global-set-key (kbd "<f9>") 'python-docstring/generate-docstring)
;;
;; This function can be called when the cursor is over a python function
;; header. It will produce a google-ish style docstring.
;;
;;; To Do:
;;    - Find way to prompt user to enter data with tab stuff
;;
;;; Code:

;; Struct to store information for one argument
(cl-defstruct python-docstring/--arg name type def)

(defun python-docstring/get-function-header ()
  "Find the function header at the current cursor position."

  ;; Search backwards to the def and then forwards to the :
  (let ((start (search-backward "def"))
        (end   (re-search-forward "\).*:")))
    ;; Replace newlines and whitespace with blanks and return the
    ;; substring
    (replace-regexp-in-string "\n" ""
      (replace-regexp-in-string "\s*" ""
            (buffer-substring start end))))
)

(defun python-docstring/get-function-indentation ()
  "Get the indentation of the current function."
  (let ((start (search-backward "def")))
    (beginning-of-line)
    (- start (point))
    )
  )

(defun python-docstring/--get-function-name (header)
  "Given a function HEADER return the function's name."
  ;; Match the regex that contains def[ANYTHING](. The ANYTHING
  ;; should contain the function name
  (string-match "def.*\("
                header)

  ;; Return the substring that contains the function name
  (substring (match-string 0 header) 4 -1)
)

(defun python-docstring/--get-function-return-type (header)
  "Get the return type from the HEADER if one exists."

  ;; When the string contains -> return the type
  (when (string-match "->" header)
    (substring header (match-end 0) -1)
    )
)

(defun python-docstring/--argument-builder (arg-text)
  "Build an argument struct from an ARG-TEXT string."
  (if (string-match ":.*=" arg-text)
      (let ((args (split-string (replace-regexp-in-string "=" ":"
                                 (replace-regexp-in-string " " "" arg-text)) ":")))
        (make-python-docstring/--arg
         :name (car args)
         :type (car (cdr args))
         :def (car (cdr (cdr args)))))

    (if (string-match ":" arg-text)
        (let ((args (split-string arg-text ":")))
          (make-python-docstring/--arg
           :name (car args)
           :type (car (cdr args))))

      (if (string-match "=" arg-text)
          (let ((args (split-string arg-text "=")))
            (make-python-docstring/--arg
             :name (car args)
             :def (car (cdr args))))

        (make-python-docstring/--arg
         :name arg-text))))
)

(defun python-docstring/--get-function-args (header)
  "Given a function HEADER (from --get-function-header) return all args, types, and default values."

  ;; Match inbetween the parenthesis to get the args
  (string-match "\(.*\)"
                header)

  ;; Define the arguments as the arguments from the header split by commas with optional
  ;; spaces
  (let* ((arguments-text (substring (match-string 0 header) 1 -1))
         (arguments (split-string arguments-text ",\s?")))
    ;; Only proceed if arguments is not an empty string
    (if (not (= (length arguments-text) 0))
        ;; Apply the argument-builder to each argument string
        (mapcar 'python-docstring/--argument-builder arguments)))
)

(defun python-docstring/--print-arg (arg indentation)
  "Print the given ARG for a docstring with a given INDENTATION.
This will print the comment in the Google docstring style:
  arg_name (arg_type, optional: arg_default_value)"

  ;; Only print the string if it is not the "self" argument
  (when (not (string= (python-docstring/--arg-name arg) "self"))
    ;; Indent
    (insert indentation)
    (insert indentation)
    (insert indentation)

    ;; Insert the argument name
    (insert (python-docstring/--arg-name arg))

    ;; Check for either a type or a default value
    (when (or
            (python-docstring/--arg-def arg)
            (python-docstring/--arg-type arg))

      ;; Insert the paren for the type/default value
      (insert " (")

      ;; If there is a type then return the type
      (if (python-docstring/--arg-type arg)
          (insert (python-docstring/--arg-type arg)))

      ;; Print the comma if there is both a default argument and
      ;; a type
      (if (and
            (python-docstring/--arg-def arg)
            (python-docstring/--arg-type arg))
            (insert ", "))

      ;; If there is a default argument then print that
      (when (python-docstring/--arg-def arg)
        (insert "default: ")
        (insert (python-docstring/--arg-def arg)))

      ;; Close the paren
      (insert ")"))

    ;; Insert the : and go to the next line
    (insert ":\n"))
  )

(defun python-docstring/generate-docstring ()
  "Generate documentation for function under cursor."
  (interactive)
  ;; Get information about the function
  (let* ((header (python-docstring/get-function-header))
         (args (python-docstring/--get-function-args header))
         (return-type (python-docstring/--get-function-return-type header))
         (indentation (make-string (python-docstring/get-function-indentation) ?\s)))
    ;; Go to the last char of the function
    (re-search-forward "\).*:")

    ;; Go down one line
    (forward-line)

    ;; Insert the start of the docstring and a FIXME for the comment
    (insert indentation)
    (insert indentation)
    (insert "\"\"\" FIXME: Insert function comment\n")

    ;; If there are arguments then print them
    (unless (eq args nil)
      ;; Insert the start of the argument list
      (insert indentation)
      (insert indentation)
      (insert "Args:\n")

      ;; Map the print function to each argument with the specified indentation
      (mapc (lambda (arg)
              (python-docstring/--print-arg arg indentation)) args))

    ;; Print the return type if one exists
    (when return-type
      ;; Print the returns header
      (insert "\n")
      (insert indentation)
      (insert indentation)
      (insert "Returns:\n")

      ;; Indent the return type
      (insert indentation)
      (insert indentation)
      (insert indentation)

      ;; Print the return type
      (insert return-type)
      (insert ":\n")
    )

    ;; Print the ending comment
    (insert indentation)
    (insert indentation)
    (insert "\"\"\"\n")

    ;; Search back to the beginning and put the mark over the F in FIXME
    (forward-line -1)
    (search-backward "\"\"\"")
    (forward-char 4)
  )
  )

(define-minor-mode python-docstring-minor-mode
  "Minor mode for inserting python docstring in google style."
  :lighter " docstring"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<f9>") 'python-docstring/generate-docstring)
            map))

(provide 'python-docstring)
;;; python-docstring.el ends here
