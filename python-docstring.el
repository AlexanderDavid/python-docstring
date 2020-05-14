;;; python-docstring.el --- Insert a python docstring -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Alex Day
;;
;; Author: Alex Day <http://github/alexanderdavid>
;; Maintainer: Alex Day <alexday135@gmail.com>
;; Created: May 11, 2020
;; Modified: May 11, 2020
;; Version: 0.0.1
;; Keywords: Python, Docstring, Google-Style
;; Homepage: https://github.com/alexexanderdavid/python-docstring.el
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; To Do:
;;    - Make indentation match
;;    - Find way to prompt user to enter data with tab stuff
;;
;;; Code:

;; Struct for the argument of a function
(cl-defstruct arg name type def)

(defun python-docstring/get-function-header ()
  "Find the function header at the current cursor position"
  (let ((start (search-backward "def"))
        (end   (search-forward "\)")))
    (replace-regexp-in-string "\n" ""
      (replace-regexp-in-string "\s*" ""
        (if (buffer-substring end (+ end 6))
            (buffer-substring start (search-forward ":"))
            (buffer-substring start end)
            ))))

  )

(defun python-docstring/get-function-indentation ()
  "Get the indentation of the current function."
  (let ((start (search-backward "def")))
    (beginning-of-line)
    (- start (point))
    )
  )

(defun python-docstring/get-function-name (header)
  "Given a function header return the function's name."
  (string-match "def.*\("
                header)

  (substring (match-string 0 header) 4 -1)
)

(defun python-docstring/get-function-return-type (header)
  "Get the return type if one exists"
  (when (string-match "->" header)
    (substring header (match-end 0) -1)
    )
  )

(defun python-docstring/argument-builder (arg-text)
  "Build an argument struct from a string."
  (if (string-match ":.*=" arg-text)
      (let ((args (split-string (replace-regexp-in-string "=" ":"
                                 (replace-regexp-in-string " " "" arg-text)) ":")))
        (make-arg :name (car args) :type (car (cdr args)) :def (car (cdr (cdr args)))))
    (if (string-match ":" arg-text)
        (let ((args (split-string arg-text ":")))
          (make-arg :name (car args) :type (car (cdr args))))
      (if (string-match "=" arg-text)
          (let ((args (split-string arg-text "=")))
           (make-arg :name (car args) :def (car (cdr args))))
        (make-arg :name arg-text))))
)

(defun python-docstring/get-function-args (header)
  "Given a function header return all args, types, and default values."
  (string-match "\(.*\)"
                header)

  (mapcar 'python-docstring/argument-builder
          (split-string (substring (match-string 0 header) 1 -1) ",\s?"))

  )


(defun python-docstring/generate-docstring ()
  "Generate documentation for function under cursor."
  (interactive)
  (let* ((header (python-docstring/get-function-header))
         (args (python-docstring/get-function-args header))
         (return-type (python-docstring/get-function-return-type header))
         (indentation (make-string (python-docstring/get-function-indentation) ?\s)))
    (search-forward "\)")
    (next-line)
    (beginning-of-line)
    (insert "\n")
    (previous-line)
    (insert indentation)
    (insert indentation)
    (insert "\"\"\"\n")
    (insert indentation)
    (insert indentation)
    (insert "Args:\n")
    (mapc (lambda (arg)
              (when (not (string= (arg-name arg) "self"))
                  (insert indentation)
                  (insert indentation)
                  (insert indentation)
                   (insert (arg-name arg))
                   (when (or (arg-def arg) (arg-type arg))
                     (insert " (")
                     (if (arg-type arg)
                         (insert (replace-regexp-in-string "\s" "" (arg-type arg))))
                     (if (and (arg-type arg) (arg-def arg))
                         (insert ", "))
                     (when (arg-def arg)
                       (insert "default: ")
                       (insert (arg-def arg))
                       )
                     (insert ")")
                     )
                   (insert ": \n")
                )
              ) args)

    (when return-type
      (insert "\n")
      (insert indentation)
      (insert indentation)
      (insert "Returns:\n")
      (insert indentation)
      (insert indentation)
      (insert indentation)
      (insert return-type)
      (insert ": ")
      (insert "\n")
    )
    (kill-whole-line)
    (insert indentation)
    (insert indentation)
    (insert "\"\"\"\n")
  )
)


(provide 'docstring)
;;; docstring.el ends here
