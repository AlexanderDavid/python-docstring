;;; python-docstring --- Print python docstring for function at point
;;; Commentary:
;;  This is the test suite for the python-docstring.el file. It uses
;;  the emacs-buttercup testing module. This does not test all of the functions
;;  but it tests a majority of them and it tests all of the pure functions
;;; Code:
(require 'buttercup)
(require 'python-docstring)

;; Test the get-function-name function
(describe "Test the get-function-name function"
  (it "returns the function name with no type given"
    (expect (python-docstring/--get-function-name "def foo():")
            :to-equal "foo"))
  (it "returns the function name with a type given"
    (expect (python-docstring/--get-function-name "def foo() -> int:")
            :to-equal "foo"))
  (it "returns the function name with arguments given"
    (expect (python-docstring/--get-function-name "def foo(arg)")
            :to-equal "foo")))

;; Test the get-function-return-type function
(describe "Test the get-function-return-type function"
  (it "Should return the return type when given one"
    ;; The function will be given a function with no spaces
    (expect (python-docstring/--get-function-return-type "def foo()->type:")
            :to-equal "type"))
  (it "Should return nil when given no return type"
    (expect (python-docstring/--get-function-return-type "def foo():")
            :to-be nil)))

;; Test the argument-builder function
(describe "The argument-builder function"
  (it "should return just the argument name when not given type or default"
    (let ((arg (python-docstring/--argument-builder "argument")))
      (expect (python-docstring/--arg-name arg) :to-equal "argument")
      (expect (python-docstring/--arg-type arg) :to-be nil)
      (expect (python-docstring/--arg-def arg) :to-be nil)))
  (it "should return the argument name and type when not given default"
    (let ((arg (python-docstring/--argument-builder "argument:type")))
      (expect (python-docstring/--arg-name arg) :to-equal "argument")
      (expect (python-docstring/--arg-type arg) :to-equal "type")
      (expect (python-docstring/--arg-def arg) :to-be nil)))
  (it "should return the argument name and default when not given type"
    (let ((arg (python-docstring/--argument-builder "argument=default")))
      (expect (python-docstring/--arg-name arg) :to-equal "argument")
      (expect (python-docstring/--arg-type arg) :to-be nil)
      (expect (python-docstring/--arg-def arg) :to-equal "default")))
  (it "should return the argument name, type, and default"
    (let ((arg (python-docstring/--argument-builder "argument:type=default")))
      (expect (python-docstring/--arg-name arg) :to-equal "argument")
      (expect (python-docstring/--arg-type arg) :to-equal "type")
      (expect (python-docstring/--arg-def arg) :to-equal "default"))))

;; Test the get-function-args function
(describe "The get-function-args function"
  (it "should return nil when given no arguments"
    (let ((args (python-docstring/--get-function-args "deffoo():")))
      (expect args :to-be nil)))
  (it "should return the correct number of arguments"
    (let ((args (python-docstring/--get-function-args "deffoo(test,test:type,test:type=default)")))
      (expect (length args) :to-equal 3)))
  (it "should return the correct arguments"
    (let ((args (python-docstring/--get-function-args "deffoo(test:type=default)")))
      (expect (python-docstring/--arg-name (car args)) :to-equal "test")
      (expect (python-docstring/--arg-type (car args)) :to-equal "type")
      (expect (python-docstring/--arg-def (car args)) :to-equal "default"))))



(provide 'test-python-docstring)
;;; test-python-docstring.el ends here
