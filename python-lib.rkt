#lang plai-typed

(require "python-core-syntax.rkt")
(require "builtins/num.rkt"
         "builtins/str.rkt"
         "builtins/list.rkt"
         "builtins/tuple.rkt"
         "builtins/object.rkt"
         "builtins/bool.rkt"
         "util.rkt"
         (typed-in "get-structured-python.rkt"
                   (get-structured-python : ('a -> 'b)))
         (typed-in "parse-python.rkt"
                   (parse-python/port : ('a string -> 'b)))
         (typed-in racket/base (open-input-file : ('a -> 'b)))

         "python-desugar.rkt"
         (typed-in racket/base (append : ((listof 'a) (listof 'a) -> (listof 'a)))))

#|

Here is a suggestion for how to implement shared runtime functionality -
write it as core expression forms and use python-lib to wrap your
desugared expressions in an environment that will contain useful
bindings.  For example, this sample library binds `print` to a function
that calls the primitive `print`.

|#


(define print-lambda
  (CFunc (list 'to-print) (none) 
    (CSeq 
      (CPrim1 'print (CApp 
                       (CGetField (CId 'to-print) '__str__) 
                       (list (CId 'to-print))))
      (CNone))))

(define assert-true-lambda
  (CFunc (list 'check-true) (none)
    (CIf (CId 'check-true) (CNone) (CError (CStr "Assert failed")))))

(define assert-false-lambda
  (CFunc (list 'check-false) (none)
    (CIf (CId 'check-false) (CError (CStr "Assert failed")) (CTrue))))

(define assert-equal-lambda
  (CFunc (list 'check1 'check2)  (none)
    (CIf (CApp (CGetField (CId 'check1) '__eq__) (list (CId 'check1) (CId 'check2)))
         (CNone)
         (CError (CStr "Assert failed")))))

(define assert-is-lambda
  (CFunc (list 'check1 'check2) (none)
    (CIf (CPrim2 'Is (CId 'check1) (CId 'check2))
         (CNone)
         (CError (CStr "Assert failed")))))

(define assert-isnot-lambda
  (CFunc (list 'check1 'check2) (none)
    (CIf (CPrim2 'Is (CId 'check1) (CId 'check2))
         (CError (CStr "Assert failed"))
         (CNone))))

;(define assert-raises-lambda
;  (CFunc (list (none)

(define exception
  (CClass
    'Exception
    'object
    (seq-ops (list 
               (def '__init__
                    (CFunc (list 'self 'args) (none)
                           (CSeq 
                             (CAssign 
                               (CGetField
                                 (CId 'self)
                                 'args)
                               (CId 'args))
                             (CAssign
                               (CGetField
                                 (CId 'self)
                                 '__class__)
                               (CId 'Exception)))))
               (def '__str__
                    (CFunc (list 'self) (none)
                           (CReturn
                             (CApp
                               (CGetField
                                 (CId 'self)
                                 '__add__)
                               (list
                                 (CGetField
                                   (CId 'self)
                                   '__class__)
                                 (CGetField
                                   (CId 'self)
                                   'args))))))))))

(define (make-exception-class [name : symbol]) : CExpr
  (CClass
    name
    'Exception
    (CNone)))

(define len-lambda
  (CFunc (list 'self) (none)
    (CReturn
      (CApp
        (CGetField
          (CId 'self)
          '__len__)
        (list (CId 'self))))))

(define min-lambda
  (CFunc (list 'self) (none)
    (CReturn
      (CApp
        (CGetField
          (CId 'self)
          '__min__)
        (list (CId 'self))))))

(define max-lambda
  (CFunc (list 'self) (none)
    (CReturn
      (CApp
        (CGetField
          (CId 'self)
          '__max__)
        (list (CId 'self))))))

(define abs-lambda
  (CFunc (list 'self) (none)
    (CReturn
      (CApp
        (CGetField
          (CId 'self)
          '__abs__)
        (list (CId 'self))))))

(define int-lambda
  (CFunc (list 'self) (none)
    (CReturn
      (CApp
        (CGetField
          (CId 'self)
          '__int__)
        (list (CId 'self))))))

(define float-lambda
  (CFunc (list 'self) (none)
    (CReturn
      (CApp
        (CGetField
          (CId 'self)
          '__float__)
        (list (CId 'self))))))

(define-type LibBinding
  [bind (left : symbol) (right : CExpr)])

(define lib-functions
  (list (bind 'True (CTrue))
        (bind 'False (CFalse))
        (bind 'None (CNone))

        ; dummies
        (bind 'object (CNone))
        (bind 'num (CNone))
        (bind 'str (CNone))
        (bind 'Exception (CNone))
        (bind 'bool (CNone))
        (bind 'any (CNone))
        (bind 'all (CNone))

        (bind 'object object-class)
        (bind 'num (num-class 'num))
        (bind 'str str-class)
        (bind 'list list-class)
        (bind 'tuple tuple-class)
        (bind 'bool bool-class)
        (bind 'len len-lambda)
        (bind 'min min-lambda)
        (bind 'max max-lambda)
        (bind 'abs abs-lambda)
        (bind 'int int-lambda)
        (bind 'float float-lambda)
        (bind 'print print-lambda)

        (bind 'Exception exception)
        (bind 'TypeError (make-exception-class 'TypeError))
        (bind 'SyntaxError (make-exception-class 'SyntaxError))
        (bind 'NameError (make-exception-class 'NameError))
        (bind 'AttributeError (make-exception-class 'AttributeError))
        (bind '___assertEqual assert-equal-lambda)
        (bind '___assertTrue assert-true-lambda)
        (bind '___assertFalse assert-false-lambda)
        (bind '___assertIs assert-is-lambda)
        (bind '___assertIsNot assert-isnot-lambda)))

;; these are builtin functions that we have written in actual python files which
;; are pulled in here and desugared for lib purposes
(define pylib-programs
  (map (lambda(file) 
         (desugar 
           (get-structured-python 
             (parse-python/port 
               (open-input-file file)
               python-path))))
       (list "pylib/any.py"
             "pylib/all.py")))

(define-type-alias Lib (CExpr -> CExpr))

(define (python-lib [expr : CExpr]) : CExpr
  (seq-ops (append
             (map (lambda(b) (CAssign (CId (bind-left b)) (bind-right b)))
                      lib-functions)
           ;  pylib-programs
             (list expr))))
