#lang plai-typed

(require "python-core-syntax.rkt")
(require "builtins/num.rkt"
         "builtins/str.rkt"
         "builtins/list.rkt"
         "builtins/object.rkt"
         "util.rkt"
)

#|

Here is a suggestion for how to implement shared runtime functionality -
write it as core expression forms and use python-lib to wrap your
desugared expressions in an environment that will contain useful
bindings.  For example, this sample library binds `print` to a function
that calls the primitive `print`.

|#

(define-type-alias Lib (CExpr -> CExpr))

(define print-lambda
  (CFunc (list 'to-print)
    (CSeq 
      (CPrim1 'print (CId 'to-print))
      (CNone))))

(define assert-true-lambda
  (CFunc (list 'check-true)
    (CIf (CId 'check-true) (CNone) (CError (CStr "Assert failed")))))

(define assert-false-lambda
  (CFunc (list 'check-false)
    (CIf (CId 'check-false) (CError (CStr "Assert failed")) (CTrue))))

(define assert-equal-lambda
  (CFunc (list 'check1 'check2)
    (CIf (CPrim2 'Eq (CId 'check1) (CId 'check2))
         (CNone)
         (CError (CStr "Assert failed")))))

(define exception
  (CClass
    'Exception
    'object
    (seq-ops (list 
               (def '__init__
                    (CFunc (list 'self 'args)
                           (CAssign 
                             (CGetField
                               (CId 'self)
                               'args)
                             (CId 'args))))))))

(define type-error
  (CObject
    'Exception
    (some (MetaClass 'TypeError))))

(define len-lambda
  (CFunc (list 'self)
    (CReturn
      (CApp
        (CGetField
          (CId 'self)
          '__len__)
        (list (CId 'self))))))

(define min-lambda
  (CFunc (list 'self)
    (CReturn
      (CApp
        (CGetField
          (CId 'self)
          '__min__)
        (list (CId 'self))))))

(define max-lambda
  (CFunc (list 'self)
    (CReturn
      (CApp
        (CGetField
          (CId 'self)
          '__max__)
        (list (CId 'self))))))

(define true-val
  (CTrue))

(define false-val
  (CFalse))

(define-type LibBinding
  [bind (left : symbol) (right : CExpr)])

(define lib-functions
  (list (bind 'True true-val)
        (bind 'False false-val)
        (bind 'None (CNone))
        (bind 'object object-class)
        (bind 'num (num-class 'num))
        (bind 'str str-class)
        (bind 'len len-lambda)
        (bind 'min min-lambda)
        (bind 'max max-lambda)
        (bind 'print print-lambda)
        (bind 'Exception exception)
        (bind 'TypeError type-error)
        (bind '___assertEqual assert-equal-lambda)
        (bind '___assertTrue assert-true-lambda)
        (bind '___assertFalse assert-false-lambda)
        (bind 'num (num-class 'num))
        (bind 'str str-class)
        (bind 'list list-class)
        ))

(define (python-lib expr)
  (local [(define (python-lib/recur libs)
            (cond [(empty? libs) expr]
                  [(cons? libs)
                   (type-case LibBinding (first libs)
                     (bind (name value)
                           (CLet name value
                                 (python-lib/recur (rest libs)))))]))]
    (python-lib/recur lib-functions)))


