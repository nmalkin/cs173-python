#lang plai-typed 

(require "../python-core-syntax.rkt"
         "../util.rkt"
         "num.rkt"
         "str.rkt")

(define bool-class 
  (CClass 
    'bool
    'num
    (seq-ops (list
               (def '__str__
                    (CFunc (list 'self)
                           (CIf (CApp (CGetField (CId 'self) '__eq__)
                                      (list (CId 'self) (make-builtin-num 1)))
                                (CReturn (make-builtin-str "True"))
                                (CReturn (make-builtin-str "False")))))))))
(define (make-builtin-bool [b : boolean]) : CExpr
  (CObject 
    'bool
    (some 
      (if b 
        (MetaNum 1)
        (MetaNum 0)))))
