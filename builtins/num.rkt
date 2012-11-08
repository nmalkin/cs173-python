#lang plai-typed

(require "../python-core-syntax.rkt")
(require "../util.rkt")

(define (num-class [name : symbol]) : CExpr
  (CClass
    name
    'object
    (seq-ops (list 
               (def '__add__ 
                    (CFunc (list 'self 'other) 
                           (CBuiltinPrim 'num+ 
                                         (list 
                                           (CId 'self) 
                                           (CId 'other)))))
               (def '__str__
                   (CFunc (list 'self)
                          (CBuiltinPrim 'str
                                        (list (CId 'self)))))))))


(define (make-builtin-num [n : number]) : CExpr
  (CObject
    'num
    (some (MetaNum n))))


