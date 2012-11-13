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
                           (CReturn (CBuiltinPrim 'num+ 
                                         (list 
                                           (CId 'self) 
                                           (CId 'other))))))
               (def '__sub__ 
                    (CFunc (list 'self 'other) 
                           (CReturn (CBuiltinPrim 'num-
                                         (list 
                                           (CId 'self) 
                                           (CId 'other))))))
               (def '__mult__ 
                    (CFunc (list 'self 'other) 
                           (CReturn (CBuiltinPrim 'num* 
                                         (list 
                                           (CId 'self) 
                                           (CId 'other))))))
               (def '__str__
                   (CFunc (list 'self)
                          (CReturn (CBuiltinPrim 'str
                                        (list (CId 'self))))))
               (def '__eq__
                    (CFunc (list 'self 'other)
                           (CReturn (CBuiltinPrim 'num=
                                                  (list
                                                    (CId 'self)
                                                    (CId 'other))))))
               (def '__gt__
                    (CFunc (list 'self 'other)
                           (CReturn (CBuiltinPrim 'num>
                                                  (list
                                                    (CId 'self)
                                                    (CId 'other))))))
               (def '__lt__
                    (CFunc (list 'self 'other)
                           (CReturn (CBuiltinPrim 'num<
                                                  (list
                                                    (CId 'self)
                                                    (CId 'other))))))))))

(define (make-builtin-num [n : number]) : CExpr
  (CObject
    'num
    (some (MetaNum n))))


