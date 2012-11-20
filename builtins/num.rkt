#lang plai-typed

(require "../python-core-syntax.rkt")
(require "../util.rkt")

(define (num-class [name : symbol]) : CExpr
  (CClass
    name
    'object
    (seq-ops (list 
               (def '__add__ 
                    (CFunc (list 'self 'other)  (none)
                           (CReturn (CBuiltinPrim 'num+ 
                                         (list 
                                           (CId 'self) 
                                           (CId 'other))))))
               (def '__sub__ 
                    (CFunc (list 'self 'other)  (none)
                           (CReturn (CBuiltinPrim 'num-
                                         (list 
                                           (CId 'self) 
                                           (CId 'other))))))

               (def '__mult__ 
                    (CFunc (list 'self 'other)  (none)
                           (CReturn (CBuiltinPrim 'num* 
                                         (list 
                                           (CId 'self) 
                                           (CId 'other))))))
               (def '__div__ 
                    (CFunc (list 'self 'other)  (none)
                           (CIf (CApp (CGetField (CId 'other) '__eq__) 
                                       (list (CId 'other) (make-builtin-num 0))
                                       (none))
                                (CRaise (some (make-exception 'ZeroDivisionError
                                                              "Divided by 0")))
                                (CReturn (CBuiltinPrim 'num/
                                              (list 
                                                (CId 'self) 
                                                (CId 'other)))))))

               (def '__str__
                   (CFunc (list 'self) (none)
                          (CReturn (CBuiltinPrim 'num-str
                                        (list (CId 'self))))))
               (def '__eq__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'num=
                                                  (list
                                                    (CId 'self)
                                                    (CId 'other))))))
               (def '__gt__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'num>
                                                  (list
                                                    (CId 'self)
                                                    (CId 'other))))))
               (def '__lt__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'num<
                                                  (list
                                                    (CId 'self)
                                                    (CId 'other))))))
               (def '__gte__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'num>=
                                                  (list
                                                    (CId 'self)
                                                    (CId 'other))))))
               (def '__invrt__
                    (CFunc  (list 'self) (none)
                      (CReturn (CBuiltinPrim 'num-
                                             (list 
                                               (make-builtin-num 0) 
                                               (CBuiltinPrim 'num+
                                                   (list (CId 'self) 
                                                         (make-builtin-num
                                                           1))))))))



               (def '__abs__
                    (CFunc (list 'self) (none)
                           (CIf (CBuiltinPrim 'num< 
                                              (list 
                                                (CId 'self)
                                                (make-builtin-num 0)))
                                (CReturn (CBuiltinPrim 'num-
                                                       (list (make-builtin-num 0)
                                                           (CId 'self))))
                                (CReturn (CBuiltinPrim 'num+
                                                       (list (make-builtin-num 0)
                                                           (CId 'self)))))))
               (def '__lte__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'num<=
                                                  (list
                                                    (CId 'self)
                                                    (CId 'other))))))))))

(define (make-builtin-num [n : number]) : CExpr
  (CObject
    'num
    (some (MetaNum n))))


