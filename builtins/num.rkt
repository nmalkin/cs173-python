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
                                           (CId 'self (LocalId)) 
                                           (CId 'other (LocalId)))))))
               (def '__sub__ 
                    (CFunc (list 'self 'other)  (none)
                           (CReturn (CBuiltinPrim 'num-
                                         (list 
                                           (CId 'self (LocalId)) 
                                           (CId 'other (LocalId)))))))

               (def '__mult__ 
                    (CFunc (list 'self 'other)  (none)
                           (CReturn (CBuiltinPrim 'num* 
                                         (list 
                                           (CId 'self (LocalId)) 
                                           (CId 'other (LocalId)))))))
               (def '__div__ 
                    (CFunc (list 'self 'other)  (none)
                           (CIf (CApp (CGetField (CId 'other (LocalId)) '__eq__) 
                                       (list (CId 'other (LocalId)) (make-builtin-num 0))
                                       (none))
                                (CRaise (some (make-exception 'ZeroDivisionError
                                                              "Divided by 0")))
                                (CReturn (CBuiltinPrim 'num/
                                              (list 
                                                (CId 'self (LocalId)) 
                                                (CId 'other (LocalId))))))))
               (def '__floordiv__ 
                    (CFunc (list 'self 'other)  (none)
                           (CIf (CApp (CGetField (CId 'other (LocalId)) '__eq__) 
                                       (list (CId 'other (LocalId)) (make-builtin-num 0))
                                       (none))
                                (CRaise (some (make-exception 'ZeroDivisionError
                                                              "Divided by 0")))
                                (CReturn (CBuiltinPrim 'num//
                                              (list 
                                                (CId 'self (LocalId)) 
                                                (CId 'other (LocalId))))))))
               (def '__mod__ 
                    (CFunc (list 'self 'other)  (none)
                           (CIf (CApp (CGetField (CId 'other (LocalId)) '__eq__) 
                                       (list (CId 'other (LocalId)) (make-builtin-num 0))
                                       (none))
                                (CRaise (some (make-exception 'ZeroDivisionError
                                                              "Divided by 0")))
                                (CReturn (CBuiltinPrim 'num%
                                              (list 
                                                (CId 'self (LocalId)) 
                                                (CId 'other (LocalId))))))))
               (def '__str__
                   (CFunc (list 'self) (none)
                          (CReturn (CBuiltinPrim 'num-str
                                        (list (CId 'self (LocalId)))))))
               (def '__eq__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'num=
                                                  (list
                                                    (CId 'self (LocalId))
                                                    (CId 'other (LocalId)))))))
               (def '__gt__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'num>
                                                  (list
                                                    (CId 'self (LocalId))
                                                    (CId 'other (LocalId)))))))
               (def '__lt__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'num<
                                                  (list
                                                    (CId 'self (LocalId))
                                                    (CId 'other (LocalId)))))))
               (def '__gte__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'num>=
                                                  (list
                                                    (CId 'self (LocalId))
                                                    (CId 'other (LocalId)))))))
               (def '__invrt__
                    (CFunc  (list 'self) (none)
                      (CReturn (CBuiltinPrim 'num-
                                             (list 
                                               (make-builtin-num 0) 
                                               (CBuiltinPrim 'num+
                                                   (list (CId 'self (LocalId)) 
                                                         (make-builtin-num
                                                           1))))))))
               (def '__abs__
                    (CFunc (list 'self) (none)
                           (CIf (CBuiltinPrim 'num< 
                                              (list 
                                                (CId 'self (LocalId))
                                                (make-builtin-num 0)))
                                (CReturn (CBuiltinPrim 'num-
                                                       (list (make-builtin-num 0)
                                                           (CId 'self (LocalId)))))
                                (CReturn (CBuiltinPrim 'num+
                                                       (list (make-builtin-num 0)
                                                           (CId 'self (LocalId))))))))
               (def '__lte__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'num<=
                                                  (list
                                                    (CId 'self (LocalId))
                                                    (CId 'other (LocalId)))))))
               (def '__cmp__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'numcmp
                                                  (list
                                                    (CId 'self (LocalId))
                                                    (CId 'other (LocalId)))))))))))

(define (make-builtin-num [n : number]) : CExpr
  (CObject
    'num
    (some (MetaNum n))))


