#lang plai-typed

(require "../python-core-syntax.rkt")
(require "../util.rkt"
         (typed-in racket/base (integer? : (number -> boolean))))

(define (make-builtin-num [n : number]) : CExpr
  (CObject
    (if (integer? n)
      'int
      'float)
    (some (MetaNum n))))

(define int-class
  (CClass
    'int
    'num

    (seq-ops (list
               (def '__init__
                    (CFunc (list 'self 'other) (none)
                        (CAssign (CId 'self)
                            (CApp (CGetField (CId 'other) '__int__)
                                  (list (CId 'other))
                                  (none)))))))))

(define float-class
  (CClass
    'int
    'num
    (seq-ops (list
               (def '__init__
                    (CFunc (list 'self 'other) (none)
                        (CAssign (CId 'self)
                            (CApp (CGetField (CId 'other) '__float__)
                                  (list (CId 'other))
                                  (none)))))))))

(define num-class 
  (CClass
    'num
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
               (def '__floordiv__ 
                    (CFunc (list 'self 'other)  (none)
                           (CIf (CApp (CGetField (CId 'other) '__eq__) 
                                       (list (CId 'other) (make-builtin-num 0))
                                       (none))
                                (CRaise (some (make-exception 'ZeroDivisionError
                                                              "Divided by 0")))
                                (CReturn (CBuiltinPrim 'num//
                                              (list 
                                                (CId 'self) 
                                                (CId 'other)))))))
               (def '__mod__ 
                    (CFunc (list 'self 'other)  (none)
                           (CIf (CApp (CGetField (CId 'other) '__eq__) 
                                       (list (CId 'other) (make-builtin-num 0))
                                       (none))
                                (CRaise (some (make-exception 'ZeroDivisionError
                                                              "Divided by 0")))
                                (CReturn (CBuiltinPrim 'num%
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
                                                    (CId 'other))))))
               (def '__cmp__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'numcmp
                                                  (list
                                                    (CId 'self)
                                                    (CId 'other))))))))))



