#lang plai-typed

(require "../python-core-syntax.rkt")
(require "../util.rkt")
(require [opaque-type-in racket/set [Set set?]])
(require
  (typed-in racket/set (set->list : (set? -> (listof 'a))))
  (typed-in racket/set (set? : ('a -> boolean))))

(define set-class : CExpr
  (CClass
   'set
   'object
   (seq-ops (list 
              (def '__len__
                    (CFunc (list 'self) (none)
                           (CReturn (CBuiltinPrim 'set-len
                                                  (list
                                                   (CId 'self))))))
              #|
              (def '__str__
                   (CFunc (list 'self) (none)
                          (CReturn (CBuiltinPrim 'set-str
                                                     (list (CId 'self))))))

              (def 'clear
                   (CFunc (list 'self) (none)
                          (CReturn (CBuiltinPrim 'set-clear
                                                     (list (CId 'self))))))

              (def 'update
                   (CFunc (list 'self 'other) (none)
                          (CReturn (CBuiltinPrim 'set-update
                                                     (list (CId 'self)
                                                           (CId 'other))))))

              (def '__in__
                (CFunc (list 'self 'other) (none)
                       (CReturn (CBuiltinPrim 'set-in
                                              (list
                                               (CId 'self)
                                               (CId 'other)
                                               )))))

              (def '__eq__
                (CFunc (list 'self 'other) (none)
                       (CReturn (CBuiltinPrim 'set-eq
                                              (list
                                               (CId 'self)
                                               (CId 'other)
                                               )))))
              |#
))))

(define (set-len (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'set
               (some (VObject 'num
                              (some (MetaNum (length (set->list (MetaSet-elts mval1)))))
                              (hash empty)))))
