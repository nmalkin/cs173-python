#lang plai-typed

(require "../python-core-syntax.rkt")
(require "../util.rkt")
(require [opaque-type-in racket/set [Set set?]])
(require
  (typed-in racket/set (set->list : (set? -> (listof 'a))))
  (typed-in racket/set (set? : ('a -> boolean)))
  (typed-in racket/set (set=? : (set? set? -> boolean)))
  (typed-in racket/set (set-member? : (set? 'a -> boolean)))
)

(define set-class : CExpr
  (CClass
   'set
   'object
   (seq-ops (list 
              (def '__len__
                    (CFunc (list 'self) (none)
                           (CReturn (CBuiltinPrim 'set-len
                                                  (list
                                                   (CId 'self (LocalId)))))))
              (def '__init__
                   (CFunc (list 'self) (none)
                          (CReturn (CBuiltinPrim 'set-init
                                                     (list (CId 'self (LocalId)))))))

              #|
              (def 'clear
                   (CFunc (list 'self) (none)
                          (CReturn (CBuiltinPrim 'set-clear
                                                     (list (CId 'self))))))

              (def 'update
                   (CFunc (list 'self 'other) (none)
                          (CReturn (CBuiltinPrim 'set-update
                                                     (list (CId 'self)
                                                           (CId 'other))))))
              |#

              (def '__in__
                (CFunc (list 'self 'other) (none)
                       (CReturn (CBuiltinPrim 'set-in
                                              (list
                                               (CId 'self (LocalId))
                                               (CId 'other (LocalId))
                                               )))))

              (def '__eq__
                (CFunc (list 'self 'other) (none)
                       (CReturn (CBuiltinPrim 'set-eq
                                              (list
                                               (CId 'self (LocalId))
                                               (CId 'other (LocalId))
                                               )))))
))))

(define (set-init (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
        (some (VObject 'set
                       (some (MetaSet (make-set empty)))
                       (hash empty))))

(define (set-len (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'set
               (some (VObject 'num
                              (some (MetaNum (length (set->list (MetaSet-elts mval1)))))
                              (hash empty)))))

(define (set-eq (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'set 'set
               (let ([self (MetaSet-elts mval1)]
                     [other (MetaSet-elts mval2)])
                 (if (set=? self other)
                     (some true-val)
                     (some false-val)))))

(define (set-in [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'set
               (let ([contents (MetaSet-elts mval1)])
                 (if (set-member? contents (second args)) ; FIXME: what if (second args) DNE?
                     (some true-val)
                     (some false-val)))))
