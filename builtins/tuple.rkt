#lang plai-typed

(require "../python-core-syntax.rkt")
(require "../util.rkt"
         "list.rkt")

(define tuple-class : CExpr
  (CClass
   'tuple
   'object
   (seq-ops (list (def '__add__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'tuple+
                                                  (list
                                                   (CId 'self (LocalId))
                                                   (CId 'other (LocalId)))))))

                   (def '__init__
                        (CFunc (list 'self) (some 'args)
                          (CReturn
                          (CIf ; Did we get any args?
                            (CBuiltinPrim 'num=
                                          (list
                                            (CApp (CGetField (CId 'args (LocalId)) '__len__)
                                                  (list (CId 'args (LocalId)))
                                                  (none))
                                            (CObject 'num (some (MetaNum 0)))))
                            ; No. Return an empty tuple.
                            (CTuple empty)
                            ; Yes. Call __tuple__ on the first argument.
                            (CLet 'first-arg
                                  (CApp (CGetField (CId 'args (LocalId)) '__attr__)
                                        (list (CId 'args (LocalId))
                                              (CObject 'num (some (MetaNum 0))))
                                        (none))
                                  (CApp (CGetField (CId 'first-arg (LocalId)) '__tuple__)
                                        (list (CId 'first-arg (LocalId)))
                                        (none)))))))

                  (def '__mult__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'tuple*
                                                  (list
                                                   (CId 'self (LocalId))
                                                   (CId 'other (LocalId)))))))
                  (def '__len__
                    (CFunc (list 'self) (none)
                           (CReturn (CBuiltinPrim 'tuple-len
                                                  (list
                                                   (CId 'self (LocalId)))))))
                  (def '__in__
                    (CFunc (list 'self 'test) (none)
                           (CReturn (CBuiltinPrim 'tuple-in
                                                  (list
                                                   (CId 'self (LocalId))
                                                   (CId 'test (LocalId))
                                                   )))))
                  (def '__list__
                     (CFunc (list 'self) (none)
                            (CReturn (CBuiltinPrim 'tuple-list
                                         (list
                                           (CId 'self (LocalId)))))))

                  (def '__tuple__
                     (CFunc (list 'self) (none)
                            (CReturn (CBuiltinPrim 'tuple-tuple
                                         (list
                                           (CId 'self (LocalId)))))))

                  (def '__str__
                       (CFunc (list 'self) (none)
                              (CReturn (CBuiltinPrim 'tuple-str
                                                     (list (CId 'self (LocalId)))))))
                  (def '__attr__
                    (CFunc (list 'self 'idx) (none)
                           (CReturn (CBuiltinPrim 'tuple-attr
                                                  (list
                                                   (CId 'self (LocalId))
                                                   (CId 'idx (LocalId)))))))
))))

(define (make-builtin-tuple [l : (listof CVal)]) : CVal
  (VObject 'tuple
           (some (MetaTuple l))
           (make-hash empty)))

;; convert a tuple to a list
(define (tuple-list (args : (listof CVal)) 
                    [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'tuple
              (some 
                (make-builtin-list (MetaTuple-v mval1)))))

; returns the tuple itself
; that way, tuple.__tuple__() "is" itself
(define (tuple-tuple (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'tuple
               (some (first args))))

(define (tuple+ (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'tuple 'tuple
               (some (VObject 'tuple
                              (some (MetaTuple
                                     (append (MetaTuple-v mval1)
                                             (MetaTuple-v mval2))))
                              (hash empty)))))

(define (tuple* (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'tuple 'num
               (letrec ([tuple-list (MetaTuple-v mval1)]
                        [repetitions (MetaNum-n mval2)]
                        [repeat (lambda ([lst : (listof CVal)] [reps : number]) : (listof CVal)
                                  (cond
                                    [(= reps 0) (list)]
                                    [(= reps 1) lst]
                                    [(> reps 1) (append lst (repeat lst (sub1 reps)))]))])
               (some (VObject 'tuple
                              (some (MetaTuple
                                     (repeat tuple-list repetitions)))
                              (hash empty))))))

(define (tuple-len (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'tuple
               (some (VObject 'num
                              (some (MetaNum (length (MetaTuple-v mval1))))
                              (hash empty)))))

(define (tuple-in [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
 (letrec ([self-list (MetaTuple-v (some-v (VObject-mval (first args))))]
          [test (second args)]
          [contains (lambda ([lst : (listof CVal)] [val : CVal]) : CVal
                    (cond
                     [(empty? lst) false-val]
                     [(cons? lst)
                       (if (equal? val (first lst))
                         true-val
                         (contains (rest lst) val))]))])
   (some (contains self-list test))))

(define (tuple-attr (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  ; TODO: slicing
  (check-types args env sto 'tuple 'num
               (some (list-ref (MetaTuple-v mval1) (MetaNum-n mval2)))))

(define (tuple-str (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'tuple
               (some (VObject 'str
                        (some (MetaStr
                                (pretty-metaval mval1)))
                        (make-hash empty)))))

