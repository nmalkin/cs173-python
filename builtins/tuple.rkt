#lang plai-typed

(require "../python-core-syntax.rkt")
(require "../util.rkt")

(define tuple-class : CExpr
  (CClass
   'tuple
   'object
   (seq-ops (list (def '__add__
                    (CFunc (list 'self 'other)
                           (CReturn (CBuiltinPrim 'tuple+
                                                  (list
                                                   (CId 'self)
                                                   (CId 'other))))))
                  (def '__mult__
                    (CFunc (list 'self 'other)
                           (CReturn (CBuiltinPrim 'tuple*
                                                  (list
                                                   (CId 'self)
                                                   (CId 'other))))))
                  (def '__len__
                    (CFunc (list 'self)
                           (CReturn (CBuiltinPrim 'tuple-len
                                                  (list
                                                   (CId 'self))))))
                  (def '__in__
                    (CFunc (list 'self 'test)
                           (CReturn (CBuiltinPrim 'tuple-in
                                                  (list
                                                   (CId 'self)
                                                   (CId 'test)
                                                   )))))
                  (def '__str__
                       (CFunc (list 'self)
                              (CReturn (CBuiltinPrim 'tuple-str
                                                     (list (CId 'self))))))
                  (def '__attr__
                    (CFunc (list 'self 'idx)
                           (CReturn (CBuiltinPrim 'tuple-attr
                                                  (list
                                                   (CId 'self)
                                                   (CId 'idx))))))
))))

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
