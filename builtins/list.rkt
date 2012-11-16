#lang plai-typed

(require "../python-core-syntax.rkt")
(require "../util.rkt")

(define list-class : CExpr
  (CClass
   'list
   'object
   (seq-ops (list (def '__add__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'list+
                                                  (list
                                                   (CId 'self)
                                                   (CId 'other))))))
                  (def '__init__
                       (CFunc (list 'self 'other) (none) 
                              (CAssign
                                (CId 'self)
                                (CApp (CGetField (CId 'other) '__list__)
                                             (list (CId 'other))
                                             (none)))))

                  (def '__len__
                    (CFunc (list 'self) (none)
                           (CReturn (CBuiltinPrim 'list-len
                                                  (list
                                                   (CId 'self))))))
                  (def '__list__
                       (CFunc (list 'self) (none)
                              (CReturn (CBuiltin 'list-cpy
                                                 (list 
                                                   (CId 'self))))))
                  (def '__in__
                    (CFunc (list 'self 'test) (none)
                           (CReturn (CBuiltinPrim 'list-in
                                                  (list
                                                   (CId 'self)
                                                   (CId 'test)
                                                   )))))
                  (def '__str__
                       (CFunc (list 'self) (none)
                              (CReturn (CBuiltinPrim 'list-str
                                                     (list (CId 'self))))))
                  (def '__attr__
                    (CFunc (list 'self 'idx) (none)
                           (CReturn (CBuiltinPrim 'list-attr
                                                  (list
                                                   (CId 'self)
                                                   (CId 'idx))))))))))

(define (make-builtin-list [l : (listof CVal)]) : CVal
  (VObject 'list
           (some (MetaList l))
           (make-hash empty)))

(define (list+ (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'list 'list
               (some (VObject 'list
                              (some (MetaList
                                     (append (MetaList-v mval1)
                                             (MetaList-v mval2))))
                              (hash empty)))))

(define (list-len (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'list
               (some (VObject 'num
                              (some (MetaNum (length (MetaList-v mval1))))
                              (hash empty)))))
(define (list-cpy [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof
                                                                        CVal)
  (check-types args env sto 'list
       (some (make-builtin-list (MetaList-v mval1)))))

(define (list-in [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
 (letrec ([self-list (MetaList-v (some-v (VObject-mval (first args))))]
          [test (second args)]
          [contains (lambda ([lst : (listof CVal)] [val : CVal]) : CVal
                    (cond
                     [(empty? lst) false-val]
                     [(cons? lst)
                       (if (equal? val (first lst))
                         true-val
                         (contains (rest lst) val))]))])
   (some (contains self-list test))))

(define (list-attr (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  ; here we'll eventually need to support slicin' and dicin' bro
  (check-types args env sto 'list 'num
               (some (list-ref (MetaList-v mval1) (MetaNum-n mval2)))))

(define (list-str (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'list
               (some (VObject 'str 
                        (some (MetaStr
                                (pretty-metaval mval1)))
                        (make-hash empty)))))
