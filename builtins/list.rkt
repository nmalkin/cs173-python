#lang plai-typed

(require "../python-core-syntax.rkt"
         "../util.rkt"
         "num.rkt"
         "none.rkt")

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
                              (CReturn (CBuiltinPrim 'list-cpy
                                                 (list 
                                                   (CId 'self))))))
                  (def '__in__
                    (CFunc (list 'self 'test) (none)
                           (CReturn (CBuiltinPrim 'list-in
                                                  (list
                                                   (CId 'self)
                                                   (CId 'test))))))
                  (def '__str__
                       (CFunc (list 'self) (none)
                              (CReturn (CBuiltinPrim 'list-str
                                                     (list (CId 'self))))))
                  (def '__attr__
                    (CFunc (list 'self 'idx) (none)
                           (CReturn (CBuiltinPrim 'list-attr
                                                  (list
                                                   (CId 'self)
                                                   (CId 'idx))))))
                  (def '__cmp__
                    (CFunc (list 'self 'other) (none)
                           (CLet 'listcmp (CNone)
                             (seq-ops (list
                               (def 'listcmp
                                    (CFunc (list 'self 'other 'idx) (none)
                                           (seq-ops (list
                                             (def 'li1
                                                  (CApp (CGetField (CId 'self)
                                                                   '__attr__)
                                                        (list (CId 'self)
                                                              (CId 'idx))
                                                        (none)))
                                             (def 'li2
                                                  (CApp (CGetField (CId 'other)
                                                                   '__attr__)
                                                        (list (CId 'other)
                                                              (CId 'idx))
                                                        (none)))
                                             (CIf (CPrim2 'Is (CId 'li1) (CNone))
                                                  (CIf (CPrim2 'Is (CId 'li2) (CNone))
                                                       (CReturn (make-builtin-num 0))
                                                       (CReturn (make-builtin-num -1)))
                                                  (CIf (CPrim2 'Is (CId 'li2) (CNone))
                                                       (CReturn (make-builtin-num 1))
                                           (seq-ops (list
                                             (def 'cmpval
                                                  (CApp (CGetField (CId 'li1)
                                                                   '__cmp__)
                                                        (list (CId 'li1)
                                                              (CId 'li2))
                                                        (none)))
                                             (CIf (CApp (CGetField (CId 'cmpval)
                                                                   '__eq__)
                                                        (list (CId 'cmpval)
                                                              (make-builtin-num 0))
                                                        (none))
                                                  (seq-ops (list 
                                                    (def 'nidx
                                                         (CApp (CGetField (CId 'idx)
                                                                          '__add__)
                                                               (list (CId 'idx)
                                                                     (make-builtin-num 1))
                                                               (none)))
                                                    (CReturn 
                                                      (CApp (CId 'listcmp)
                                                          (list (CId 'self)
                                                                (CId 'other)
                                                                (CId 'nidx))
                                                          (none)))))
                                                  (CReturn (CId 'cmpval)))))))))))
                               (CReturn 
                                 (CApp (CId 'listcmp)
                                     (list (CId 'self)
                                           (CId 'other)
                                           (make-builtin-num 0))
                                     (none))))))))
                  (def '__eq__
                    (CFunc (list 'self 'other) (none)
                           (seq-ops (list
                                      (def '_cmpresult
                                           (CApp (CGetField (CId 'self) '__cmp__)
                                                 (list (CId 'self) (CId 'other))
                                                 (none)))
                                      (CReturn (CApp (CGetField (CId '_cmpresult) '__eq__)
                                                     (list (CId '_cmpresult)
                                                           (make-builtin-num 0))
                                                     (none)))))))))))
                           #|(CLet 'listeq (CNone)
                             (seq-ops (list
                               (def 'listeq
                                    (CFunc (list 'self 'other 'idx) (none)
                                           (seq-ops (list
                                             (def 'li1
                                                  (CApp (CGetField (CId 'self)
                                                                   '__attr__)
                                                        (list (CId 'self)
                                                              (CId 'idx))
                                                        (none)))
                                             (def 'li2
                                                  (CApp (CGetField (CId 'other)
                                                                   '__attr__)
                                                        (list (CId 'other)
                                                              (CId 'idx))
                                                        (none)))
                                             (CIf (CPrim2 'Is (CId 'li1) (CNone))
                                                  (CIf (CPrim2 'Is (CId 'li2) (CNone))
                                                       (CReturn (CTrue))
                                                       (CReturn (CFalse)))
                                                  (CIf (CPrim2 'Is (CId 'li2) (CNone))
                                                       (CReturn (CFalse))
                                           (seq-ops (list
                                             (def 'eqval
                                                  (CApp (CGetField (CId 'li1)
                                                                   '__eq__)
                                                        (list (CId 'li1)
                                                              (CId 'li2))
                                                        (none)))
                                             (CIf (CApp (CGetField (CId 'eqval)
                                                                   '__eq__)
                                                        (list (CId 'eqval)
                                                              (CTrue))
                                                        (none))
                                                  (seq-ops (list 
                                                    (def 'nidx
                                                         (CApp (CGetField (CId 'idx)
                                                                          '__add__)
                                                               (list (CId 'idx)
                                                                     (make-builtin-num 1))
                                                               (none)))
                                                    (CReturn 
                                                      (CApp (CId 'listeq)
                                                          (list (CId 'self)
                                                                (CId 'other)
                                                                (CId 'nidx))
                                                          (none)))))
                                                  (CReturn (CId 'eqval)))))))))))
                               (CReturn 
                                 (CApp (CId 'listeq)
                                       (list (CId 'self)
                                           (CId 'other)
                                           (make-builtin-num 0))
                                     (none))))))))))))|#

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
(define (list-cpy [args : (listof CVal)]
                  [env : Env]
                  [sto : Store]) : (optionof CVal)
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
               (some 
                 (try
                   (list-ref (MetaList-v mval1) (MetaNum-n mval2))
                   (lambda ()
                     vnone)))))

(define (list-str (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'list
               (some (VObject 'str 
                        (some (MetaStr
                                (pretty-metaval mval1)))
                        (make-hash empty)))))
