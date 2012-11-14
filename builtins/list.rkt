#lang plai-typed

(require "../python-core-syntax.rkt")
(require "../util.rkt")

(define list-class : CExpr
  (CClass
   'list
   'object
   (seq-ops (list (def '__add__
                    (CFunc (list 'self 'other)
                           (CReturn (CBuiltinPrim 'list+
                                                  (list
                                                   (CId 'self)
                                                   (CId 'other))))))
                  (def '__len__
                    (CFunc (list 'self)
                           (CReturn (CBuiltinPrim 'list-len
                                                  (list
                                                   (CId 'self))))))
                  (def '__in__
                    (CFunc (list 'self 'test)
                           (CReturn (CBuiltinPrim 'list-in
                                                  (list
                                                   (CId 'self)
                                                   (CId 'test)
                                                   )))))
                  (def '__str__
                       (CFunc (list 'self)
                              (CReturn (CBuiltinPrim 'list-str
                                                     (list (CId 'self))))))
                  (def '__attr__
                    (CFunc (list 'self 'idx)
                           (CReturn (CBuiltinPrim 'list-attr
                                                  (list
                                                   (CId 'self)
                                                   (CId 'idx))))))))))

(define (list+ (args : (listof CVal))) : (optionof CVal)
  (check-types args 'list 'list
               (some (VObject 'list
                              (some (MetaList
                                     (append (MetaList-v mval1)
                                             (MetaList-v mval2))))
                              (hash empty)))))

(define (list-len (args : (listof CVal))) : (optionof CVal)
  (check-types args 'list
               (some (VObject 'num
                              (some (MetaNum (length (MetaList-v mval1))))
                              (hash empty)))))

(define (list-in [args : (listof CVal)]) : (optionof CVal)
 (letrec ([self-list (MetaList-v (some-v (VObject-mval (first args))))]
          [test (second args)]
          [contains (lambda ([lst : (listof CVal)] [val : CVal]) : CVal
                    (cond
                     [(empty? lst) (VFalse)]
                     [(cons? lst)
                       (if (equal? val (first lst))
                         (VTrue)
                         (contains (rest lst) val))]))])
   (some (contains self-list test))))

(define (list-attr (args : (listof CVal))) : (optionof CVal)
  ; here we'll eventually need to support slicin' and dicin' bro
  (check-types args 'list 'num
               (some (list-ref (MetaList-v mval1) (MetaNum-n mval2)))))

(define (list-str (args : (listof CVal))) : (optionof CVal)
  (check-types args 'list
               (some (VObject 'str 
                        (some (MetaStr
                                (pretty-metaval mval1)))
                        (make-hash empty)))))
