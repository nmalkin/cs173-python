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
))))

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
                       (if (is? val (first lst))
                         (VTrue)
                         (contains (rest lst) val))]))])
   (some (contains self-list test))))
