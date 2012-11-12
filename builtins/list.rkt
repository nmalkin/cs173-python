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
))))

(define (list+ (args : (listof CVal))) : (optionof CVal)
  (check-types args 'list 'list
               (some (VObject 'list
                              (some (MetaList
                                     (append (MetaList-l mval1)
                                             (MetaList-l mval2))))
                              (hash empty)))))

(define (list-len (args : (listof CVal))) : (optionof CVal)
  (check-types args 'list
               (some (VObject 'num
                              (some (MetaNum (length (MetaList-l mval1))))
                              (hash empty)))))

