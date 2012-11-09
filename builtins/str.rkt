#lang plai-typed

(require "../python-core-syntax.rkt")
(require "../util.rkt")

(define str-class : CExpr
  (CClass
   'str 
   'object
   (seq-ops (list (def '__add__
                    (CFunc (list 'self 'other)
                           (CReturn (CBuiltinPrim 'str+
                                                  (list
                                                   (CId 'self)
                                                   (CId 'other))))))
                  (def '__mult__
                    (CFunc (list 'self 'other)
                           (CBuiltinPrim 'str*
                                         (list
                                          (CId 'self)
                                          (CId 'other)))))
                  (def '__rmult__
                    (CFunc (list 'self 'other)
                           (CBuiltinPrim 'str*
                                         (list
                                          (CId 'self)
                                          (CId 'other)))))))))

(define (make-builtin-str [s : string]) : CExpr
  (CObject
   'str
   (some (MetaStr s))))

(define (str+ (args : (listof CVal))) : (optionof CVal)
  (check-types args 'str 'str
               (some (VObject 'str 
                              (some (MetaStr
                                     (string-append (MetaStr-s mval1)
                                                    (MetaStr-s mval2))))
                              (hash empty)))))

(define (str (args : (listof CVal))) : (optionof CVal)
  (check-types args 'str
               (some (VObject 'str
                              (some (MetaStr (MetaStr-s mval1)))
                              (hash empty)))))
