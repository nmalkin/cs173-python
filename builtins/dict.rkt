#lang plai-typed

(require "../python-core-syntax.rkt")
(require "../util.rkt")

(define dict-class : CExpr
  (CClass
   'dict
   'object
   (seq-ops (list 
              (def '__len__
                    (CFunc (list 'self)
                           (CReturn (CBuiltinPrim 'dict-len
                                                  (list
                                                   (CId 'self))))))
              (def '__str__
                   (CFunc (list 'self)
                          (CReturn (CBuiltinPrim 'dict-str
                                                     (list (CId 'self))))))
))))


(define (dict-len (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict
               (some (VObject 'num
                              (some (MetaNum (length (hash-keys (MetaDict-contents mval1)))))
                              (hash empty)))))

(define (dict-str (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict
               (some (VObject 'str 
                        (some (MetaStr
                                (pretty-metaval mval1)))
                        (make-hash empty)))))

