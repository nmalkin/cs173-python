#lang plai-typed

(require "../python-core-syntax.rkt")
(require "../util.rkt")

(define dict-class : CExpr
  (CClass
   'dict
   'object
   (seq-ops (list 
              (def '__len__
                    (CFunc (list 'self) (none)
                           (CReturn (CBuiltinPrim 'dict-len
                                                  (list
                                                   (CId 'self))))))
              (def '__str__
                   (CFunc (list 'self) (none)
                          (CReturn (CBuiltinPrim 'dict-str
                                                     (list (CId 'self))))))

              (def 'clear
                   (CFunc (list 'self) (none)
                          (CReturn (CBuiltinPrim 'dict-clear
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

(define (dict-clear (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict
               (let ([contents (MetaDict-contents mval1)])
                 (begin
                   ; remove all key-value pairs from hash
                   (map (lambda (key) (hash-remove! contents key))
                        (hash-keys contents))
                   (some (VNone))))))

