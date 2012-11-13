#lang plai-typed

;; object - the base-class of everything
(require "../python-core-syntax.rkt" 
         "../util.rkt"
         "num.rkt"
         (typed-in racket/base (string-length : (string -> number))))

(define object-class
  (CClass 
    'object
    'none 
    (seq-ops (list
               (def '__init__ 
                    (CFunc (list 'self) (CId 'self)))
               (def '__eq__
                    (CFunc (list 'self 'other)
                           (CReturn (CBuiltinPrim 'eq
                                    (list 
                                      (CId 'self)
                                      (CId 'other))))))
               (def '__cmp__
                    (CFunc (list 'self 'other)
                           ;TODO: MAKE THIS AN EXCEPTION
                           (CError (CStr "NotImplemented"))))

               (def '__gt__
                    (CFunc (list 'self 'other)
                           (CSeq (CAssign (CId '_cmpresult)
                                    (CApp (CGetField (CId 'self) '__cmp__)
                                          (list (CId 'self) (CId 'other))))
                                 (CReturn (CApp (CGetField (CId '_cmpresult) '__gt__)
                                            (list (CId '_cmpresult)
                                                  (make-builtin-num 0)))))))))))


;; produces VTrue if the object is truthy and VFalse if it is not
(define (truthy-object? [o : CVal]) : CVal
  (if (some? (VObject-mval o))
    (let ([mval (some-v (VObject-mval o))])
      (type-case MetaVal mval
                 [MetaNum (n) (if (= n 0) (VFalse) (VTrue))]
                 [MetaStr (s) (if (= (string-length s) 0) (VFalse) (VTrue))]
                 [else (VTrue)]))
    (VTrue)))


