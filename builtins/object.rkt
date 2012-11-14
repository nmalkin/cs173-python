#lang plai-typed

;; object - the base-class of everything
(require "../python-core-syntax.rkt" 
         "../util.rkt"
         "num.rkt"
         "str.rkt"
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

               (def '__str__ 
                    (CFunc (list 'self) 
                           (CReturn (CBuiltinPrim 'obj-str (list (CId
                                                                   'self))))))

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
                                                  (make-builtin-num 0)))))))
               (def '__lt__
                    (CFunc (list 'self 'other)
                           (CSeq (CAssign (CId '_cmpresult)
                                    (CApp (CGetField (CId 'self) '__cmp__)
                                          (list (CId 'self) (CId 'other))))
                                 (CReturn (CApp (CGetField (CId '_cmpresult) '__lt__)
                                            (list (CId '_cmpresult)
                                                  (make-builtin-num 0)))))))
               (def '__lt__
                    (CFunc (list 'self 'other)
                           (CSeq (CAssign (CId '_cmpresult)
                                    (CApp (CGetField (CId 'self) '__cmp__)
                                          (list (CId 'self) (CId 'other))))
                                 (CReturn (CApp (CGetField (CId '_cmpresult) '__lt__)
                                            (list (CId '_cmpresult)
                                                  (make-builtin-num 0)))))))
               (def '__lte__
                    (CFunc (list 'self 'other)
                           (CSeq (CAssign (CId '_cmpresult)
                                    (CApp (CGetField (CId 'self) '__cmp__)
                                          (list (CId 'self) (CId 'other))))
                                 (CReturn (CApp (CGetField (CId '_cmpresult)
                                                           '__lte__)
                                            (list (CId '_cmpresult)
                                                  (make-builtin-num 0)))))))
               (def '__gte__
                    (CFunc (list 'self 'other)
                           (CSeq (CAssign (CId '_cmpresult)
                                    (CApp (CGetField (CId 'self) '__cmp__)
                                          (list (CId 'self) (CId 'other))))
                                 (CReturn (CApp (CGetField (CId '_cmpresult)
                                                           '__gte__)
                                            (list (CId '_cmpresult)
                                                  (make-builtin-num 0)))))))))))


;; produces VTrue if the object is truthy and VFalse if it is not
(define (truthy-object? [o : CVal]) : CVal
  (if (some? (VObject-mval o))
    (let ([mval (some-v (VObject-mval o))])
      (type-case MetaVal mval
                 [MetaNum (n) (if (= n 0) (VFalse) (VTrue))]
                 [MetaStr (s) (if (= (string-length s) 0) (VFalse) (VTrue))]
                 [MetaList (v) (if (= (length v) 0) (VFalse) (VTrue))]
                 [else (VTrue)]))
    (VTrue)))

(define (obj-str (args : (listof CVal))) : (optionof CVal)
  (local [(define o (first args))]
         (type-case CVal o
            [VObject (ante mval d)
                     (some (VObject 'str 
                        (some (MetaStr
                       (string-append "<instance of " 
                           (string-append 
                             (if (symbol=? ante 'none)
                               "Object"
                               (symbol->string ante))
                             ">")))) (make-hash empty)))]
            [else (error 'obj-str "Non object")])))



