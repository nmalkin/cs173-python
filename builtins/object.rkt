#lang plai-typed

;; object - the base-class of everything
(require "../python-core-syntax.rkt" 
         "../util.rkt"
         (typed-in racket/base (string-length : (string -> number))))

(define object-class
  (CClass 
    'object
    'none 
    (seq-ops (list
               (def '__init__ 
                    (CFunc (list 'self) (CId 'self)))))))

;; produces VTrue if the object is truthy and VFalse if it is not
(define (truthy-object? [o : CVal]) : CVal
  (if (some? (VObject-mval o))
    (let ([mval (some-v (VObject-mval o))])
      (type-case MetaVal mval
                 [MetaNum (n) (if (= n 0) (VFalse) (VTrue))]
                 [MetaStr (s) (if (= (string-length s) 0) (VFalse) (VTrue))]
                 [else (VTrue)]))
    ;;TODO: CHECK FOR EXISTENCE OF __BOOL__ AND __LEN__ AND DISPATCH TO THEM FOR
    ;;USER DEFINED CLASS
    (VTrue)))


