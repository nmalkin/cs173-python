#lang plai-typed

;; object - the base-class of everything
(require "../python-core-syntax.rkt")
(require "../util.rkt")

(define object-class
  (CClass 
    'object
    'none 
    (seq-ops (list
               (def '__init__ 
                    (CFunc (list 'self) (CId 'self)))))))

