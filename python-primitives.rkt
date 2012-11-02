#lang plai-typed

(require "python-core-syntax.rkt"
         (typed-in racket/string (string-join : ((listof string) string -> string))))

#|

Since there may end up being a large number of primitives that you
implement for python, here is a suggested factoring into a separate
file.  You can add new primitives here by adding new symbols to the
dispatch.  You might also choose to add more than single-arity
primitives here.

|#

(require (typed-in racket/base [display : (string -> void)]))

(define (pretty arg)
  (type-case CVal arg
    [VNum (n) (to-string n)]
    [VStr (s) (string-append "'" (string-append s "'"))]
    [VTrue () "True"]
    [VFalse () "False"]
    [VDict (contents) (dict-str contents)]
    [VNone () "None"]
    [VClosure (env args body) (error 'pretty "Can't print closures yet")]
    [VClass (b d) (string-append "class: " (string-append (to-string d) "\n"))]
    [else (error 'pretty (string-append "Cannot print case: " (to-string arg)))]))
  

(define (print arg)
  (display (pretty arg)))

(define (python-prim1 op arg)
  (case op
    [(print) (begin (print arg) arg)]))

(define (dict-str (contents : (hashof CVal CVal)))
  (string-append "{" 
                (string-append
                 (string-join
                       (map (lambda(k)
                              (string-append (pretty k)
                                             (string-append 
                                                 ": "
                                                 (pretty (some-v (hash-ref contents
                                                                   k))))))
                              (hash-keys contents))
                       ", ")
                 "}")))

