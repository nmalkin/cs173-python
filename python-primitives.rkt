#lang plai-typed

(require "python-core-syntax.rkt"
         "util.rkt"
         "builtins/str.rkt"
         "builtins/list.rkt"
         (typed-in racket/string (string-join : ((listof string) string -> string)))
         (typed-in racket/base (number->string : (number -> string))))

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
    [VStr (s) (string-append "'" (string-append s "'"))]
    [VTrue () "True"]
    [VFalse () "False"]
    [VDict (contents) (dict-str contents)]
    [VNone () "None"]
    [VObject (a mval d) (if (some? mval)
                            (pretty-metaval (some-v mval))
                            "Can't print non-builtin object.")]
    [VClosure (env args body) (error 'pretty "Can't print closures yet")]
    ;[else (error 'pretty (string-append "Cannot print case: " (to-string arg)))]
    ))

(define (pretty-metaval mval)
  (type-case MetaVal mval
    [MetaNum (n) (number->string n)]
    [MetaStr (s) s]
    [MetaClass () "Class"]
    [MetaList (l) (string-append 
                   (string-append "[" 
                                  (string-join (map pretty l) ", "))
                   "]")]))

(define (print arg)
  (display (pretty arg)))

(define (python-prim1 op arg)
  (case op
    [(print) (begin (print arg) arg)]))

(define (builtin-prim [op : symbol] [args : (listof CVal)]) : (optionof CVal)
  (case op
    ['num+ (let ([arg1 (first args)]
                 [arg2 (second args)]) 
             (if (and (VObject? arg1) (VObject? arg2)
                      (symbol=? (VObject-antecedent arg1) 'num)
                      (symbol=? (VObject-antecedent arg2) 'num))
                 (let ([mayb-mval1 (VObject-mval arg1)] 
                       [mayb-mval2 (VObject-mval arg2)]) 
                   (if (and (some? mayb-mval1) (some? mayb-mval2))
                       (let ([mval1 (some-v mayb-mval1)]
                             [mval2 (some-v mayb-mval2)])
                         (some (VObject 'num (some (MetaNum 
                                                    (+ (MetaNum-n mval1) 
                                                       (MetaNum-n mval2))))
                                        (make-hash empty))))
                       (none)))
                 (none)))]
    ['str (let ([arg (first args)])
            (some (VStr (number->string (MetaNum-n (some-v 
                                                    (VObject-mval arg)))))))]
    ['str+ (str+ args)]
    ['list+ (list+ args)]
    ))


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

