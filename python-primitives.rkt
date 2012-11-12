#lang plai-typed

(require "python-core-syntax.rkt"
         "util.rkt"
         "builtins/str.rkt"
         "builtins/list.rkt"
         "builtins/tuple.rkt"
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
    [VClosure (env args body) (error 'pretty "Can't print closures yet")]))

(define (pretty-metaval mval)
  (type-case MetaVal mval
    [MetaNum (n) (number->string n)]
    [MetaStr (s) s]
    [MetaClass (c) (symbol->string c)]
    [MetaList (v) (string-append
                   (string-append "["
                                  (string-join (map pretty v) ", "))
                   "]")]
    [MetaTuple (v) (string-append
                   (string-append "("
                                  (string-join (map pretty v) ", "))
                   ")")]))

(define (print arg)
  (display (pretty arg)))

(define (python-prim1 op arg)
  (case op
    [(print) (begin (print arg) arg)]))

(define (builtin-prim [op : symbol] [args : (listof CVal)]) : (optionof CVal)
  (case op
    ['num+ (check-types args 'num 'num 
                        (some (VObject 'num (some (MetaNum 
                                                    (+ (MetaNum-n mval1) 
                                                       (MetaNum-n mval2))))
                                        (make-hash empty))))]
    ['num= (check-types args 'num 'num 
                        (if (= (MetaNum-n mval1) (MetaNum-n mval2))
                          (some (VTrue))
                          (some (VFalse))))]
    ['num> (check-types args 'num 'num 
                        (if (> (MetaNum-n mval1) (MetaNum-n mval2))
                          (some (VTrue))
                          (some (VFalse))))]

    ['num< (check-types args 'num 'num 
                        (if (< (MetaNum-n mval1) (MetaNum-n mval2))
                          (some (VTrue))
                          (some (VFalse))))]


    ['str (let ([arg (first args)])
            (some (VStr (number->string (MetaNum-n (some-v 
                                                    (VObject-mval arg)))))))]
    ['str+ (str+ args)]
    ['list+ (list+ args)]
    ['list-len (list-len args)]
    ['tuple+ (tuple+ args)]
    ['tuple-len (tuple-len args)]
    ['str* (str* args)]
    ['strcmp (strcmp args)]
    ['strlen (strlen args)]
    ['strbool (strbool args)]
    ['strmin (strmin args)]
    ['strmax (strmax args)]
    ['strin (strin args)]
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

