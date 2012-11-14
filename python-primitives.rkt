#lang plai-typed

(require "python-core-syntax.rkt"
         "util.rkt"
         "builtins/str.rkt"
         "builtins/list.rkt"
         "builtins/tuple.rkt"
         "builtins/object.rkt"
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
    ['num- (check-types args 'num 'num 
                        (some (VObject 'num (some (MetaNum 
                                                    (- (MetaNum-n mval1) 
                                                       (MetaNum-n mval2))))
                                        (make-hash empty))))]
    ['num* (check-types args 'num 'num 
                        (some (VObject 'num (some (MetaNum 
                                                    (* (MetaNum-n mval1) 
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

    ['num>= (check-types args 'num 'num 
                        (if (>= (MetaNum-n mval1) (MetaNum-n mval2))
                          (some (VTrue))
                          (some (VFalse))))]

    ['num<= (check-types args 'num 'num 
                        (if (<= (MetaNum-n mval1) (MetaNum-n mval2))
                          (some (VTrue))
                          (some (VFalse))))]


    ['num-str (let ([arg (first args)])
            (some (VObject 'str 
                           (some (MetaStr 
                             (number->string (MetaNum-n (some-v (VObject-mval
                                                                  arg))))))
                           (make-hash empty))))]
    ;string
    ['str+ (str+ args)]
    ['str= (streq args)]
    ['str* (str* args)]
    ['strcmp (strcmp args)]
    ['strlen (strlen args)]
    ['strbool (strbool args)]
    ['strmin (strmin args)]
    ['strmax (strmax args)]
    ['strin (strin args)]

    ;list
    ['list+ (list+ args)]
    ['list-len (list-len args)]
    ['list-in (list-in args)]
    ['list-attr (list-attr args)]
    ['list-str (list-str args)]

    ;tuple
    ['tuple+ (tuple+ args)]
    ['tuple-len (tuple-len args)]

    ;object 
    ['obj-str (obj-str args)]))


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

