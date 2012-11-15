#lang plai-typed

(require "python-core-syntax.rkt"
         "util.rkt"
         "builtins/str.rkt"
         "builtins/list.rkt"
         "builtins/tuple.rkt"
         "builtins/dict.rkt"
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
  (display (string-append (pretty arg) "\n")))

(define (python-prim1 op arg)
  (case op
    [(print) (begin (print arg) arg)]))

(define (builtin-prim [op : symbol] [args : (listof CVal)] 
                      [env : Env] [sto : Store]) : (optionof CVal)
  (case op
    ['num+ (check-types args env sto 'num 'num 
                        (some (VObject 'num (some (MetaNum 
                                                    (+ (MetaNum-n mval1) 
                                                       (MetaNum-n mval2))))
                                        (make-hash empty))))]
    ['num- (check-types args env sto 'num 'num 
                        (some (VObject 'num (some (MetaNum 
                                                    (- (MetaNum-n mval1) 
                                                       (MetaNum-n mval2))))
                                        (make-hash empty))))]
    ['num* (check-types args env sto 'num 'num 
                        (some (VObject 'num (some (MetaNum 
                                                    (* (MetaNum-n mval1) 
                                                       (MetaNum-n mval2))))
                                        (make-hash empty))))]
    ['num/ (check-types args env sto 'num 'num 
                        (some (VObject 'num (some (MetaNum 
                                                    (* (MetaNum-n mval1) 
                                                       (MetaNum-n mval2))))
                                        (make-hash empty))))]
    ['num= (check-types args env sto 'num 'num 
                        (if (= (MetaNum-n mval1) (MetaNum-n mval2))
                          (some true-val)
                          (some false-val)))]
    ['num> (check-types args env sto 'num 'num 
                        (if (> (MetaNum-n mval1) (MetaNum-n mval2))
                          (some true-val)
                          (some false-val)))]

    ['num< (check-types args env sto 'num 'num 
                        (if (< (MetaNum-n mval1) (MetaNum-n mval2))
                          (some true-val)
                          (some false-val)))]

    ['num>= (check-types args env sto 'num 'num 
                        (if (>= (MetaNum-n mval1) (MetaNum-n mval2))
                          (some true-val)
                          (some false-val)))]

    ['num<= (check-types args env sto 'num 'num 
                        (if (<= (MetaNum-n mval1) (MetaNum-n mval2))
                         (some true-val)
                          (some false-val)))]

    ['num-str (let ([arg (first args)])
            (some (VObject 'str 
                           (some (MetaStr 
                             (number->string (MetaNum-n (some-v (VObject-mval
                                                                  arg))))))
                           (make-hash empty))))]
    ;string
    ['str+ (str+ args env sto)]
    ['str= (streq args env sto)]
    ['str* (str* args env sto)]
    ['strcmp (strcmp args env sto)]
    ['strlen (strlen args env sto)]
    ['strbool (strbool args env sto)]
    ['strmin (strmin args env sto)]
    ['strmax (strmax args env sto)]
    ['strin (strin args env sto)]

    ;list
    ['list+ (list+ args env sto)]
    ['list-len (list-len args env sto)]
    ['list-in (list-in args env sto)]
    ['list-attr (list-attr args env sto)]
    ['list-str (list-str args env sto)]

    ;tuple
    ['tuple+ (tuple+ args env sto)]
    ['tuple* (tuple* args env sto)]
    ['tuple-len (tuple-len args env sto)]
    ['tuple-in (tuple-in args env sto)]
    ['tuple-attr (tuple-attr args env sto)]
    ['tuple-str (tuple-str args env sto)]

    ;dict
    ['dict-len (dict-len args env sto)]
    ['dict-str (dict-str args env sto)]
    ['dict-clear (dict-clear args env sto)]
    ['dict-in (dict-in args env sto)]

    ;object 
    ['obj-str (obj-str args)]))
