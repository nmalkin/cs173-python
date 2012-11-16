#lang plai-typed

(require "../python-core-syntax.rkt"
         "../util.rkt"
         "num.rkt")
(require (typed-in racket/base (string=? : (string string -> boolean)))
         (typed-in racket/base (string>? : (string string -> boolean)))
         (typed-in racket/base (string<? : (string string -> boolean)))
         (typed-in racket/base (string-length : (string -> number)))
         (typed-in racket/string (string-replace : (string string string -> string)))
         (typed-in racket/base (make-string : (number char -> string)))
         (typed-in racket/base (string->list : (string -> (listof char))))
         (typed-in racket/base (char->integer : (char -> number)))
         (typed-in racket/base (integer->char : (number -> char))))

(define str-class : CExpr
  (CClass
   'str 
   'object
   (seq-ops (list ;;how do I write this one?
                  ;(def '__init__
                  ;  (CFunc (list 'self 'inputstr)
                  ;         (CAssign
                  ;           (CId 'self)
                  ;           (CObject
                  ;             'str
                  ;             (some (MetaVal (CId 'inputstr)))))))
                  (def '__add__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'str+
                                                  (list
                                                   (CId 'self)
                                                   (CId 'other))))))
                  (def '__mult__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'str*
                                         (list
                                          (CId 'self)
                                          (CId 'other))))))
                  (def '__str__
                       (CFunc (list 'self) (none)
                              (CReturn (CId 'self))))
                  (def '__eq__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'str=
                                         (list
                                          (CId 'self)
                                          (CId 'other))))))

                  (def '__cmp__
                     (CFunc (list 'self 'other) (none)
                            (CReturn (CBuiltinPrim 'strcmp
                                         (list
                                           (CId 'self)
                                           (CId 'other))))))
                  (def '__in__
                     (CFunc (list 'self 'test) (none)
                            (CReturn (CBuiltinPrim 'strin
                                         (list
                                           (CId 'self)
                                           (CId 'test))))))
                  (def '__min__
                     (CFunc (list 'self) (none)
                            (CReturn (CBuiltinPrim 'strmin
                                         (list
                                           (CId 'self))))))
                  (def '__max__
                     (CFunc (list 'self) (none)
                            (CReturn (CBuiltinPrim 'strmax
                                         (list
                                           (CId 'self))))))

                  (def '__len__
                     (CFunc (list 'self) (none)
                            (CReturn (CBuiltinPrim 'strlen
                                         (list
                                           (CId 'self))))))
                  (def '__list__
                     (CFunc (list 'self) (none)
                            (CReturn (CBuiltinPrim 'strlist
                                         (list
                                           (CId 'self))))))
                  (def '__attr__
                     (CFunc (list 'self 'idx) (none)
                            (CReturn (CBuiltinPrim 'strattr
                                         (list
                                           (CId 'self)
                                           (CId 'idx))))))))))

(define (make-builtin-str [s : string]) : CExpr
  (CObject
   'str
   (some (MetaStr s))))
(define (strlist [args : (listof CVal)] [env : Env] [sto : Store])
  : (optionof CVal)
  (check-types args env sto 'str
               (some (VObject 'list
                              (some (MetaList 
                                      (map
                                        (lambda(s)
                                         (VObject 'str 
                                                  (some (MetaStr (make-string 1 s)))
                                                  (make-hash empty)))
                                        (string->list (MetaStr-s mval1)))))
                              (make-hash empty)))))
                                      

(define (str+ (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'str 'str
               (some (VObject 'str 
                              (some (MetaStr
                                     (string-append (MetaStr-s mval1)
                                                    (MetaStr-s mval2))))
                              (hash empty)))))

(define (str (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'str
               (some (VObject 'str
                              (some (MetaStr (MetaStr-s mval1)))
                              (hash empty)))))

(define (str* (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  ;(let ([str*l 
  (check-types args env sto 'str 'num
                      (some (VObject 'str
                                     (some (MetaStr 
                                      (str*-rec (MetaStr-s mval1)
                                                (MetaNum-n mval2))))
                                     (hash empty)))))
    #|(if (none? str*l)
      (check-types args env sto 'num 'str
             (some (VObject 'str
                            (some (MetaStr 
                             (str*-rec (MetaStr-s mval2)
                                       (MetaNum-n mval1))))
                            (hash empty))))
      str*l)))|#

(define (str*-rec [str : string] [num : number]) : string
  (cond
    [(<= num 0) ""]
    [else (string-append str (str*-rec str (sub1 num)))]))

(define (strcmp [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'str 'str
     (some (VObject 'num
                    (some (MetaNum
                      (let ([str1 (MetaStr-s mval1)]
                            [str2 (MetaStr-s mval2)])
                        (cond
                          [(string<? str1 str2) -1]
                          [(string>? str1 str2) 1]
                          [(string=? str1 str2) 0]))))
                    (hash empty)))))
(define (streq [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'str 'str 
               (let ([str1 (MetaStr-s mval1)] 
                     [str2 (MetaStr-s mval2)]) 
                 (if (string=? str1 str2) 
                   (some true-val) 
                   (some false-val)))))

(define (strin [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'str 'str
     (let ([self (MetaStr-s mval1)]
           [test (MetaStr-s mval2)])
       (some (if (or (< (string-length (string-replace self test ""))
                     (string-length self))
                     (string=? test ""))
                 true-val
                 false-val)))))

(define (strlen [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'str
     (some (VObject 'num
                    (some (MetaNum
                            (string-length (MetaStr-s mval1))))
                    (hash empty)))))

(define (strbool [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'str
     (some (if (string=? (MetaStr-s mval1) "")
               false-val
               true-val))))

(define (strmin [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'str
     (some (VObject 'str
                    (some (MetaStr
                            (make-string 1
                              (integer->char
                                (foldl (lambda (c res)
                                         (min res c))
                                         ;; the maximum char integer is currently #x10FFFF
                                         ;; should find a better way to do this
                                         #x110000
                                         (map char->integer
                                          (string->list (MetaStr-s mval1))))))))
                    (hash empty)))))

(define (strmax [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'str
     (some (VObject 'str
                    (some (MetaStr
                            (make-string 1
                              (integer->char
                                (foldl (lambda (c res)
                                         (max res c))
                                       -1
                                       (map char->integer
                                          (string->list (MetaStr-s mval1))))))))
                    (hash empty)))))

(define (strattr [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  ; handle slicing here?  
  (check-types args env sto 'str 'num
     (some (VObject 'str
                    (some (MetaStr
                            (make-string 1
                                         (string-ref 
                                           (MetaStr-s mval1)
                                           (MetaNum-n mval2)))))
                    (hash empty)))))

