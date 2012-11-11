#lang plai-typed

(require "../python-core-syntax.rkt"
         "../util.rkt")
(require (typed-in racket/base (string=? : (string string -> boolean)))
         (typed-in racket/base (string>? : (string string -> boolean)))
         (typed-in racket/base (string<? : (string string -> boolean)))
         (typed-in racket/base (string-length : (string -> number)))
         (typed-in racket/string (string-replace : (string string string -> string)))
         (typed-in racket/base (make-string : (number string -> string)))
         (typed-in racket/base (string->list : (string -> (listof string))))
         (typed-in racket/base (char->integer : (string -> number)))
         (typed-in racket/base (integer->char : (number -> string))))

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
                    (CFunc (list 'self 'other)
                           (CReturn (CBuiltinPrim 'str+
                                                  (list
                                                   (CId 'self)
                                                   (CId 'other))))))
                  (def '__mult__
                    (CFunc (list 'self 'other)
                           (CReturn (CBuiltinPrim 'str*
                                         (list
                                          (CId 'self)
                                          (CId 'other))))))
                  (def '__cmp__
                     (CFunc (list 'self 'other)
                            (CReturn (CBuiltinPrim 'strcmp
                                         (list
                                           (CId 'self)
                                           (CId 'other))))))
                  (def '__in__
                     (CFunc (list 'self 'test)
                            (CReturn (CBuiltinPrim 'strin
                                         (list
                                           (CId 'self)
                                           (CId 'test))))))
                  (def '__min__
                     (CFunc (list 'self)
                            (CReturn (CBuiltinPrim 'strmin
                                         (list
                                           (CId 'self))))))
                  (def '__max__
                     (CFunc (list 'self)
                            (CReturn (CBuiltinPrim 'strmax
                                         (list
                                           (CId 'self))))))
                  (def '__len__
                     (CFunc (list 'self)
                            (CReturn (CBuiltinPrim 'strlen
                                         (list
                                           (CId 'self))))))
                  (def '__bool__
                     (CFunc (list 'self)
                            (CReturn (CBuiltinPrim 'strbool
                                         (list
                                           (CId 'self))))))))))

(define (make-builtin-str [s : string]) : CExpr
  (CObject
   'str
   (some (MetaStr s))))

(define (str+ (args : (listof CVal))) : (optionof CVal)
  (check-types args 'str 'str
               (some (VObject 'str 
                              (some (MetaStr
                                     (string-append (MetaStr-s mval1)
                                                    (MetaStr-s mval2))))
                              (hash empty)))))

(define (str (args : (listof CVal))) : (optionof CVal)
  (check-types args 'str
               (some (VObject 'str
                              (some (MetaStr (MetaStr-s mval1)))
                              (hash empty)))))

(define (str* (args : (listof CVal))) : (optionof CVal)
  ;(let ([str*l 
  (check-types args 'str 'num
                      (some (VObject 'str
                                     (some (MetaStr 
                                      (str*-rec (MetaStr-s mval1)
                                                (MetaNum-n mval2))))
                                     (hash empty)))))
    #|(if (none? str*l)
      (check-types args 'num 'str
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

(define (strcmp [args : (listof CVal)]) : (optionof CVal)
  (check-types args 'str 'str
     (some (VObject 'num
                    (some (MetaNum
                      (let ([str1 (MetaStr-s mval1)]
                            [str2 (MetaStr-s mval2)])
                        (cond
                          [(string<? str1 str2) -1]
                          [(string>? str1 str2) 1]
                          [(string=? str1 str2) 0]))))
                    (hash empty)))))

(define (strin [args : (listof CVal)]) : (optionof CVal)
  (check-types args 'str 'str
     (let ([self (MetaStr-s mval1)]
           [test (MetaStr-s mval2)])
       (some (if (or (< (string-length (string-replace self test ""))
                     (string-length self))
                     (string=? test ""))
                 (VTrue)
                 (VFalse))))))

(define (strlen [args : (listof CVal)]) : (optionof CVal)
  (check-types args 'str
     (some (VObject 'num
                    (some (MetaNum
                            (string-length (MetaStr-s mval1))))
                    (hash empty)))))

(define (strbool [args : (listof CVal)]) : (optionof CVal)
  (check-types args 'str
     (some (if (string=? (MetaStr-s mval1) "")
               (VFalse)
               (VTrue)))))

(define (strmin [args : (listof CVal)]) : (optionof CVal)
  (check-types args 'str
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

(define (strmax [args : (listof CVal)]) : (optionof CVal)
  (check-types args 'str
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
