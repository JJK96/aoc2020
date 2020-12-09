#lang racket

(define (chars->num chars truechar)
  (string->number 
    (list->string
      (for/list ([char chars])
        (if (eqv? char truechar) #\1 #\0)))
    2))

(define (pass->coords pass)
  (values (chars->num (substring pass 0 7) #\B)
          (chars->num (substring pass 7) #\R)))

(define (pass->id pass)
  (let-values ([(row col) (pass->coords pass)])
    (+ col (* 8 row))))

(define (find-missing list [prev #f])
  (define i (car list))
  (if (or (not prev)
          (= prev (sub1 i)))
    (find-missing (cdr list) i)
    (sub1 i)))

(define passes (file->lines "input"))
(define ids
  (sort (for/list ([pass passes])
          (pass->id pass))
        <))

(define (fun1) (last ids))
(define (fun2) (find-missing ids))

(fun1)
(fun2)
