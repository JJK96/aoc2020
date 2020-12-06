#lang racket
(require racket/set)

(define (find-sum numbers sum)
  (define (loop numbers num-set)
    (if (null? numbers)
      '()
      (let*
        ( [num (car numbers)]
          [diff (- sum num)])
        (if (set-member? num-set diff) 
          (list diff num)
          (loop (cdr numbers) (set-add num-set num))))))
  (loop numbers (set)))

(define (find-sum3 numbers sum)
  (for*/first
    ( [a numbers]
      [b numbers]
      [c numbers]
      #:when (= sum (+ a b c)))
    (list a b c)))

(define numbers 
  (for/list ([line (file->lines "input")])
       (string->number line)))

(define (fun1) (apply * (find-sum numbers 2020)))

(define (fun2) (apply * (find-sum3 numbers 2020)))

(fun1)
(fun2)
