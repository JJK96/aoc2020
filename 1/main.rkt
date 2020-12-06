#lang racket
(require racket/set)

(define (find-sum numbers sum)
  (let ([num-set (mutable-set)])
    (define (loop numbers)
      (if (null? numbers)
        '()
        (let*
          ( [num (car numbers)]
            [diff (- sum num)])
          (cond 
            [(set-member? num-set diff) (list diff num)]
            [else 
               (set-add! num-set num)
               (loop (cdr numbers))]))))
    (loop numbers)))

(apply * (find-sum
            (for/list ([line (file->lines "input")])
               (string->number line))
            2020))
