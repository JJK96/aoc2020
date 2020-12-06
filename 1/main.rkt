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

(apply * (find-sum
            (for/list ([line (file->lines "input")])
               (string->number line))
            2020))
