#lang racket

(define (numbers [input "input"])
  (call-with-input-file input
    (lambda (in)
      (for/list ([num (in-lines in)])
        (string->number num)))))

(define (is-sum? numbers num)
  (define num-set (list->set numbers))
  (for/first ([n numbers]
              #:when (set-member? num-set (- num n)))
    #t))

(define (find-number numbers [preamble 25])
  (define num (list-ref numbers preamble))
  (define is-sum (is-sum? (take numbers preamble) num))
  (if is-sum
    (find-number (cdr numbers) preamble)
    num))

(define (starts-with-contiguous-set numbers num [sum 0] [contiguous-set (list)])
  (cond
    [(= sum num) contiguous-set]
    [(> sum num) #f]
    [else (starts-with-contiguous-set 
            (cdr numbers) 
            num
            (+ sum (car numbers))
            (cons (car numbers) contiguous-set))]))

(define (find-contiguous-set numbers num)
  (define contiguous-set 
    (starts-with-contiguous-set numbers num))
  (if contiguous-set
    contiguous-set
    (find-contiguous-set (cdr numbers) num)))

(define (find-weakness contiguous-set)
  (+ (apply min contiguous-set) (apply max contiguous-set)))

(define weakness (find-number (numbers)))
(println weakness)
(find-weakness (find-contiguous-set (numbers) weakness))
