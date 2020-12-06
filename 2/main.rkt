#lang racket

(define (parse-line line)
    (match (regexp-match #px"(\\d+)-(\\d+) (\\w): (.+)" line)
      [(list _ low high char pass) (list (string->number low) (string->number high) (string-ref char 0) pass)]))

(define (correct-password line)
  (define (loop low high char pass)
    (cond 
       [(< high 0) #f]
       [(null? pass) (<= low 0)]
       [(eqv? char (car pass))
        (loop (- low 1) (- high 1) char (cdr pass))]
       [else (loop low high char (cdr pass))]))
  (match (parse-line line)
    [(list low high char pass) (loop low high char (string->list pass))]))

(define (matching-lines lines)
  (for/list 
     ([line lines]
      #:when (correct-password line))
    line))

(define (correct-password2 line)
  (match (parse-line line)
    [(list pos1 pos2 char pass)
     (let* ([cond1 (eqv? char (string-ref pass (- pos1 1)))]
            [cond2 (eqv? char (string-ref pass (- pos2 1)))])
        (and (not (and cond1 cond2))
             (or cond1 cond2)))]))

(define (matching-lines2 lines)
  (for/list 
     ([line lines]
      #:when (correct-password2 line))
    line))

(define (fun1) 
  (printf "1. ~a\n" (length (matching-lines (file->lines "input")))))
(define (fun2) 
  (printf "2. ~a\n" (length (matching-lines2 (file->lines "input")))))

(fun1)
(fun2)
