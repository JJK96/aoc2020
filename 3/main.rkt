#lang racket

(define map%
  (class object%
    (super-new)
    (init mapstring)
    (define map (string-split mapstring))
    (define/public (print-map)
      (for ([line map])
         (println line))) 
    (define/private (move pos slope)
      (match-let 
        ([(cons right down) slope]
         [(cons x y) pos])
        (cons (+ x right)
              (+ y down))))
    (define/public (get pos)
      (define row (list-ref map (cdr pos)))
      (string-ref row 
                  (modulo (car pos) (string-length row))))
    (define/public (tree? pos)
      (eqv? #\# (get pos)))
    (define/public (on-map? pos)
      (and (< (cdr pos) (length map))))
    (define/public (count-trees slope)
        (define (_count-trees pos count)
            (cond
                [(on-map? pos) 
                 (_count-trees 
                    (move pos slope)
                    (+ count (if (tree? pos) 1 0)))]
                [else count]))
        (_count-trees (cons 0 0) 0))))

(define map (new map% [mapstring (file->string "input")]))
(define (fun1) (printf "1. ~a\n" (send map count-trees (cons 3 1))))

(define (fun2)
  (define slopes (list (cons 1 1) (cons 3 1) (cons 5 1) (cons 7 1) (cons 1 2)))
  (printf "2. ~a\n"
    (apply * 
      (for/list ([slope slopes])
        (send map count-trees slope)))))

(fun1)
(fun2)
