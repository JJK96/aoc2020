#lang racket

(define machine% 
  (class object%
   (init-field code)
   (init-field (pc 0))
   (init-field (acc 0))
   (super-new)
   (define/public (display)
     (format "pc: ~a, acc: ~a, code:~a\n" pc acc code))
   (define/public (step)
     (if (>= pc (vector-length code))
       #f
       (let ([instr (vector-ref code pc)]
             [newpc (add1 pc)])
         (match instr
           [(cons "acc" amt) (set! acc (+ acc amt))]
           [(cons "jmp" amt) (set! newpc (+ pc amt))]
           [else '()])
         (set! pc newpc)
         (display)
         #t)))
   (define/public (run-until-repeat [pcs '()])
     (if (member pc pcs)
         acc
         (let ([newpcs (cons pc pcs)])
           (step)
           (run-until-repeat newpcs))))
   (define/public (run-until-end [pcs '()])
     (if (member pc pcs)
         #f ;Repeating, so return false
         (let ([newpcs (cons pc pcs)])
           (if (step)
             (run-until-end newpcs)
             acc))))))

(define (parse-instr instr)
  (define split (string-split instr))
  (cons (list-ref split 0)
        (string->number (list-ref split 1))))

(define (change-instr instr)
  (match instr
    [(cons "jmp" amt) (cons "nop" amt)]
    [(cons "nop" amt) (cons "jmp" amt)]
    [else #f]))

(define (fix-code code [change 0])
  (define instr (vector-ref code change))
  (define new-instr (change-instr instr))
  (if new-instr
    (let ([new-code (vector-copy code)])
      (vector-set! new-code change new-instr)
      (define machine (new machine% [code new-code]))
      (define result (send machine run-until-end))
      (if result
        result
        (fix-code code (add1 change))))
    (fix-code code (add1 change))))

(define code (list->vector (map parse-instr (file->lines "input"))))

(define (fun1)
  (define machine (new machine% [code code]))
  (send machine run-until-repeat))

(define (fun2)
  (fix-code code))

(fun1)
(fun2)
