#lang racket

(require data/either)

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
           [(cons 'acc amt) (set! acc (+ acc amt))]
           [(cons 'jmp amt) (set! newpc (+ pc amt))]
           [else '()])
         (set! pc newpc)
         #t)))
   (define/public (run [pcs (set)])
     (if (set-member? pcs pc)
         (failure acc)
         (let ([newpcs (set-add pcs pc)])
           (if (step)
             (run newpcs)
             (success acc)))))))

(define (parse-instr instr)
  (match (string-split instr)
    [(list opcode arg) (cons (string->symbol opcode)
                             (string->number arg))]))

(define (change-instr instr)
  (match instr
    [(cons 'jmp amt) (cons 'nop amt)]
    [(cons 'nop amt) (cons 'jmp amt)]
    [else #f]))

(define (fix-code code [change 0])
  (define instr (vector-ref code change))
  (define new-instr (change-instr instr))
  (define continuation
    (lambda () (fix-code code (add1 change))))
  (if new-instr
    (let ([new-code (vector-copy code)])
      (vector-set! new-code change new-instr)
      (define machine (new machine% [code new-code]))
      (match (send machine run)
        [(success acc) acc]
        [_ (continuation)]))
    (continuation)))

(define code 
  (call-with-input-file "input"
    (lambda (in) 
      (for/vector ([line (in-lines in)])
         (parse-instr line)))))

(define (fun1)
  (define machine (new machine% [code code]))
  (from-failure #f (send machine run)))

(define (fun2)
  (fix-code code))

(fun1)
(fun2)
