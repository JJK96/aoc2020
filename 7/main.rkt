#lang racket

(require
   megaparsack
   megaparsack/text
   data/monad
   data/applicative)

(define word/p 
  (do (word <- (many+/p letter/p))
      (pure (list->string word))))

(define color/p
  (do (word1 <- word/p)
      space/p
      (word2 <- word/p)
      (pure (string-join (list word1 word2)))))

(define bag/p
  (do (color <- color/p)
      space/p
      (many+/p letter/p)
      (pure color)))

(define contained/p
  (do (amount <- integer/p)
      space/p
      (bag <- bag/p)
      (or/p (char-in/p ",."))
      (or/p space/p eof/p)
      (pure (cons amount bag))))

(define contained-list/p
  (do (contained <- (many/p contained/p))
      (pure contained)))

(define rule/p
  (do (bag <- bag/p)
      (string/p " contain ")
      (contained <- contained-list/p)
      (pure (cons bag contained))))

(define (contains? color rule)
  (member color (cdr rule)
     (lambda (bag contains-bag)
       (equal? color (cdr contains-bag)))))

(define (can-contain rules color)
  (define containers (map car (filter (curry contains? color) rules)))
  (apply (curry set-union containers) 
         (map (curry can-contain rules) containers)))

(define (should-contain rules color)
  (define bags 
    (for/first ([rule rules]
                #:when (equal? (car rule) color))
     (cdr rule)))
  (for/sum ([bag bags])
    (* (car bag) (add1 (should-contain rules (cdr bag))))))

(define rules 
  (map (compose parse-result! (curry parse-string rule/p)) 
       (file->lines "input")))
(length (can-contain rules "shiny gold"))
(should-contain rules "shiny gold")
