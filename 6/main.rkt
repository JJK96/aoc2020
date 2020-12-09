#lang racket

(define groups
  (map string-split 
    (string-split (file->string "input") "\n\n")))

(define (count/anyone group)
  ((compose length remove-duplicates string->list)
   (apply string-append group)))

(define (count/everyone group)
  (set-count 
    (apply set-intersect 
       (map string->list group))))

(apply + (map count/anyone groups))
(apply + (map count/everyone groups))
