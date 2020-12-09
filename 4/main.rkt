#lang racket

(require racket/file)

(define (get-passports [filename "input"])
  (define passports (string-split (file->string filename) "\n\n"))
  (for/list ([passport passports])
    (apply hash (string-split passport #px":|\\s"))))
(define passports (get-passports)) 

(define (num-between num low high)
  (define value (string->number num))
  (and (>= value low)
       (<= value high)))

(define (validate-hgt hgt)
  (define len (string-length hgt))
  (define unit (substring hgt (- len 2) len))
  (define number (substring hgt 0 (- len 2)))
  (match unit
    ["cm" (num-between number 150 193)]
    ["in" (num-between number 59 76)]
    [else #f]))

(define (validate-hcl hcl)
  (regexp-match? #px"^#[0-9a-f]{6}$" hcl))

(define (validate-ecl ecl)
  (member ecl '("amb" "blu" "brn" "gry" "grn" "hzl" "oth")))

(define (validate-pid pid)
  (regexp-match? #px"^\\d{9}$" pid))

(define (validate key value)
  (match key
   ["byr" (num-between value 1920 2002)]
   ["iyr" (num-between value 2010 2020)]
   ["eyr" (num-between value 2020 2030)]
   ["hgt" (validate-hgt value)]
   ["hcl" (validate-hcl value)]
   ["ecl" (validate-ecl value)]
   ["pid" (validate-pid value)]))

(define (verify passport [validate? #f])
  (define required-fields '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))
  (for/and ([f required-fields])
    (and (hash-has-key? passport f)
      (or (not validate?)
          (validate f (hash-ref passport f))))))

(define (fun1) 
  (for/sum ([passport passports])
     (if (verify passport) 1 0)))

(define (fun2)
  (for/sum ([passport passports])
     (if (verify passport #t) 1 0)))

(fun1)
(fun2)
