#lang racket

(require "declarations.rkt")

(provide viginere-encryption)
(provide viginere-decryption)

(define key "CRYPTOGRAPHY")

(define (viginere-encryption msg vkey)
  (begin (set! key vkey)
         (viginere-cipher (string-upcase msg) +)))

(define (viginere-decryption cipher-text)
  (viginere-cipher cipher-text -))

(define (key-enter new-key)
  (set! key new-key))

(define (viginere-cipher text f)
  (define key-list (map int (string->list key)))
  (define k key-list)
  (define (cipher c)
    (define (add-ascii a b)
      (+ 65 (modulo (f (- a 65) (- b 65)) 26)))
    (if (alphabet? c)
        (if (null? k)
            (begin (set! k key-list) (cipher c))
            (let* ([x (car k)]) (begin (set! k (cdr k)) (add-ascii c x) )))
        c))
  (list->string (map char (map cipher (map int (string->list text))))))