#lang racket

(require "declarations.rkt")

(provide OTP-encryption)
(provide OTP-decryption)

(define key "Initial Key")

(define (generate-key size) ;; generates a string key of length same as
                            ;; length of given msg.
  (define (random-number size) 
    (if (= size 0) '()
        (cons (random 1 155) (random-number (- size 1)))))
  
  (let* ([random-number-list (random-number size)]
         [key-char-list (map char random-number-list)])
    (list->string key-char-list)))

(define (OTP-general text)
  (let* ([msg-char-list (string->list text)]
         [msg-int-list (map int msg-char-list)]
         [key-int-list (map int (string->list key))]
         [XORed-list (zip XOR msg-int-list key-int-list)]
         [encrypted-char-list (map char XORed-list)])
    (list->string encrypted-char-list)))


(define (OTP-encryption msg)
  (set! key (generate-key (string-length msg)))
  (OTP-general msg))

(define (OTP-decryption cipher-text)
  (OTP-general cipher-text))
  