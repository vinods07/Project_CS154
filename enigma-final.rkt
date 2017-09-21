#lang racket
(provide enigma-encryptor)
(provide enigma-decryptor set-up-machine)

(define basic-l (build-list 26 values))
(define (random-wiring)
  (define (whetherIn? x l)
    (if (null? l) #f
        (if (= x (car l)) #t
            (whetherIn? x (cdr l)))))
  (define (build l ans)
    (if (null? l) ans
        (let ((i (random 26)))
          (if (whetherIn? i l) (build (remove i l) (cons i ans))
              (build l ans)))))
  (build basic-l '()))

(define (plugboard-values-generator)
  (define (whetherIn? x l)
    (if (null? l) #f
        (if (= x (car l)) #t
            (whetherIn? x (cdr l)))))
  (define (build l ans)
    (if (= (length ans) 12) ans
        (let ((i (random 26)))
          (if (whetherIn? i l) (build (remove i l) (cons i ans))
              (build l ans)))))
  (define (convert l)
    (if (null? l) '()
        (cons (cons (car l) (cadr l)) (convert (cddr l)))))
  (convert (build basic-l '())))

(define (rotor-shift l)
  (let* ((len (length l))
         (last-item (list-ref l (- len 1))))
    (cons last-item (reverse (cdr (reverse l))))))

(define (rotor-list l1 l2)
  (define f (lambda (x y) (if (>= y x) (- y x)
                              (+ 26 y (- x)))))
  (if (null? l1) '()
      (cons (f (car l1) (car l2)) (rotor-list (cdr l1) (cdr l2)))))

(define (output-list l1 l2)
  (if (null? l1) '()
      (cons (remainder (+ (car l1) (car l2)) 26) (output-list (cdr l1) (cdr l2)))))

(define plugboard-values (plugboard-values-generator))

(define first-rotor-enc
  (rotor-list basic-l (random-wiring)))
(define first-rotor-dec first-rotor-enc)
(define rotation-i-1-enc 0)
(define rotation-i-1-dec 0)
(define second-rotor-enc
  (rotor-list basic-l (random-wiring)))
(define second-rotor-dec second-rotor-enc)
(define rotation-i-2-enc 0)
(define rotation-i-2-dec 0)
(define third-rotor-enc
  (rotor-list basic-l (random-wiring)))
(define third-rotor-dec third-rotor-enc)
(define rotation-i-3-enc 0)
(define rotation-i-3-dec 0)

(define (set-up-machine)
  (begin
    (set! plugboard-values (plugboard-values-generator))
    (set! first-rotor-enc
           (rotor-list basic-l (random-wiring)))
    (set! first-rotor-dec first-rotor-enc)
    (set! second-rotor-enc
           (rotor-list basic-l (random-wiring)))
    (set! second-rotor-dec second-rotor-enc)
    (set! third-rotor-enc
           (rotor-list basic-l (random-wiring)))
    (set! third-rotor-dec third-rotor-enc)
    (set! rotation-i-1-enc 0)
    (set! rotation-i-2-enc 0)
    (set! rotation-i-3-enc 0)
    (set! rotation-i-1-dec 0)
    (set! rotation-i-2-dec 0)
    (set! rotation-i-3-dec 0)))

(define (pass-through-plugboard no plugboard)
  (if (null? plugboard) no
      (if (= no (caar plugboard)) (cdar plugboard)
          (if (= no (cdar plugboard)) (caar plugboard)
              (pass-through-plugboard no (cdr plugboard))))))

(define (pass-through-rotor no rotor)
  (remainder (+ no (list-ref rotor no)) 26))

(define (reverse-pass-through-rotor no rotor)
  (define (helper l1 l2)
    (if (null? l1) (error "Check again")
        (if (= no (remainder (+ (car l1) (car l2)) 26)) (car l1)
            (helper (cdr l1) (cdr l2)))))
  (helper basic-l rotor))

(define (reflect no)
  (- 25 no))

(define (enigma-encryptor string-msg)
  (let* [(msglist (string->list (string-downcase string-msg)))
         (input (map (lambda (c) (if (and (< (char->integer c) 123) (> (char->integer c) 96)) (- (char->integer c) 97)
                                     c)) msglist))]
    (define (helper l ans)
      (if (null? l) (reverse ans)
          (if (not (number? (car l))) (helper (cdr l) (cons (car l) ans))
              (if (or (> (car l) 25) (< (car l) 0)) (error "Check numbering")
                  (begin
                    (set! ans (cons
                               (pass-through-plugboard
                                (reverse-pass-through-rotor
                                 (reverse-pass-through-rotor                                 
                                  (reverse-pass-through-rotor
                                   (reflect                                   
                                    (pass-through-rotor
                                     (pass-through-rotor
                                      (pass-through-rotor
                                       (pass-through-plugboard (car l) plugboard-values)
                                       first-rotor-enc)
                                      second-rotor-enc)
                                     third-rotor-enc)
                                    )
                                   third-rotor-enc)
                                  second-rotor-enc)
                                 first-rotor-enc)
                                plugboard-values)
                               ans))
                    (set! first-rotor-enc (rotor-shift first-rotor-enc))
                    (if (= rotation-i-1-enc 25)
                        (begin
                          (set! rotation-i-1-enc 0)
                          (set! second-rotor-enc (rotor-shift second-rotor-enc))
                          (if (= rotation-i-2-enc 25)
                              (begin
                                (set! rotation-i-2-enc 0)
                                (set! third-rotor-enc (rotor-shift third-rotor-enc))
                                (if (= rotation-i-3-enc 25)
                                    (set! rotation-i-3-enc 0)
                                    (set! rotation-i-3-enc (+ rotation-i-3-enc 1))))
                              (set! rotation-i-2-enc (+ rotation-i-2-enc 1))))
                        (set! rotation-i-1-enc (+ rotation-i-1-enc 1)))
                    (helper (cdr l) ans))))))
    (list->string (map (lambda (c) (if (number? c) (integer->char (+ c 97))
                                       c)) (helper input '())))))

(define (enigma-decryptor string-msg)
  (let* [(msglist (string->list (string-downcase string-msg)))
         (input (map (lambda (c) (if (and (< (char->integer c) 123) (> (char->integer c) 96)) (- (char->integer c) 97)
                                     c)) msglist))]
    (define (helper l ans)
      (if (null? l) (reverse ans)
          (if (not (number? (car l))) (helper (cdr l) (cons (car l) ans))
              (if (or (> (car l) 25) (< (car l) 0)) (error "Check numbering")
                  (begin
                    (set! ans (cons
                               (pass-through-plugboard
                                (reverse-pass-through-rotor
                                 (reverse-pass-through-rotor
                                  (reverse-pass-through-rotor
                                   (reflect
                                    (pass-through-rotor
                                     (pass-through-rotor
                                      (pass-through-rotor
                                       (pass-through-plugboard (car l) plugboard-values)
                                       first-rotor-dec)
                                      second-rotor-dec)
                                     third-rotor-dec)
                                    )
                                   third-rotor-dec)
                                  second-rotor-dec)
                                 first-rotor-dec)
                                plugboard-values)
                               ans))
                    (set! first-rotor-dec (rotor-shift first-rotor-dec))
                    (if (= rotation-i-1-dec 25)
                        (begin
                          (set! rotation-i-1-dec 0)
                          (set! second-rotor-dec (rotor-shift second-rotor-dec))
                          (if (= rotation-i-2-dec 25)
                              (begin
                                (set! rotation-i-2-dec 0)
                                (set! third-rotor-dec (rotor-shift third-rotor-dec))
                                (if (= rotation-i-3-dec 25)
                                    (set! rotation-i-3-dec 0)
                                    (set! rotation-i-3-dec (+ rotation-i-3-dec 1))))
                              (set! rotation-i-2-dec (+ rotation-i-2-dec 1))))
                        (set! rotation-i-1-dec (+ rotation-i-1-dec 1)))
                    (helper (cdr l) ans))))))
    (string-upcase (list->string (map (lambda (c) (if (number? c) (integer->char (+ c 97))
                                       c)) (helper input '()))))))