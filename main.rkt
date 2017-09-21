#lang racket/gui
(require graphics/graphics)
(require "declarations.rkt")
(require "Caesar-Shift.rkt")
(require "Scytale.rkt")
(require "OTP.rkt")
(require "Viginer.rkt")
(require "AES.rkt")
(require "enigma-final.rkt")
(require "Hacker.rkt")
(open-graphics)
(define v1 (open-viewport "Cryptographic Security GUI" 900 680) )
((draw-viewport v1) "light blue")
;((draw-viewport v1) 20)
(define cs-rect
  ((draw-solid-rectangle v1)
   (make-posn 0 0) 220 220 "red"))
((draw-string v1) (make-posn 45 120) "CESEAR-SHIFT" "black")

(define scytale-rect
  ((draw-solid-rectangle v1)
   (make-posn 230 0) 220 220 "red"))
((draw-string v1) (make-posn 305 120) "SCYTALE" "black")

(define OTP-rect
  ((draw-solid-rectangle v1)
   (make-posn 460 0) 220 220 "red"))
((draw-string v1) (make-posn 500 120) "ONE TIME PAD" "black")

(define viginere-rect
  ((draw-solid-rectangle v1)
   (make-posn 690 0) 220 220 "red"))
((draw-string v1) (make-posn 730 120) "VIGINERE-CIPHER" "black")

(define AES-rect
  ((draw-solid-rectangle v1)
   (make-posn 690 230) 220 220 "red"))
((draw-string v1) (make-posn 760 340) "A.E.S. " "black")

(define enigma-rect
  ((draw-solid-rectangle v1)
   (make-posn 0 230) 220 220 "red"))
((draw-string v1) (make-posn 60 340) "ENIGMA" "black")

(define centrals-rect
  ((draw-solid-rectangle v1)
   (make-posn 240 240) 425 200 "green"))
((draw-string v1) (make-posn 370 340) "CENTRAL SERVER" "blue")

(define hacker-rect
  ((draw-solid-rectangle v1)
   (make-posn 0 460) 900 220 "blue"))
((draw-string v1) (make-posn 410 600) "HACKER" "black")

(define main-str "")
(define main-moe "")
(define frames 1)

(define caesar-shift-gui
  (lambda ()
    (begin
      (let ((frame (new frame% [label "CAESAR SHIFT GUI"]
                        [width 400]
                        [height 400])))
        (define caesar-text (new text-field% [parent frame] [label "Enter The Text Here"]))
        (define w1 (new horizontal-panel% [parent frame]
                        [alignment '(center top)]))
        (define key-text (new text-field% [parent frame] [label "Enter The Key Here: Number From 1 to 25"]
                              [min-width 100]
                              [min-height 20]))
        (new message% [parent frame]
             [label "OR"])
        (define w2 (new horizontal-panel% [parent frame]
                        [alignment '(center top)]))
        (define w3 (new horizontal-panel% [parent frame]
                        [alignment '(center top)]))
        
        (define b1 (new button%
                        [parent w2]
                        [label "Generate Random Key"]
                        [callback (lambda (button event)
                                    (begin
                                      (send key-text set-value "random")
                                      ))]))
        (define ms "")
       
        (define b2 (new button%
                        [parent w3]
                        [label "Encrypt"]
                        [callback (lambda (button event)
                                    (let* [(str1 (send caesar-text get-value))
                                           (cskey (send key-text get-value))]
                                      (if (or (null? (string->list str1))
                                              (null? (string->list cskey)))
                                          (let ((f1 (new frame%
                                                         [label "Error"]
                                                         [width 200]
                                                         [height 100])))
                                            (begin
                                              (define m1 (new message%
                                                              [parent f1]
                                                              [label "You Cannot Leave Input Text Empty"]))
                                              (send f1 show #t)))
                                          (if (not (string->number cskey)) (let ((str2 (caesar-shift-encryptor str1 "random")))
                                                                             (begin
                                                                               (new message% [parent frame] [label (line-wrap str2)])
                                                                               (set! ms str2)))
                                              (if (and (< (string->number cskey) 26) (> (string->number cskey) 0))
                                                  (let ((str2 (caesar-shift-encryptor str1 (string->number cskey))))
                                                    (begin
                                                      (new message% [parent frame] [label (line-wrap str2)])
                                                      (set! ms str2)))
                                                  (let ((f1 (new frame%
                                                                 [label "Error"]
                                                                 [width 200]
                                                                 [height 100])))
                                                    (begin
                                                      (define m1 (new message%
                                                                      [parent f1]
                                                                      [label "Enter Number In Range"]))
                                                      (send f1 show #t))))))))]))

        (define b3 (new button%
                        [parent frame]
                        [label "Send To Server"]
                        [callback (lambda (button event)
                                    (begin
                                      (set! main-str ms)
                                      (set! main-moe "Caesar-Shift")
                                      (set! frames 3)
                                      (send frame show #f)
                                      (CC-gui ms  "Caesar-Shift")
                                      (HACKER-gui ms "Caesar-Shift")))]))

        (define backButton (new button%
                                [parent frame]
                                [label "Back"]
                                [callback (lambda (button event)
                                            (begin
                                              (set! frames (- frames 1)) 
                                              (send frame show #f)
                                              (call)
                                              ))]))
         (define the-bitmap
        (make-object bitmap% "red-tree.jpg"))
      
      (define canvas
        (instantiate canvas%
          (frame)
          (paint-callback
           (lambda (canvas dc)
             (send dc draw-bitmap the-bitmap 0 0)))
          (min-width (send the-bitmap get-width))
          (min-height (send the-bitmap get-height))))
        (when (system-position-ok-before-cancel?)
          (send w1 change-children reverse))
        
        (when (system-position-ok-before-cancel?)
          (send w1 change-children reverse))
        (send frame show #t)))))

(define scytale-gui
  (lambda ()
    (begin
      (let ((frame (new frame% [label "Scytale GUI"]
                        [width 400]
                        [height 400])))
        (define caesar-text (new text-field% [parent frame] [label "Enter The Text Here"]))
        (define w1 (new horizontal-panel% [parent frame]
                        [alignment '(center top)]))
        
        (define w2 (new horizontal-panel% [parent frame]
                        [alignment '(center top)]))
        (define w3 (new horizontal-panel% [parent frame]
                        [alignment '(center top)]))
        (define w4 (new horizontal-panel% [parent frame]
                        [alignment '(center top)]))
        
        (define ms "")
        (define b2 (new button%
                        [parent w3]
                        [label "Encrypt"]
                        [callback (lambda (button event)
                                    (let [(str1 (send caesar-text get-value))]
                                      (if (null? (string->list str1))
                                          (let ((f1 (new frame%
                                                         [label "Error"]
                                                         [width 200]
                                                         [height 100])))
                                            (begin
                                              (define m1 (new message%
                                                              [parent f1]
                                                              [label "You Cannot Leave Input Text Empty"]))
                                              (send f1 show #t)))
                                          (let ((str2 (scytale-encryption str1)))
                                            (begin (new message% [parent frame] [label (line-wrap str2)])
                                                   (set! ms str2))))))]))
        (define b3 (new button%
                        [parent frame]
                        [label "Send To Server"]
                        [callback (lambda (button event)
                                    (begin
                                      (set! main-str ms)
                                      (set! main-moe "Scytale")
                                      (set! frames 3) 
                                      (send frame show #f)
                                      (CC-gui ms  "Scytale")
                                      (HACKER-gui ms "Scytale")))]))
        (define backButton (new button%
                                [parent frame]
                                [label "Back"]
                                [callback (lambda (button event)
                                            (begin
                                              (set! frames (- frames 1)) 
                                              (send frame show #f)
                                              (call)
                                              ))]))
         (define the-bitmap
        (make-object bitmap% "Scytale-Method.jpg"))
      
      (define canvas
        (instantiate canvas%
          (frame)
          (paint-callback
           (lambda (canvas dc)
             (send dc draw-bitmap the-bitmap 0 0)))
          (min-width (send the-bitmap get-width))
          (min-height (send the-bitmap get-height))))
        (when (system-position-ok-before-cancel?)
          (send w1 change-children reverse))
        
        (when (system-position-ok-before-cancel?)
          (send w1 change-children reverse))
        (send frame show #t)))))

(define OTP-gui
  (lambda ()
    (begin
      (let ((frame (new frame% [label "OTP GUI"]
                        [width 400]
                        [height 400])))
        (define OTP-text (new text-field% [parent frame] [label "Enter The Text Here"]))
        (define w1 (new horizontal-panel% [parent frame]
                        [alignment '(center top)]))
        (define w2 (new horizontal-panel% [parent frame]
                        [alignment '(center top)]))

        (define ms "")
        
        (define b2 (new button%
                        [parent w1]
                        [label "Encrypt"]
                        [callback (lambda (button event)
                                    (let [(str1 (send OTP-text get-value))]
                                      (if (null? (string->list str1))
                                          (let ((f1 (new frame%
                                                         [label "Error"]
                                                         [width 200]
                                                         [height 100])))
                                            (begin
                                              (define m1 (new message%
                                                              [parent f1]
                                                              [label "You Cannot Leave Input Text Empty"]))
                                              (send f1 show #t)))
                                          (let ((str2 (OTP-encryption str1)))
                                            (begin (set! ms str2)
                                                   (new message% [parent frame] [label (line-wrap str2)]))))))]))

        (define b3 (new button%
                        [parent frame]
                        [label "Send To Server"]
                        [callback (lambda (button event)
                                    (begin
                                      (set! main-str ms)
                                      (set! main-moe "OTP")
                                      (set! frames 3)
                                      (send frame show #f)
                                      (CC-gui ms  "OTP")
                                      (HACKER-gui ms "OTP")))]))
        (define backButton (new button%
                                [parent frame]
                                [label "Back"]
                                [callback (lambda (button event)
                                            (begin
                                              (set! frames (- frames 1))
                                              (send frame show #f)
                                              (call)
                                              ))]))
         (define the-bitmap
        (make-object bitmap% "blue.jpg"))
      
      (define canvas
        (instantiate canvas%
          (frame)
          (paint-callback
           (lambda (canvas dc)
             (send dc draw-bitmap the-bitmap 0 0)))
          (min-width (send the-bitmap get-width))
          (min-height (send the-bitmap get-height))))
        (when (system-position-ok-before-cancel?)
          (send w1 change-children reverse))
        
        
        (when (system-position-ok-before-cancel?)
          (send w1 change-children reverse))
        (send frame show #t)))))

(define viginere-gui
  (lambda ()
    (begin
      (let ((frame (new frame% [label "Viginere-Cipher GUI"]
                        [width 400]
                        [height 400])))
        (define vigenere-text (new text-field% [parent frame] [label "Enter The Text Here"]))
        (define w1 (new horizontal-panel% [parent frame]
                        [alignment '(center top)]))
        (define key-text (new text-field% [parent frame] [label "Enter The Key Here: A String In Capitals"]
                              [min-width 100]
                              [min-height 20]))
        (new message% [parent frame]
             [label "OR"])
        (define w2 (new horizontal-panel% [parent frame]
                        [alignment '(center top)]))
        (define w3 (new horizontal-panel% [parent frame]
                        [alignment '(center top)]))
        (define w4 (new horizontal-panel% [parent frame]
                        [alignment '(center top)]))
        (define b1 (new button%
                        [parent w2]
                        [label "Generate Given Key"]
                        [callback (lambda (button event)
                                    (begin
                                      (send key-text set-value "CRYPTOGRAPHY")))]))
        (define ms "")
        
        (define b2 (new button%
                        [parent w3]
                        [label "Encrypt"]
                        [callback (lambda (button event)
                                    (let* [(str1 (send vigenere-text get-value))
                                           (cskey (send key-text get-value))]
                                      (if (or (null? (string->list str1))
                                              (null? (string->list cskey)))
                                          (let ((f1 (new frame%
                                                         [label "Error"]
                                                         [width 200]
                                                         [height 100])))
                                            (begin
                                              (define m1 (new message%
                                                              [parent f1]
                                                              [label "You Cannot Leave Input Text Empty"]))
                                              (send f1 show #t)))
                                          (let ((str2 (viginere-encryption str1 cskey)))
                                            (begin (set! ms str2)
                                                   (new message% [parent frame] [label (line-wrap str2)]))))))]))
        (define b3 (new button%
                        [parent frame]
                        [label "Send To Server"]
                        [callback (lambda (button event)
                                    (begin
                                      (set! main-str ms)
                                      (set! main-moe "Viginere")
                                      (set! frames 3)
                                      (send frame show #f)
                                      (CC-gui ms  "Viginere")
                                      (HACKER-gui ms "Viginere")))]))
        (define backButton (new button%
                                [parent frame]
                                [label "Back"]
                                [callback (lambda (button event)
                                            (begin
                                              (set! frames (- frames 1)) 
                                              (send frame show #f)
                                              (call)
                                              ))]))
        (define the-bitmap
        (make-object bitmap% "cool-grunge-computer.jpg"))
      
      (define canvas
        (instantiate canvas%
          (frame)
          (paint-callback
           (lambda (canvas dc)
             (send dc draw-bitmap the-bitmap 0 0)))
          (min-width (send the-bitmap get-width))
          (min-height (send the-bitmap get-height))))
        (when (system-position-ok-before-cancel?)
          (send w1 change-children reverse))
        (send frame show #t)))))

(define AES-gui
  (lambda ()
    (begin
      (let ((frame (new frame% [label "AES ENCRYPTION GUI"]
                        [width 800]
                        [height 300])))
        (define AES-text (new text-field% [parent frame] [label "Enter The Text Here"]))
        (define w1 (new horizontal-panel% [parent frame]
                        [alignment '(center top)]))
        
        
        (define w3 (new horizontal-panel% [parent frame]
                        [alignment '(center top)]))
        (define ms "")
        
        (define b2 (new button%
                        [parent w1]
                        [label "Encrypt"]
                        [callback (lambda (button event)
                                    (let [(str1 (send AES-text get-value))]
                                      (if (null? (string->list str1))
                                          (let ((f1 (new frame%
                                                         [label "Error"]
                                                         [width 200]
                                                         [height 100])))
                                            (begin
                                              (define m1 (new message%
                                                              [parent f1]
                                                              [label "You Cannot Leave Input Text Empty"]))
                                              (send f1 show #t)))
                                          (let ((str2 (AES-text-encryptor str1)))
                                            (begin (set! ms str2)
                                                   (new message% [parent frame] [label (line-wrap str2)]))))))]))
        (define b3 (new button%
                        [parent frame]
                        [label "Send To Server"]
                        [callback (lambda (button event)
                                    (begin
                                      (set! main-str ms)
                                      (set! main-moe "AES")
                                      (set! frames 3)
                                      (send frame show #f)
                                      (CC-gui ms  "AES")
                                      (HACKER-gui ms "AES")))]))
        (define backButton (new button%
                                [parent frame]
                                [label "Back"]
                                [callback (lambda (button event)
                                            (begin
                                              (set! frames (- frames 1)) 
                                              (send frame show #f)
                                              (call)
                                              ))]))
        (define the-bitmap
        (make-object bitmap% "aes.jpg"))
      
      (define canvas
        (instantiate canvas%
          (frame)
          (paint-callback
           (lambda (canvas dc)
             (send dc draw-bitmap the-bitmap 0 0)))
          (min-width (send the-bitmap get-width))
          (min-height (send the-bitmap get-height))))
        (when (system-position-ok-before-cancel?)
          (send w1 change-children reverse))
        (send frame show #t)))))

(define ENIGMA-gui
  (lambda ()
    (begin
      (let ((frame (new frame% [label "ENIGMA ENCRYPTION GUI"]
                        [width 500]
                        [height 300])))
        (define ENIGMA-text (new text-field% [parent frame] [label "Enter The Text Here"]))
        (define w1 (new horizontal-panel% [parent frame]
                        [alignment '(center top)]))
        
        
        (define w3 (new horizontal-panel% [parent frame]
                        [alignment '(center top)]))

        (define ms "")
        
        (define b2 (new button%
                        [parent w1]
                        [label "Encrypt"]
                        [callback (lambda (button event)
                                    (let [(str1 (send ENIGMA-text get-value))]
                                      (if (null? (string->list str1))
                                          (let ((f1 (new frame%
                                                         [label "Error"]
                                                         [width 200]
                                                         [height 100])))
                                            (begin
                                              (define m1 (new message%
                                                              [parent f1]
                                                              [label "You Cannot Leave Input Text Empty"]))
                                              (send f1 show #t)))
                                          (let ((str2 (enigma-encryptor str1)))
                                            (begin (set! ms str2)
                                                   (new message% [parent frame] [label (line-wrap str2)]))))))]))
        (define b3 (new button%
                        [parent frame]
                        [label "Send To Server"]
                        [callback (lambda (button event)
                                    (begin
                                      (set! main-str ms)
                                      (set! main-moe "Enigma")
                                      (set! frames 3)
                                      (send frame show #f)
                                      (CC-gui ms  "Enigma")
                                      (HACKER-gui ms "Enigma")))]))
        (define b5 (new button%
                        [parent frame]
                        [label "Reset Machine"]
                        [callback (lambda (button event)
                                  (set-up-machine))]))
        (define backButton (new button%
                                [parent frame]
                                [label "Back"]
                                [callback (lambda (button event)
                                            (begin
                                              (set! frames (- frames 1)) 
                                              (send frame show #f)
                                              (call)
                                              ))]))
        (define the-bitmap
        (make-object bitmap% "enigma-installed-rotors.jpg"))
      
      (define canvas
        (instantiate canvas%
          (frame)
          (paint-callback
           (lambda (canvas dc)
             (send dc draw-bitmap the-bitmap 0 0)))
          (min-width (send the-bitmap get-width))
          (min-height (send the-bitmap get-height))))
        (when (system-position-ok-before-cancel?)
          (send w1 change-children reverse))
        (send frame show #t)))))

(define (CC-gui str moe)
  (begin
    (let ((frame (new frame% [label "Central Server"]
                      [width 500]
                      [height 300])))
        
      (define m1 (new message%
                      [parent frame]
                      [label str]))
      (define m2 (new message%
                      [parent frame]
                      [label moe]))
      (define w3 (new horizontal-panel% [parent frame]
                      [alignment '(center top)]))

      (define ms "")
        
      (define b2 (new button%
                      [parent w3]
                      [label "Decrypt"]
                      [callback (lambda (button event)
                                  (begin
                                    (cond [(eq? moe "Caesar-Shift") (set! ms (caesar-shift-decryptor str))]
                                          [(eq? moe "Scytale") (set! ms (scytale-decryption str))]
                                          [(eq? moe "OTP") (set! ms (OTP-decryption str))]
                                          [(eq? moe "Viginere") (set! ms (viginere-decryption str))]
                                          [(eq? moe "Enigma") (set! ms (enigma-decryptor str))]
                                          [(eq? moe "AES") (set! ms (AES-text-decryptor str))]
                                          [else (error "Wrong Message")])
                                    (new message% [parent frame]
                                         [label (line-wrap ms)])))]))
        
      (define backButton (new button%
                              [parent frame]
                              [label "Back"]
                              [callback (lambda (button event)
                                          (begin
                                            (set! frames (- frames 1)) 
                                            (send frame show #f)
                                            (if (= frames 2) (display "")
                                                (call))
                                            ))]))
      (define the-bitmap
        (make-object bitmap% "serverroom.jpg"))
      
      (define canvas
        (instantiate canvas%
          (frame)
          (paint-callback
           (lambda (canvas dc)
             (send dc draw-bitmap the-bitmap 0 0)))
          (min-width (send the-bitmap get-width))
          (min-height (send the-bitmap get-height))))

      (when (system-position-ok-before-cancel?)
        (send w3 change-children reverse))
      (send frame show #t))))

(define (HACKER-gui str moe)
  (begin
    (let ((frame (new frame% [label "Hacker Frame"]
                      [width 800]
                      [height 300])))
        
      (define m1 (new message%
                      [parent frame]
                      [label (line-wrap str)]))
      (define m2 (new message%
                      [parent frame]
                      [label moe]))
      (define w3 (new horizontal-panel% [parent frame]
                      [alignment '(center top)]))

      (define ms "")
        
      (define b2 (new button%
                      [parent w3]
                      [label "CAESAR-hack"]
                      [callback (lambda (button event)
                                  (new message% [parent frame]
                                       [label (line-wrap (caesar-shift-Hack str))]))]))
        
      (define b3 (new button%
                      [parent w3]
                      [label "SCYTALE-hack"]
                      [callback (lambda (button event)
                                  (new message% [parent frame]
                                       [label (line-wrap (scytale-Hack str))]))]))
      (define backButton (new button%
                              [parent frame]
                              [label "Back"]
                              [callback (lambda (button event)
                                          (begin
                                            (set! frames (- frames 1)) 
                                            (send frame show #f)
                                            (if (= frames 2) (display "")
                                                (call))))]))
      (define the-bitmap
        (make-object bitmap% "hackpic.jpg"))
      
      (define canvas
        (instantiate canvas%
          (frame)
          (paint-callback
           (lambda (canvas dc)
             (send dc draw-bitmap the-bitmap 0 0)))
          (min-width (send the-bitmap get-width))
          (min-height (send the-bitmap get-height))))
        
      (when (system-position-ok-before-cancel?)
        (send w3 change-children reverse))
      (send frame show #t))))

(define call
  (lambda ()
    (begin
      (define m (get-mouse-click v1))
      ;(ready-mouse-click v1)
      (define p (mouse-click-posn m))
      (let* ((x (posn-x p))
             (y (posn-y p)))
        (cond [(and (> x 0) (< x 220) (< y 220) (> y 0))
               (begin (set! frames (+ frames 1))(caesar-shift-gui))]
              [(and (> x 230) (< x 450) (< y 220) (> y 0))
               (begin (set! frames (+ frames 1))(scytale-gui))]
              [(and (> x 460) (< x 680) (< y 220) (> y 0))
               (begin (set! frames (+ frames 1))(OTP-gui))]
              [(and (> x 690) (< x 900) (< y 220) (> y 0))
               (begin (set! frames (+ frames 1))(viginere-gui))]
              [(and (> x 690) (< x 900) (< y 450) (> y 230))
               (begin (set! frames (+ frames 1))(AES-gui))]
              [(and (> x 0) (< x 220) (< y 450) (> y 230))
               (begin (set! frames (+ frames 1))(ENIGMA-gui))]
              [(and (> x 240) (< x 665) (< y 440) (> y 240))
               (begin (set! frames (+ frames 1))(CC-gui main-str main-moe))]
              [(and (> x 0) (< x 900) (< y 680) (> y 460))
               (begin (set! frames (+ frames 1))(HACKER-gui main-str main-moe))])
        ))))

(call)