; Exercise 2.70.
;
; The following eight-symbol alphabet with associated relative
; frequencies was designed to efficiently encode the lyrics of 1950s rock
; songs. (Note that the ``symbols'' of an ``alphabet'' need not be individual
; letters.)

; A     2 NA   16
; BOOM  1 SHA  3
; GET   2 YIP  9
; JOB   2 WAH  1

; Use generate-huffman-tree (exercise 2.69) to generate a corresponding Huffman
; tree, and use encode (exercise 2.68) to encode the following message:

; Get a job

; Sha na na na na na na na na

; Get a job

; Sha na na na na na na na na

; Wah yip yip yip yip yip yip yip yip yip

; Sha boom

; How many bits are required for the encoding? What is the smallest number of
; bits that would be needed to encode this song if we used a fixed-length code
; for the eight-symbol alphabet?
; ------------------------------------------------------------

(load "e2-68.scm")
(load "e2-69.scm")

(define lyrics-tree (generate-huffman-tree '(
    (A 2)
    (GET 2)
    (SHA 3)
    (WAH 1)
    (BOOM 1)
    (JOB 2)
    (NA 16)
    (YIP 9))))

(define message '(
    Get a job
    Sha na na na na na na na na
    Get a job
    Sha na na na na na na na na
    Wah yip yip yip yip yip yip yip yip yip
    Sha boom
))

; There are 84 bits requied for the encoding, calculated as below:
(accumulate (lambda (x y) (+ 1 y)) 0 (encode message lyrics-tree))

; If we use fixed-length code for the eight-symbol alphabet, each symbol cost 3 bits. So
; we need 108 bits for the 36 words in the message, calculated as below:
(accumulate (lambda (x y) (+ 3 y)) 0 message)