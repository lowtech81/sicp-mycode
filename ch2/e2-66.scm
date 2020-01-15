; Exercise 2.66.
;
; Given the generic lookup procedure for the element in the set.
;
; (define (lookup given-key set-of-records)
;   (cond ((null? set-of-records) false)
;         ((equal? given-key (key (car set-of-records)))
;          (car set-of-records))
;         (else (lookup given-key (cdr set-of-records)))))
;
; Implement the lookup procedure for the case where the
; set of records is structured as a binary tree, ordered by the
; numerical values of the keys.
; ------------------------------------------------------------

(load "../common.scm")
(load "2.3.3-binary-trees.scm")
(load "e2-64.scm")

; record representation
(define (make-record key value)
    (cons key value))

(define (key record)
    (car record))

(define (value record)
    (cdr record))

; lookup for records represented as binary tree
(define (lookup given-key set-of-records)
    (let ((entry-key (key (entry set-of-records))))
      (cond ((null? set-of-records) false)
        ((= given-key entry-key) (entry set-of-records))
        ((< given-key entry-key) (lookup given-key (left-branch set-of-records)))
        ((> given-key entry-key) (lookup given-key (left-branch set-of-records)))
        )))

; test
(define records (list->tree (list (make-record 1 'red) (make-record 2 'green) (make-record 3 'blue))))

(value (lookup 2 records))