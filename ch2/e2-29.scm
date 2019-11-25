
; Exercise 2.29.
;
; A binary mobile consists of two branches, a left branch and a right branch.
; Each branch is a rod of a certain length, from which hangs either a weight or
; another binary mobile. We can represent a binary mobile using compound data by
; constructing it from two branches (for example, using list):

(define (make-mobile left right)
  (list left right))

; A branch is constructed from a length (which must be a number) together
; with a structure, which may be either a number (representing a simple weight) or another mobile:

(define (make-branch length structure)
  (list length structure))

; a.  Write the corresponding selectors left-branch and right-branch,
;     which return the branches of a mobile, and branch-length and
;     branch-structure, which return the components of a branch.

(define (left-branch mobile)
    (car mobile))

(define (right-branch mobile)
    (cadr mobile))

(define (branch-length branch)
    (car branch))

(define (branch-structure branch)
    (cadr branch))

; b.  Using your selectors, define a procedure total-weight that returns
;     the total weight of a mobile.

(define (is-mobile? structure)
    (and (pair? structure) (pair? (car structure)) (pair? (cadr structure))))

(define (is-branch? structure)
    (and (pair? structure) (not (pair? (car structure)))))

(define (is-weight? structure)
    (not (pair? structure)))

(define (total-weight structure)
    (cond ((null? structure) 0)
          ((not (pair? structure)) structure)
          ((is-mobile? structure)
            (+ (total-weight (left-branch structure))
               (total-weight (right-branch structure))))
          (else
            (total-weight (branch-structure structure)))))

(total-weight (list (list 1 (list (list 3 5) (list 4 7))) (list 1 2)))

(total-weight (list (list (list 1 10) (list 2 20)) (list (list 3 30) (list 4 40))))

(total-weight (list (list 4 5) (list 6 (list (list 2 3) (list 3 (list (list 5 10) (list 2 11)))))))

; c.  A mobile is said to be balanced if the torque applied by its top-left
;     branch is equal to that applied by its top-right branch (that is, if the
;     length of the left rod multiplied by the weight hanging from that rod is
;     equal to the corresponding product for the right side) and if each of the
;     submobiles hanging off its branches is balanced. Design a predicate that
;     tests whether a binary mobile is balanced.

(define (torque-eq? structure)
    (= (* (branch-length (left-branch structure)) (total-weight (left-branch structure)))
       (* (branch-length (right-branch structure)) (total-weight (right-branch structure)))))

(define (balanced? mobile)
    (if (is-mobile? mobile)
        (and 
            (torque-eq? mobile)
            (balanced? (branch-structure (left-branch mobile)))
            (balanced? (branch-structure (right-branch mobile))))
        #t))

(balanced? (list (list 2 (list (list 1 3) (list 1 3))) (list 3 (list (list 2 2) (list 2 2))))) ;#t

(balanced? (list (list 4 5) (list 2 (list (list 2 3) (list 3 (list (list 5 10) (list 2 11))))))) ;#f



; d.  Suppose we change the representation of mobiles so that the constructors are

(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

; How much do you need to change your programs to convert to the new representation?
; ----------------------------------------------------------------------------------

; (define new-mobile (make-mobile (make-branch 2 (make-mobile (make-branch 1 3) (make-branch 1 3)))
            ;  (make-branch 3 (make-mobile (make-branch 2 2) (make-branch 2 2)))))

(define new-mobile (make-mobile (make-branch 1 3) (make-branch 2 2)))

(define (right-branch mobile)
    (cdr mobile))

(define (branch-structure branch)
    (cdr branch))

(display new-mobile)

(right-branch new-mobile)

(left-branch new-mobile)

(branch-structure (left-branch new-mobile)) 

(total-weight new-mobile)

(balanced? new-mobile)

