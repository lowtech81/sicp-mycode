(load "queue.scm")
(load "../helpers.scm")

; Exercise 3.21.  Ben Bitdiddle decides to test the queue
; implementation described above. He types in the procedures to the
; Lisp interpreter and proceeds to try them out:

(define q1 (make-queue))
(output (insert-queue! q1 'a))
; ((a) a)
(output (insert-queue! q1 'b))
; ((a b) b)
(output (delete-queue! q1))
; ((b) b)
(output (delete-queue! q1))
; (() b)

; ``It's all wrong!'' he complains. ``The interpreter's response
; shows that the last item is inserted into the queue twice. And when
; I delete both items, the second b is still there, so the queue
; isn't empty, even though it's supposed to be.'' Eva Lu Ator
; suggests that Ben has misunderstood what is happening. ``It's not
; that the items are going into the queue twice,'' she explains.
; ``It's just that the standard Lisp printer doesn't know how to make
; sense of the queue representation. If you want to see the queue
; printed correctly, you'll have to define your own print procedure
; for queues.'' Explain what Eva Lu is talking about. In particular,
; show why Ben's examples produce the printed results that they do.
; Define a procedure print-queue that takes a queue as input and
; prints the sequence of items in the queue.
; ------------------------------------------------------------

; because (insert-queue!) and (delete-queue!) procedures return the 'queue'
; it self, so its a list of the 'items list' as the first element of the 'queue'
; and the rear elemnt as the last element of  the 'queue'
(define print-queue
    (lambda queue
       (display (front-ptr queue))))

(define q1 (make-queue))
(print-queue (insert-queue! q1 'a))
; (a)
(print-queue (insert-queue! q1 'b))
; (a b)
(print-queue (delete-queue! q1))
; (b)
(print-queue (delete-queue! q1))
; 
