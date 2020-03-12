; Exercise 2.74.
;
; Insatiable Enterprises, Inc., is a highly decentralized
; conglomerate company consisting of a large number of independent divisions
; located all over the world. The company's computer facilities have just been
; interconnected by means of a clever network-interfacing scheme that makes the
; entire network appear to any user to be a single computer. Insatiable's
; president, in her first attempt to exploit the ability of the network to
; extract administrative information from division files, is dismayed to
; discover that, although all the division files have been implemented as data
; structures in Scheme, the particular data structure used varies from division
; to division. A meeting of division managers is hastily called to search for a
; strategy to integrate the files that will satisfy headquarters' needs while
; preserving the existing autonomy of the divisions.

; Show how such a strategy can be implemented with data-directed programming.
; As an example, suppose that each division's personnel records consist of a
; single file, which contains a set of records keyed on employees' names. The
; structure of the set varies from division to division. Furthermore, each
; employee's record is itself a set (structured differently from division to
; division) that contains information keyed under identifiers such as address
; and salary. In particular:

; a.  Implement for headquarters a get-record procedure that retrieves a
; specified employee's record from a specified personnel file. The procedure
; should be applicable to any division's file. Explain how the individual
; divisions' files should be structured. In particular, what type information
; must be supplied?

; b.  Implement for headquarters a get-salary procedure that returns the salary
; information from a given employee's record from any division's personnel
; file. How should the record be structured in order to make this operation
; work?

; c.  Implement for headquarters a find-employee-record procedure. This should
; search all the divisions' files for the record of a given employee and return
; the record. Assume that this procedure takes as arguments an employee's name
; and a list of all the divisions' files.

; d.  When Insatiable takes over a new company, what changes must be made in
; order to incorporate the new personnel information into the central system?
; ------------------------------------------------------------

;a) Each division's records consist of a single file, which contains a set of records keyed on employees' names. 
; This single file should be structured as a list of employees' records, with a tag of the division name.
; And each employee's record should have a tag of their name. The way these tags are put must be supplied. 
; E.g. the file of division-1 can be '(division-1 (Mike 2000) (Jack 3500)) Here is the corresponding answer:

 (define (install-division-1-package) 
   ;;internal procedures 
   (define (get-record name file) 
     (cond ((null? file) (error "no result")) 
           ((eq? name (get-name (cadr file))) (cons (cadr file) 
                                                    (get-record name (cdr file)))) 
           (else (get-record name (cdr file))))) 
   (define (get-name record) 
     (car record)) 
    
   ;;interface to the rest of the system 
   (put 'get-record 'division-1 get-record) 
   (put 'get-name 'division-1 get-name) 
   'done) 
  
 (define (get-record name file) 
   (apply-generic 'get-record name file)) 
     
 (define (apply-generic op name file) 
   (let ((division-name (type-tag file))) 
     (let ((proc (get op division-name))) 
       (if proc 
           (proc name file) 
           (error "no result"))))) 
  
 (define (type-tag file) 
   (car file)) 

;b) Take division-1 above as an example, the answer just needs to be modified by adding a few lines:

 ;; Addition to the division-1 package 
 (define (get-salary name file) 
     (cond ((null? file) (error "no result")) 
           ((eq? name (get-name (cadr file))) (cons (cadr (cadr file)) 
                                                    (get-salary name (cdr file)))) 
           (else (get-salary name (cdr file))))) 
  
 (put 'get-salary 'division-1 get-salary) 
  
 ;;Addition to the environment 
 (define (get-salary name file) 
   (apply-generic 'get-salary name file)) 

; c) Just implement "get-record" to all the divisions.

 (define (find-employee-record name list) 
   (if (null? list) 
       (error "no result") 
       (append (get-record (car list)) 
               (find-employee-record name (cdr list))))) 
; d) Install a new package which specifies the procedures to look up name/salary in the new company's file.