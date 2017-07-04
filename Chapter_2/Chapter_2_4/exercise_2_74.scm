; Exercise 2.74. Insatiable Enterprises, Inc., is a highly decentralized
; conglomerate company consisting of a large number of independent divisions
; located all over the world. The company’s computer facilities have just been
; interconnected by means of a clever network-interfacing scheme that makes the
; entire network appear to any user to be a single computer. Insatiable’s
; president, in her first attempt to exploit the ability of the network to
; extract administrative information from division files, is dismayed to
; discover that, although all the division files have been implemented as data
; structures in Scheme, the particular data structure used varies from division
; to division. A meeting of division managers is hastily called to search for
; a strategy to integrate the files that will satisfy headquarters’ needs while
; preserving the existing autonomy of the divisions.

;Show how such a strategy can be implemented with data-directed programming. As
;an example, suppose that each division’s personnel records consist of a single
;file, which contains a set of records keyed on employees’ names. The structure
;of the set varies from division to division. Furthermore, each employee’s
;record is itself a set (structured differently from division to division) that
;contains information keyed under identifiers such as address and salary. In
;particular:
;
;a. Implement for headquarters a get-record procedure that retrieves a specified
;employee’s record from a specified personnel file. The procedure should be
;applicable to any division’s file. Explain how the individual divisions’ files
;should be structured. In particular, what type information must be supplied?

(define (get-record loc key)
  ((get 'get-record loc) key))

; The only information that needs to be supplied is the employee key and a
; function to access the record from the key

;b. Implement for headquarters a get-salary procedure that returns the salary
;information from a given employee’s record from any division’s personnel file.
;How should the record be structured in order to make this operation work?
(define (get-salary loc key)
  ((get 'parse-field loc) ((get 'get-record loc) key)))

; Given that each branch was able to supply the information for a. The only 
; additional information they need to give is a function to pull a field
; given a record

;c. Implement for headquarters a find-employee-record procedure. This should
;search all the divisions’ files for the record of a given employee and return
;the record. Assume that this procedure takes as arguments an employee’s name
;and a list of all the divisions’ files.
(define (find-employee-record locs key)
  (define (iter locs key found-recs)
    (if (null? locs)
          (found-recs)
          ((let ((record (get-record (car locs) key)))
             (if (eq? 'failed record)
                    (iter (cdr locs) key found-recs)
                    (iter (cdr locs) key (cons record found-recs)))))))
  (iter locs key '()))
; this assumes everyones get-salary function will return 'failed if it can't
; find the employee in their records


;d. When Insatiable takes over a new company, what changes must be made in order
;to incorporate the new personnel information into the central system?

; The only thing that needs to be supplied is the informations from parts
; a. and b.
