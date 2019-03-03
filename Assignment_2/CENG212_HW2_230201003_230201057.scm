#lang scheme

(define students-grades

'((ali CENG212 CB)(ayse CENG463 AA)(ahmet CENG213 AA)(ahmet CENG212 FF)(mehmet CENG213 AA)))

(define course-list

'((CENG212 Concepts-of-Programming-Languages) (CENG463

Introduction to Machine Learning) (CENG213 Theory of

Computation)))

;Return a student's grade from a specific course

(define (find-grade name c_name students)
  (cond
    [(empty? students) (print "No such entry")]
    [(and
      (equal? name (car (car students)))
      (equal? c_name (car (cdr (car students)))))
     (car (cdr (cdr (car students))))]
    [else (find-grade name c_name (cdr students))]))

;start examples
;(find-grade (quote ayse) (quote CENG463) students-grades)

;end

;List the students who got AA from a specific course

(define (find-all c_name grade students t_list)
  (cond
    [(empty? students) t_list]
    [(and
      (equal? c_name (car (cdr (car students))))
      (equal? grade (car (cdr (cdr (car students))))))
     (define tt_list (append t_list (list (car (car students)))))
     (find-all c_name grade (cdr students) tt_list)]
    [else (find-all c_name grade (cdr students) t_list)]))

;start examples
;(find-all (quote CENG212) (quote AA) students-grades '())
;(find-all (quote CENG463) (quote BB) students-grades '())
;(find-all (quote CENG213) (quote CC) students-grades '())
;(find-all (quote CENG213) (quote DD) students-grades '())
;end

;How many students failed from a specific course?

(define (how_many c_name students failed)
  (define (course_grade c_name grade students)
  (cond
    [(empty? students) 0]
    [(and
      (equal? (car (cdr (car students))) c_name)
      (equal? (car (cdr (cdr (car students)))) grade))
     (+ 1 (course_grade c_name grade (cdr students)))]
    [else (course_grade c_name grade (cdr students))]))
  (cond
    [(empty? failed) 0]
    [else (+ (course_grade c_name (car failed) students)
             (how_many c_name students (cdr failed)))]))

;start examples
;(how_many (quote CENG213) students-grades (list (quote FF) (quote FD)))
;(how_many (quote CENG212) students-grades (list 'AA))
;end

;Find the students who have succeeded from all courses

(define (list_all_pass students p_list g_students logic)
  (define (find_element a_list element)
  (cond
    [(empty? a_list) 0]
    [(equal? (car a_list) element)
     (+ 1 (find_element (cdr a_list) element))]
    [else (find_element (cdr a_list) element)]))
  (define (failed? name students)
  (cond
    [(empty? students) 0]
    [(and
      (equal? (car (car students)) name)
      (or
       (equal? (car (cdr (cdr (car students)))) (quote FD))
       (equal? (car (cdr (cdr (car students)))) (quote FF))))
     (+ 1 (failed? name (cdr students)))]
    [else (failed? name (cdr students))]))
  (define (all_pass? name students logic)
  (if (xor logic (equal? (failed? name students) 0)) name #f))
  (cond
    [(empty? students) p_list]
    [(equal?
      (all_pass? (car (car students)) g_students logic)
      (car (car students)))
      (define pp_list (if (equal? (find_element p_list (car (car students))) 0)
                       (append p_list (list (car (car students)))) p_list))
      (list_all_pass (cdr students) pp_list g_students logic)]
    [else (list_all_pass (cdr students) p_list g_students logic)]))

;start examples
;(list_all_pass students-grades '() students-grades #f)
;end

;insert function
(define (insert criteria)
  (define howManyElm
    (lambda (list) (howManyElm_ list 0)))
  (define howManyElm_
    (lambda (list count)
      (cond ((equal? null list) count)
            (else (howManyElm_ (cdr list) (+ 1 count))))))
  (cond ((eqv? (howManyElm criteria) 2) (set! course-list (cons criteria course-list)))
        ((eqv? (howManyElm criteria) 3) (set! students-grades (cons criteria students-grades)))))

;(query select name course students-grades course-list)
;(query select course grade students-grades course-list)
;(query select >FD all students-grades course-list)
;(query select <FD all studnets-grades course-list)
;(query select num? course <FD students-grades course-list)



;Query function

(define (query option . cri) 
 (define (return ar_list num)
  (cond
    [(empty? ar_list) #f]
    [(equal? num 0) (car ar_list)]
    [else (return (cdr ar_list) (- num 1))]))
  (define students (return cri (- (length cri) 2)))
  (define courses (return cri (- (length cri) 1)))
  (define (traverse element lst)
  (cond
    [(empty? lst) #f]
    [(equal? element (car lst)) #t]
    [else (traverse element (cdr lst))]))
  (cond
    [(equal? option (quote select))
    (cond
      [(equal?
        (and
         (equal? (return cri 0) (quote >FD))
         (equal? (return cri 1) (quote all))) #t)
       (list_all_pass students '() students #f)]
      [(equal?
        (and
         (equal? (return cri 0) (quote <FD))
         (equal? (return cri 1) (quote all))) #t)
       (list_all_pass students '() students #t)]
      [(equal? (traverse (return cri 1) (list 'AA 'BA 'BB 'CB 'CC 'DC 'DD 'FD 'FF)) #t)
       (find-all (return cri 0) (return cri 1) students '())]
      [(and
         (equal? '<FD (return cri 2))
         (equal? 'num? (return cri 0)))
       (how_many (return cri 1) students-grades (list (quote FF) (quote FD)))]
      [(and
         (equal? '>FD (return cri 2))
         (equal? 'num? (return cri 0)))
       (how_many (return cri 1) students-grades (list 'AA 'BA 'BB 'CB 'CC 'DC 'DD))]
      [(and
         (equal? 'num? (return cri 0))
         (equal? (traverse (return cri 2) (list 'AA 'BA 'BB 'CB 'CC 'DC 'DD 'FD 'FF)) #t))
       (how_many (return cri 1) students-grades (list (return cri 2)))]
      [else
       (find-grade (return cri 0) (return cri 1) students)])]
    [(equal? option (quote insert))
     (insert (return cri 0))]))

;Query examples
(print "Query Examples:")
(query 'select 'ali 'CENG212 students-grades course-list)
(query 'select 'ayse 'CENG463 students-grades course-list)

(query 'select 'CENG213 'AA students-grades course-list)
(query 'select 'CENG212 'BB students-grades course-list)

(query 'select 'num? 'CENG212 '<FD students-grades course-list)
(query 'select 'num? 'CENG212 'AA students-grades course-list)
(query 'select 'num? 'CENG463 '>FD students-grades course-list)

(query 'select '>FD 'all students-grades course-list)

students-grades
(query 'insert '(ali CENG213 FF) )
(query 'select 'ali 'CENG213 students-grades course-list)
students-grades
     
;end



