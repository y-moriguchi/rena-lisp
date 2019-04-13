;;;
;;; Rena-Scheme
;;;
;;; Copyright (c) 2019 Yuichiro MORIGUCHI
;;;
;;; This software is released under the MIT License.
;;; http://opensource.org/licenses/mit-license.php
;;;
(define (rena . opt)
  (define (indexignore str index0)
    (if (or (null? opt) (not (car opt)))
        index0
        (call-with-values
            (lambda () ((car opt) str index0 #f))
          (lambda (match lastindex syn) (if match lastindex index0)))))
  (define (renathen . args)
    (lambda (str lastindex0 attr)
      (let loop
          ((args args)
           (str str)
           (lastindex lastindex0)
           (attr attr))
        (let ((arg (wrap (car args))))
          (call-with-values
              (lambda () (arg str lastindex attr))
            (lambda (match lastindex syn)
              (if match
                 (let ((indexnew (indexignore str lastindex)))
                   (if (null? (cdr args))
                       (values (substring str lastindex0 indexnew)
                               indexnew
                               syn)
                       (loop (cdr args) str indexnew syn)))
                 (values #f #f #f))))))))
  (define (renaor . args)
    (lambda (str lastindex attr)
      (let loop ((args args))
        (if (null? args)
            (values #f #f #f)
            (let ((arg (wrap (car args))))
              (call-with-values
                  (lambda () (arg str lastindex attr))
                (lambda (match lastindex syn)
                  (if match
                      (values match lastindex syn)
                      (loop (cdr args))))))))))
  (define (times ptn mincount maxcount . opt)
    (lambda (str lastindex0 attr)
      (let ((ptn (wrap ptn))
            (action (if (and (pair? opt) (car opt))
                        (car opt)
                        (lambda (match syn inh) syn))))
        (let loop
            ((count 0)
             (indexloop lastindex0)
             (attr attr))
          (call-with-values
              (lambda () (ptn str indexloop attr))
            (lambda (match lastindex syn)
              (if (and match (or (not maxcount) (<= count maxcount)))
                  (loop (+ count 1)
                        (indexignore str lastindex)
                        (action match syn attr))
                  (if (or (< count mincount)
                          (and maxcount (> count maxcount)))
                      (values #f #f #f)
                      (if (= lastindex0 indexloop)
                          (values "" indexloop attr)
                          (values (substring str lastindex0 indexloop)
                                  indexloop
                                  attr))))))))))
  (define (renastr str)
    (lambda (mstr lastindex attr)
      (if (< (+ lastindex (string-length str) -1) (string-length mstr))
          (let* ((indexnew (+ lastindex (string-length str)))
                 (rstr (substring mstr lastindex indexnew)))
            (if (eqv? str rstr)
                (values str indexnew attr)
                (values #f #f #f)))
          (values #f #f #f))))
  (define (renarange start end)
    (lambda (mstr lastindex attr)
      (if (< lastindex (string-length mstr))
          (let ((ch (string-ref mstr lastindex)))
             (if (and (char<=? start ch) (char<=? ch end))
                (values (string ch) (+ lastindex 1) attr)
                (values #f #f #f)))
          (values #f #f #f))))
  (define (renarange-cmp . chars)
    (lambda (mstr lastindex attr)
      (if (< lastindex (string-length mstr))
          (let ((ch (string-ref mstr lastindex)))
            (let loop ((chars chars))
              (cond ((null? chars)
                     (values (string ch) (+ lastindex 1) attr))
                    ((char=? (car chars) ch) (values #f #f #f))
                    (else (loop (cdr chars))))))
          (values #f #f #f))))
  (define (rena-any mstr lastindex attr)
    (if (< lastindex (string-length mstr))
        (values (substring mstr lastindex (+ lastindex 1))
                (+ lastindex 1)
                attr)
        (values #f #f #f)))
  (define (wrapaction matcher action)
    (lambda (mstr lastindex attr)
      (call-with-values
          (lambda () (matcher mstr lastindex attr))
        (lambda (match lastindex syn)
          (if match
              (values match lastindex (action match syn attr))
              (values #f #f #f))))))
  (define (wrap obj)
    (cond ((string? obj) (renastr obj))
          (else obj)))
  (define (y-letrec . args)
    (letrec
        ((f (lambda (g) (g g)))
         (h (lambda (p)
              (let loop ((args args))
                (if (null? args)
                    '()
                    (cons (lambda (match index attr)
                            ((apply (car args) (p p)) match index attr))
                          (loop (cdr args))))))))
      (car (f h))))
  (define (regex pattern)
    (define (result matchstr lastindex attr)
      (define (then pattern lastindex capture)
        (cond ((null? pattern) (values lastindex capture))
              ((and (pair? (car pattern)) (eq? (caar pattern) 'times))
               (let ((mincount (cadar pattern))
                     (maxcount (caddar pattern)))
                 (let loop ((count 0)
                            (lastindex lastindex)
                            (capture capture))
                   (if (or (not maxcount) (< count maxcount))
                       (call-with-values
                           (lambda ()
                             (dispatch (cadddr (car pattern))
                                       lastindex
                                       capture))
                         (lambda (indexnew capturenew)
                           (define (failed pattern lastindex capture)
                             (if (or (and maxcount (> count maxcount))
                                     (< count mincount))
                                 (values #f #f)
                                 (then pattern lastindex capture)))
                           (cond (indexnew
                                  (call-with-values
                                      (lambda ()
                                        (loop (+ count 1)
                                              indexnew
                                              capturenew))
                                    (lambda (index2 capture2)
                                      (if index2
                                          (values index2 capture2)
                                          (failed (cdr pattern)
                                                  indexnew
                                                  capturenew)))))
                                 (else (failed (cdr pattern)
                                               lastindex
                                               capture)))))
                       (then (cdr pattern) lastindex capture)))))
              ((and (pair? (car pattern))
                    (eq? (caar pattern) 'times-nongreedy))
               (let ((mincount (cadar pattern))
                     (maxcount (caddar pattern)))
                 (let loop ((count 0)
                            (lastindex lastindex)
                            (capture capture))
                   (call-with-values
                       (lambda ()
                         (dispatch (cadddr (car pattern))
                                   lastindex
                                   capture))
                     (lambda (indexnew capturenew)
                       (cond ((and maxcount (> count maxcount))
                              (values #f #f))
                             ((< count mincount)
                              (loop (+ count 1)
                                    indexnew
                                    capturenew))
                             (else
                              (call-with-values
                                  (lambda () (then (cdr pattern)
                                                   indexnew
                                                   capturenew))
                                (lambda (index2 capture2)
                                  (if index2
                                      (values index2 capture2)
                                      (loop (+ count 1)
                                            indexnew
                                            capturenew)))))))))))
              (else
               (call-with-values
                   (lambda () (dispatch (car pattern) lastindex capture))
                 (lambda (lastindex capture)
                   (if lastindex
                       (then (cdr pattern) lastindex capture)
                       (values #f #f)))))))
      (define (re-or pattern lastindex capture)
        (if (null? pattern)
            (values #f #f)
            (call-with-values
                (lambda () (dispatch (car pattern) lastindex capture))
              (lambda (indexnew capturenew)
                (if indexnew
                    (values indexnew capturenew)
                    (re-or (cdr pattern) lastindex capture))))))
      (define (matchchar chstart chend)
        (lambda (lastindex capture)
          (if (>= lastindex (string-length matchstr))
              (values #f #f)
              (let ((ch (string-ref matchstr lastindex)))
                (if (and (char>=? ch chstart) (char<=? ch chend))
                    (values (+ lastindex 1) capture)
                    (values #f #f))))))
      (define (matchrange . chars)
        (lambda (lastindex capture)
            (cond ((null? chars) (values #f #f))
                ((pair? (car chars))
                 (if (and (char? (caar chars)) (char? (cadar chars)))
                     ((matchchar (caar chars) (cadar chars)) lastindex capture)
                     (values #f #f)))
                ((char? (car chars))
                 ((matchchar (car chars) (car chars)) lastindex capture))
                (else (values #f #f)))))
      (define (matchrange-cmp . chars)
        (lambda (lastindex capture)
          (if (>= lastindex (string-length matchstr))
              (values #f #f)
              (call-with-values
                  (lambda () ((apply matchrange chars) lastindex capture))
                (lambda (indexnew capturenew)
                  (if indexnew
                      (values #f #f)
                      (values (+ lastindex 1) capture)))))))
      (define (matchstring str)
        (lambda (lastindex capture)
          (if (< (+ lastindex (string-length str) -1)
                 (string-length matchstr))
              (let* ((rindex (+ lastindex (string-length str)))
                     (rstr (substring matchstr lastindex rindex)))
                (if (string=? str rstr)
                    (values rindex capture)
                    (values #f #f))))))
      (define (setassv obj val alist)
        (cond ((null? alist) (cons (cons obj val) '()))
              ((eqv? obj (caar alist)) (cons (cons obj val) (cdr alist)))
              (else (cons (car alist) (setassv obj val (cdr alist))))))
      (define (re-capture-start name)
        (lambda (lastindex capture)
          (values lastindex (setassv name lastindex capture))))
      (define (re-capture-end name)
        (lambda (lastindex capture)
          (values lastindex
                  (setassv name
                           (substring matchstr
                                      (cdr (assv name capture))
                                      lastindex)
                           capture))))
      (define (refer name)
        (lambda (lastindex capture)
          (let ((captured (assv name capture)))
            (if captured
                ((matchstring (cdr captured)) lastindex capture)
                (values #f #f)))))
      (define (re-begin lastindex capture)
        (if (<= lastindex 0)
            (values lastindex capture)
            (values #f #f)))
      (define (re-end lastindex capture)
        (if (>= lastindex (string-length matchstr))
            (values lastindex capture)
            (values #f #f)))
      (define (dispatch pattern lastindex capture)
        (cond ((null? pattern) (values lastindex capture))
              ((char? pattern)
               ((matchchar pattern pattern) lastindex capture))
              ((string? pattern)
               ((matchstring pattern) lastindex capture))
              ((eq? pattern 'begin) (re-begin lastindex capture))
              ((eq? pattern 'end) (re-end lastindex capture))
              ((not (pair? pattern)) (values #f #f))
              ((eq? (car pattern) 'or)
               (re-or (cdr pattern) lastindex capture))
              ((eq? (car pattern) 'capture-start)
               (let ((cap (re-capture-start (cadr pattern))))
                 (cap lastindex capture)))
              ((eq? (car pattern) 'capture-end)
               (let ((cap (re-capture-end (cadr pattern))))
                 (cap lastindex capture)))
              ((eq? (car pattern) 'refer)
               ((refer (cadr pattern)) lastindex capture))
              ((eq? (car pattern) 'range)
               ((apply matchrange (cdr pattern)) lastindex capture))
              ((eq? (car pattern) 'range-complement)
               ((apply matchrange-cmp (cdr pattern)) lastindex capture))
              (else (then pattern lastindex capture))))
      (call-with-values
          (lambda () (dispatch pattern lastindex '()))
        (lambda (indexnew capture)
          (if indexnew
              (values (substring matchstr lastindex indexnew)
                      indexnew
                      capture)
              (values #f #f #f)))))
    result)
  (define (regex-parser)
    (define capture-count 1)
    (define rr (delay (rena)))
    (define renanum
      (delay
        (let ((r (force rr)))
          (r 'one-or-more (r 'range #\0 #\9)))))
    (y-letrec
     (lambda (orr andr repeat element charset)
       (r
        (r 'then
           (r andr (lambda (match syn inh) (list syn)))
           (r 'zero-or-more
              (r 'then
                 "|"
                 (r andr (lambda (match syn inh) (cons syn inh))))))
        (lambda (match syn inh) (cons 'or (reverse syn)))))
     (lambda (orr andr repeat element charset)
       (define (checkcap syn)
         (and (pair? syn)
              (pair? (car syn))
              (eq? (caar syn) 'capture-end)))
       (define (remove-unary-or syn)
         (cond ((null? syn) '())
               ((and (pair? (car syn))
                     (eq? (caar syn) 'or)
                     (= (length (car syn)) 2))
                (append (cadar syn)
                        (remove-unary-or (cdr syn))))
               (else (cons (car syn) (remove-unary-or (cdr syn))))))
       (r
        (r 'then
           (r repeat (lambda (match syn inh)
                       (if (checkcap syn) syn (list syn))))
           (r 'zero-or-more
              (r repeat (lambda (match syn inh)
                          (if (checkcap syn)
                              (append syn inh)
                              (cons syn inh))))))
        (lambda (match syn inh) (reverse (remove-unary-or syn)))))
     (lambda (orr andr repeat element charset)
       (r 'then
          element
          (r 'maybe
             (r 'or
                (r "*?" (lambda (match syn inh)
                          (list 'times-nongreedy 0 #f syn)))
                (r "+?" (lambda (match syn inh)
                          (list 'times-nongreedy 1 #f syn)))
                (r "*" (lambda (match syn inh)
                         (list 'times 0 #f syn)))
                (r "+" (lambda (match syn inh)
                         (list 'times 1 #f syn)))
                (r
                 (r 'then
                    "{"
                    (r (force renanum)
                       (lambda (match syn inh)
                         (list (string->number match) inh)))
                    ","
                    (r (force renanum)
                       (lambda (match syn inh)
                         (cons (string->number match) inh)))
                    "}")
                 (lambda (match syn inh)
                   (list 'times (cadr syn) (car syn) (caddr syn))))
                (r
                 (r 'then
                    "{"
                    (r (force renanum)
                       (lambda (match syn inh)
                         (list (string->number match) inh)))
                    ",}")
                 (lambda (match syn inh)
                   (list 'times (car syn) #f (cadr syn))))
                (r
                 (r 'then
                    "{"
                    (r (force renanum)
                       (lambda (match syn inh)
                         (list (string->number match) inh)))
                    "}")
                 (lambda (match syn inh)
                   (list 'times (car syn) (car syn) (cadr syn))))))))
     (lambda (orr andr repeat element charset)
       (r 'or
          (r 'then "(?:" orr ")")
          (r (r 'then "(" orr ")")
             (lambda (match syn inh)
               (let ((capcount capture-count))
                 (set! capture-count (+ capture-count 1))
                 (cons (list 'capture-end capcount)
                       (cons syn (list (list 'capture-start capcount)))))))
          (r (r 'then "[^" charset "]")
             (lambda (match syn inh) (cons 'range-complement syn)))
          (r (r 'then "[" charset "]")
             (lambda (match syn inh) (cons 'range syn)))
          (r "." (lambda (match syn inh)
                   (list 'range-complement #\newline)))
          (r "^" (lambda (match syn inh) 'begin))
          (r "$" (lambda (match syn inh) 'end))
          (r
           (r 'then
              "\\"
              (r (force renanum)
                 (lambda (match syn inh) (string->number match))))
           (lambda (match syn inh) (list 'refer syn)))
          (r (r 'complement #\* #\+ #\? #\| #\( #\) #\{)
             (lambda (match syn inh) (string-ref match 0)))))
     (lambda (orr andr repeat element charset)
       (r
        (r 'then
           (r "" (lambda (match syn inh) '()))
           (r 'zero-or-more
              (r 'then
                 (r (r 'complement #\])
                    (lambda (match syn inh)
                      (cons (string-ref match 0) inh)))
                 (r 'maybe
                    (r 'then
                       "-"
                       (r (r 'any)
                          (lambda (match syn inh)
                            (cons (list (car inh) (string-ref match 0))
                                  (cdr inh)))))))))
        (lambda (match syn inh) syn)))))
  (define (r . args)
    (define rr (delay (rena)))
    (define renanum
      (delay
        (let ((r (force rr)))
          (r 'one-or-more (r 'range #\0 #\9)))))
    (define renafloat
      (delay
        (let ((r (force rr)))
          (r 'then
             (r 'maybe (r 'or "+" "-"))
             (r 'or
                (r 'then
                   (r 'zero-or-more (r 'range #\0 #\9))
                    "."
                   (force renanum))
                (force renanum))
             (r 'maybe
                (r 'then
                   (r 'or "e" "E")
                   (r 'maybe (r 'or "+" "-"))
                   (force renanum)))))))
    (define renaspc
      (delay
        (let ((r (force rr)))
          (r 'one-or-more (r 'or " " "\t" "\n")))))
    (let ((farg (car args)))
      (cond ((eq? farg 'then) (apply renathen (cdr args)))
            ((eq? farg 'or) (apply renaor (cdr args)))
            ((eq? farg 'times) (apply times (cdr args)))
            ((eq? farg 'at-least)
             (times (cadr args)
                    (caddr args)
                    #f
                    (and (pair? (cdddr args)) (cadddr args))))
            ((eq? farg 'at-most)
             (times (cadr args)
                    0
                    (caddr args)
                    (and (pair? (cdddr args)) (cadddr args))))
            ((eq? farg 'one-or-more)
             (times (cadr args)
                    1
                    #f
                    (and (pair? (cddr args)) (caddr args))))
            ((eq? farg 'zero-or-more)
             (times (cadr args)
                    0
                    #f
                    (and (pair? (cddr args)) (caddr args))))
            ((eq? farg 'maybe) (r 'times (cadr args) 0 1))
            ((eq? farg 'range) (renarange (cadr args) (caddr args)))
            ((eq? farg 'complement) (apply renarange-cmp (cdr args)))
            ((eq? farg 'any) rena-any)
            ((eq? farg 'real)
             (r (force renafloat)
                (lambda (match syn inh) (string->number match))))
            ((eq? farg 'whitespace) (force renaspc))
            ((eq? farg 'regex)
             (call-with-values
                 (lambda () ((regex-parser) (cadr args) 0 '()))
               (lambda (str lastindex attr) (regex attr))))
            ((eq? farg 'sregex) (regex (cadr args)))
            ((eq? farg 'y) (apply y-letrec (cdr args)))
            ((null? (cdr args)) (wrap farg))
            (else (wrapaction (wrap farg) (cadr args))))))
  r)

