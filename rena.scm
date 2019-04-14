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
  (define (substring-over string beginindex endindex)
    (if (< beginindex endindex)
        (substring string beginindex endindex)
        ""))
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
  (define (times mincount maxcount ptn . opt)
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
                      (values (substring-over str lastindex0 indexloop)
                              indexloop
                              attr)))))))))
  (define (delimit ptn delimiter . opt)
    (lambda (str lastindex0 attr)
      (let ((ptn (wrap ptn))
            (delimiter (wrap delimiter))
            (action (if (and (pair? opt) (car opt))
                        (car opt)
                        (lambda (match syn inh) syn))))
        (let loop ((lastindex lastindex0)
                   (index-before lastindex0)
                   (attr attr)
                   (matched-once #f))
          (call-with-values
              (lambda () (ptn str lastindex attr))
            (lambda (match indexnew attrnew)
              (if match
                  (call-with-values
                      (lambda ()
                        (delimiter str
                                   (indexignore str indexnew)
                                   #f))
                    (lambda (dmatch dindexnew dattr)
                      (if dmatch
                          (loop (indexignore str dindexnew)
                                indexnew
                                (action match attrnew attr)
                                #t)
                          (values (substring str lastindex0 indexnew)
                                  indexnew
                                  (action match attrnew attr)))))
                  (if matched-once
                      (values (substring-over str lastindex0 index-before)
                              index-before
                              attr)
                      (values #f #f #f)))))))))
  (define (lookahead ptn pos)
    (lambda (string lastindex attr)
      (let ((ptn (wrap ptn)))
            (call-with-values
                (lambda () (ptn string lastindex attr))
              (lambda (match indexnew attrnew)
                (if (or (and match pos) (and (not match) (not pos)))
                    (values "" lastindex attr)
                    (values #f #f #f)))))))
  (define (set-attr attrnew)
    (lambda (string lastindex attr) (values "" lastindex attrnew)))
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
    (define renabr
      (delay
        (let ((r (force rr)))
          (r 'or "\r\n" "\r" "\n"))))
    (let ((farg (car args)))
      (cond ((eq? farg 'then) (apply renathen (cdr args)))
            ((eq? farg 'or) (apply renaor (cdr args)))
            ((eq? farg 'times) (apply times (cdr args)))
            ((eq? farg 'at-least)
             (times (cadr args)
                    #f
                    (caddr args)
                    (and (pair? (cdddr args)) (cadddr args))))
            ((eq? farg 'at-most)
             (times 0
                    (cadr args)
                    (caddr args)
                    (and (pair? (cdddr args)) (cadddr args))))
            ((eq? farg 'one-or-more)
             (times 1
                    #f
                    (cadr args)
                    (and (pair? (cddr args)) (caddr args))))
            ((eq? farg 'zero-or-more)
             (times 0
                    #f
                    (cadr args)
                    (and (pair? (cddr args)) (caddr args))))
            ((eq? farg 'maybe) (r 'times 0 1 (cadr args)))
            ((eq? farg 'delimit) (apply delimit (cdr args)))
            ((eq? farg 'lookahead) (lookahead (cadr args) #t))
            ((eq? farg 'lookahead-not) (lookahead (cadr args) #f))
            ((eq? farg 'range) (renarange (cadr args) (caddr args)))
            ((eq? farg 'complement) (apply renarange-cmp (cdr args)))
            ((eq? farg 'attr) (set-attr (cadr args)))
            ((eq? farg 'any) rena-any)
            ((eq? farg 'real)
             (r (force renafloat)
                (lambda (match syn inh) (string->number match))))
            ((eq? farg 'whitespace) (force renaspc))
            ((eq? farg 'br) (force renabr))
            ((eq? farg 'y) (apply y-letrec (cdr args)))
            ((null? (cdr args)) (wrap farg))
            (else (wrapaction (wrap farg) (cadr args))))))
  r)

