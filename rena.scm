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
      (if (< lastindex (string-length mstr))
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
            ((eq? farg 'real)
             (r (force renafloat)
                (lambda (match syn inh) (string->number match))))
            ((eq? farg 'whitespace) (force renaspc))
            ((null? (cdr args)) (wrap farg))
            (else (wrapaction (wrap farg) (cadr args))))))
  r)

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
