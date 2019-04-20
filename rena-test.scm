;;;
;;; Rena-Scheme
;;;
;;; Copyright (c) 2019 Yuichiro MORIGUCHI
;;;
;;; This software is released under the MIT License.
;;; http://opensource.org/licenses/mit-license.php
;;;
(load "rena.scm")

(define (assert r input-str input-attr expect-str expect-index expect-attr)
  (call-with-values
      (lambda () (r input-str 0 input-attr))
    (lambda (str index attr)
      (cond ((not (eqv? expect-str str))
             (display `(failed ,expect-str ,str)))
            ((not (eqv? expect-index index))
             (display `(failed ,expect-index ,index)))
            ((not (eqv? expect-attr attr))
             (display `(failed ,expect-attr ,attr)))
            (else #t)))))
(define (assert-not r input-str)
  (call-with-values
      (lambda () (r input-str 0 #f))
    (lambda (str index attr)
      (cond (str (display '(failed fail ,str)))
            (else #t)))))

(let ((r (rena)))
  (assert (r "765") "765pro" #f "765" 3 #f)
  (assert (r #\2) "27" #f "2" 1 #f)
  (assert-not (r "765") "961")
  (assert-not (r "765") "99")
  (assert-not (r #\3) "27")
  (assert-not (r #\2) "")
  (assert (r 'then "765" "pro") "765pro" #f "765pro" 6 #f)
  (assert-not (r 'then "765" "pro") "961pro")
  (assert-not (r 'then "765" "pro") "765961")
  (assert (r 'or "765" "346") "765" #f "765" 3 #f)
  (assert (r 'or "765" "346") "346" #f "346" 3 #f)
  (assert-not (r 'or "765" "346") "961")
  (assert (r 'times 2 4 "27") "2727" #f "2727" 4 #f)
  (assert (r 'times 2 4 "27") "27272727" #f "27272727" 8 #f)
  (assert (r 'times 2 4 "27") "2727272727" #f "27272727" 8 #f)
  (assert (r 'times 2 2 "27") "272727" #f "2727" 4 #f)
  (assert-not (r 'times 2 4 "27") "27")
  (assert-not (r 'times 2 2 "27") "27")
  (assert-not (r 'times 2 4 "27") "2728")
  (assert (r 'at-least 2 "27") "2727" #f "2727" 4 #f)
  (assert (r 'at-least 2 "27") "27272727" #f "27272727" 8 #f)
  (assert-not (r 'at-least 2 "27") "27")
  (assert-not (r 'at-least 2 "27") "2728")
  (assert (r 'at-most 4 "27") "2727272727" #f "27272727" 8 #f)
  (assert (r 'at-most 4 "27") "27272727" #f "27272727" 8 #f)
  (assert (r 'at-most 4 "27") "272727" #f "272727" 6 #f)
  (assert (r 'at-most 4 "27") "" #f "" 0 #f)
  (assert (r 'one-or-more "27") "27" #f "27" 2 #f)
  (assert (r 'one-or-more "27") "272727" #f "272727" 6 #f)
  (assert-not (r 'one-or-more "27") "2")
  (assert (r 'zero-or-more "27") "27" #f "27" 2 #f)
  (assert (r 'zero-or-more "27") "272727" #f "272727" 6 #f)
  (assert (r 'zero-or-more "27") "2" #f "" 0 #f)
  (assert (r 'maybe "27") "27" #f "27" 2 #f)
  (assert (r 'maybe "27") "272727" #f "27" 2 #f)
  (assert (r 'maybe "27") "2" #f "" 0 #f)
  (assert (r 'delimit "27" ",") "27,27" #f "27,27" 5 #f)
  (assert (r 'delimit "27" ",") "27" #f "27" 2 #f)
  (assert (r 'delimit "27" ",") "27," #f "27" 2 #f)
  (assert (r 'delimit "27" ",") "27,28" #f "27" 2 #f)
  (assert-not (r 'delimit "27" ",") "28")
  (assert (r 'lookahead "765") "765" #f "" 0 #f)
  (assert-not (r 'lookahead "765") "961")
  (assert (r 'lookahead-not "961") "765" #f "" 0 #f)
  (assert-not (r 'lookahead-not "961") "961")
  (assert (r 'range '(#\a #\z)) "a" #f "a" 1 #f)
  (assert (r 'range '(#\a #\z) #\_) "_" #f "_" 1 #f)
  (assert-not (r 'range '(#\a #\z)) "0")
  (assert (r 'complement '(#\a #\z)) "0" #f "0" 1 #f)
  (assert (r 'complement '(#\a #\z) #\_) "0" #f "0" 1 #f)
  (assert-not (r 'complement '(#\a #\z) #\_) "a")
  (assert-not (r 'complement '(#\a #\z) #\_) "_")
  (assert (r 'attr 765) "765" #f "" 0 765)
  (assert (r 'cond (lambda (val) (eqv? val 765))) "765" 765 "" 0 765)
  (assert-not (r 'cond (lambda (val) (eqv? val 765))) "961")
  (assert (r 'any) "765" #f "7" 1 #f)
  (assert-not (r 'any) "")
  (assert (r 'real) "0" #f "0" 1 0)
  (assert (r 'real) "765" #f "765" 3 765)
  (assert (r 'real) "76.5" #f "76.5" 4 76.5)
  (assert (r 'real) "0.765" #f "0.765" 5 0.765)
  (assert (r 'real) ".765" #f ".765" 4 0.765)
  (assert (r 'real) "765e2" #f "765e2" 5 765e2)
  (assert (r 'real) "765E2" #f "765E2" 5 765E2)
  (assert (r 'real) "765e+2" #f "765e+2" 6 765e+2)
  (assert (r 'real) "765e-2" #f "765e-2" 6 765e-2)
  (assert (r 'real) "+765" #f "+765" 4 765)
  (assert (r 'real) "+76.5" #f "+76.5" 5 76.5)
  (assert (r 'real) "+0.765" #f "+0.765" 6 0.765)
  (assert (r 'real) "+.765" #f "+.765" 5 0.765)
  (assert (r 'real) "+765e2" #f "+765e2" 6 765e2)
  (assert (r 'real) "+765E2" #f "+765E2" 6 765E2)
  (assert (r 'real) "+765e+2" #f "+765e+2" 7 765e+2)
  (assert (r 'real) "+765e-2" #f "+765e-2" 7 765e-2)
  (assert (r 'real) "-765" #f "-765" 4 -765)
  (assert (r 'real) "-76.5" #f "-76.5" 5 -76.5)
  (assert (r 'real) "-0.765" #f "-0.765" 6 -0.765)
  (assert (r 'real) "-.765" #f "-.765" 5 -0.765)
  (assert (r 'real) "-765e2" #f "-765e2" 6 -765e2)
  (assert (r 'real) "-765E2" #f "-765E2" 6 -765E2)
  (assert (r 'real) "-765e+2" #f "-765e+2" 7 -765e+2)
  (assert (r 'real) "-765e-2" #f "-765e-2" 7 -765e-2)
  (assert-not (r 'real) "id/")
  (assert-not (r 'real) "")
  (assert (r 'whitespace) "  \t\t\n" #f "  \t\t\n" 5 #f)
  (assert-not (r 'whitespace) "aaa")
  (assert-not (r 'whitespace) "")
  (assert (r 'br) "\r\n" #f "\r\n" 2 #f)
  (assert (r 'br) "\r1" #f "\r" 1 #f)
  (assert (r 'br) "\n1" #f "\n" 1 #f)
  (assert-not (r 'br) "rn"))