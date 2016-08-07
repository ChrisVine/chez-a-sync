;; Copyright (C) 2016 Chris Vine
;; 
;; This file is licensed under the Apache License, Version 2.0 (the
;; "License"); you may not use this file except in compliance with the
;; License.  You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
;; implied.  See the License for the specific language governing
;; permissions and limitations under the License.


;; The R6RS 'guard' form has the property that if no cond test is
;; found to be true for an exception in the exception handler clauses
;; and there is no else clause, the exception condition will be
;; re-raised with raise-continuable.  This is to allow continuable
;; exceptions to propagate successfully through a 'guard' form.  A
;; further consequence is that when the condition is re-raised, that
;; is done in the dynamic environment of the original call to raise.
;;
;; Continuable exceptions are generally to be avoided, as they subvert
;; the proper flow of program control and may break any resource
;; management which uses rethrows or dynamic winds.  They are also
;; incompatible with asynchronous event handlers.  In generalized
;; guard/try forms it is almost always better and more efficient to
;; re-raise exceptions using raise (so converting any continuable
;; exceptions to non-continuable ones) and when doing so not to
;; restore the dynamic environment to that of the original raise or
;; raise-continuable.  This 'try' form does that.  It's similarity to
;; python's try/except syntax is not accidental.  It uses two
;; keywords, 'try' and 'except'.  The syntactical form is as follows:
;;
;;   (try body0 body1 ... (except condition cond-clause0 cond-clause1 ...))

(define-syntax try
  (lambda (x)
    (syntax-case x (except)
      [(try body0 body1 ... (except condition clause0 clause1 ...))
       #`((call/cc
           (lambda (escape)
             (with-exception-handler
              (lambda (c)
                (escape
                 (lambda ()
                   (let ([condition c])     ;; clauses may set! this
                     #,(let loop ([first #'clause0] [rest #'(clause1 ...)])
                         (if (null? rest)
                             (syntax-case first (else =>)
                               [(else h0 h1 ...) #'(begin h0 h1 ...)]
                               [(tst) #'(let ([t tst]) (if t t (raise c)))]
                               [(tst => l) #'(let ([t tst]) (if t (l t) (raise c)))]
                               [(tst h0 h1 ...) #'(if tst (begin h0 h1 ...) (raise c))])
                             (syntax-case first (=>)
                               [(tst) #`(let ([t tst]) (if t t #,(loop (car rest) (cdr rest))))]
                               [(tst => l) #`(let ([t tst]) (if t (l t) #,(loop (car rest) (cdr rest))))]
                               [(tst h0 h1 ...) #`(if tst (begin h0 h1 ...) #,(loop (car rest) (cdr rest)))])))))))
              (lambda ()
		;; cater for multiple return values
                (call-with-values
                    (lambda () body0 body1 ...)
                  (lambda args
                    (escape (lambda ()
                              (apply values args))))))))))])))
