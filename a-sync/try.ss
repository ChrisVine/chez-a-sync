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

#!r6rs

(library (a-sync try)
  (export try)
  (import (chezscheme))

;; Try is a macro used in the implementation of the event loop
;; provided by this library and has the syntax:
;;
;;   (try body0 body1 ... (except condition cond-clause0 cond-clause1 ...))
;;
;; It has two keywords, 'try' and 'except'.  It executes the body
;; clauses, and if an exception is raised by them passes control to
;; the 'except' block with 'condition' set to the condition which was
;; raised.  The cond-clauses are then evaluated as if in a cond
;; expression.  If one of the cond tests is found to be true, or there
;; is none found to be true but there is an else clause, then the
;; relevant cond clause or else clause will be evaluated and returned.
;; If none of the cond tests in the cond-clauses are found to be true
;; and there is no else clause, the exception condition will be
;; re-raised as if by raise, for an enclosing try form, guard form or
;; with-exception-handler form (if any) to handle.
;;
;; The return value of try, if there is no exception raised by the
;; body clauses or any exception raised is handled by a cond-clause,
;; is the value of the last expression of the body clauses or of the
;; cond-clause, as the case may be.
;;
;; This is similar to but not the same as the R6RS 'guard' form.  The
;; guard form has the property that if no cond test is found to be
;; true in the cond clauses and there is no else clause, the exception
;; condition will be re-raised with raise-continuable.  This is to
;; allow continuable exceptions to propagate successfully through a
;; 'guard' form (albeit triggering any dynamic winds in doing so).  A
;; further consequence is that when the condition is re-raised, that
;; is done in the dynamic environment of the original call to raise or
;; raise-continuable.  As mentioned, the 'try' form always re-raises
;; with 'raise' (so in effect converting any continuable exception to
;; a non-continuable one), and it does so in the dynamic environment
;; in which the cond-clauses are evaluated, which is the dynamic
;; environment of the original call to 'try'.
;;
;; Or put another way, the 'try' macro behaves like a conventional
;; stack unwinding exception implementation.  'guard' does not
;; necessarily do so.
;;
;; Continuable exceptions with generalised handling forms such as
;; 'guard' are generally to be avoided, as they may break any
;; intermediate resource management which uses rethrows or dynamic
;; winds.  Continuable exceptions are also usually unsuitable for use
;; with asynchronous event handlers, which is why the 'try' form is
;; used in the implementation of the event loop provided by this
;; library.  But for cases where that is not true and you know what
;; you are doing, you can use 'guard' instead of 'try' for maintaining
;; continuable exceptions as continuable.
;;
;; The 'try' macro is first available in version 0.3 of this library.
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

) ;; library
