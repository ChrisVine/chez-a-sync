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

(import (a-sync coroutines)
	(a-sync event-loop)
	(chezscheme))

;; helpers

(define-syntax test-result
  (syntax-rules ()
    [(_ expected res)
     (assert (eqv? expected res))]))

(define print-result
  ((lambda ()
     (define count 1)
     (lambda ()
       (format #t "~a: Test ~a OK\n" "test-coroutines.ss" count)
       (set! count (1+ count))))))

;; Test 1: make-iterator

(define iter (make-iterator (lambda (yield)
			      (let loop ((val 0))
				(when (< val 3)
				  (loop (yield val)))))))
(test-result 0 (iter))
(test-result 1 (iter 1))
(test-result 2 (iter 2))
(test-result 'stop-iteration (iter 3))
(test-result 'stop-iteration (iter 0))
(print-result)

;; Test 2: make-coroutine

(define cor (make-coroutine (lambda (yield)
			      (let loop ((val 0))
				(if (< val 3)
				    (loop (yield val))
				    'end)))))

(let-values ([(resume arg) (cor)])
  (set! cor resume)
  (test-result 0 arg))
(let-values ([(resume arg) (cor 1)])
  (set! cor resume)
  (test-result 1 arg))
(let-values ([(resume arg) (cor 2)])
  (set! cor resume)
  (test-result 2 arg))
(let-values ([(resume arg) (cor 3)])
  (test-result #f resume)
  (test-result 'end arg))
(print-result)

;; Test 3: a-sync

(define main-loop (make-event-loop))
(a-sync (lambda (await resume)
	  (event-post! (lambda ()
			 (resume 5))
		       main-loop)
	  (test-result 5 (await))
	  (print-result)))
(event-loop-run! main-loop)
