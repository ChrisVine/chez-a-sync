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

(import (a-sync try)
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
       (format #t "~a: Test ~a OK\n" "test-try.ss" count)
       (set! count (1+ count))))))

;; Test 1: else

(test-result 'test
	     (try
	      (raise 'test)
	      (except c
		      [else c])))
(print-result)

;; Test 2: re-raising condition

(test-result 5
	     (try #f
		  (try (raise 'five)
		       (except c
			       [(eq? c 'six) 6]
			       [(eq? c 'seven) 7]))
		  (except c
			  [(eq? c 'four) 4]
			  [(eq? c 'five) 5])))
(print-result)
