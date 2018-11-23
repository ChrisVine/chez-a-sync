;; Copyright (C) 2017 Chris Vine
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

(import (a-sync thread-pool)
	(a-sync coroutines)
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
       (format #t "~a: Test ~a OK\n" "test-thread-pool.ss" count)
       (set! count (1+ count))))))

;; Test 1: default settings

(let ([pool (make-thread-pool 4)])
  (test-result 0 (thread-pool-get-num-tasks pool))
  (test-result 4 (thread-pool-get-num-threads pool))
  (test-result 4 (thread-pool-get-size pool))
  (test-result #f (thread-pool-get-non-blocking pool))
  (thread-pool-stop! pool)
  (print-result))

;; Test 2: supplied settings

(let ([pool (make-thread-pool 4 #t)])
  (test-result 0 (thread-pool-get-num-tasks pool))
  (test-result 4 (thread-pool-get-num-threads pool))
  (test-result 4 (thread-pool-get-size pool))
  (test-result #t (thread-pool-get-non-blocking pool))

  (thread-pool-set-non-blocking! pool #f)
  (test-result #f (thread-pool-get-non-blocking pool))

  (thread-pool-change-size! pool -1)
  (test-result 3 (thread-pool-get-size pool))
  (thread-pool-change-size! pool 3)
  (test-result 6 (thread-pool-get-num-threads pool))
  (test-result 6 (thread-pool-get-size pool))

  (thread-pool-stop! pool)
  (print-result))

;; Test 3: thread-pool-add! without fail handler

(let ([pool (make-thread-pool 3)]
      [mutex (make-mutex)]
      [condvar (make-condition)]
      [count 0])
  (thread-pool-add! pool (lambda ()
			   ;; sleep so that we can test num-tasks below
			   (sleep (make-time 'time-duration 100000000 0))
			   (with-mutex mutex
			     (set! count (1+ count))
			     (condition-signal condvar))))
  (thread-pool-add! pool (lambda ()
			   ;; sleep so that we can test num-tasks below
			   (sleep (make-time 'time-duration 100000000 0))
			   (with-mutex mutex
			     (set! count (1+ count)))
			   (condition-signal condvar)))
  (thread-pool-add! pool (lambda ()
			   ;; sleep so that we can test num-tasks below
			   (sleep (make-time 'time-duration 100000000 0))
			   (with-mutex mutex
			     (set! count (1+ count))
			     (condition-signal condvar))))
  (thread-pool-add! pool (lambda ()
			   (with-mutex mutex
			     (set! count (1+ count))
			     (condition-signal condvar))))
  (thread-pool-add! pool (lambda ()
			   (with-mutex mutex
			     (set! count (1+ count))
			     (condition-signal condvar))))
  (test-result 5 (thread-pool-get-num-tasks pool))
  (test-result 3 (thread-pool-get-num-threads pool))
  (with-mutex mutex
    (do ()
	((= count 5))
      (condition-wait condvar mutex)))
  (test-result 5 count)
  (test-result 3 (thread-pool-get-num-threads pool))
  ;; give enough time for the pool threads to reset num-tasks when
  ;; each task returns before we test this
  (sleep (make-time 'time-duration 50000000 0))
  (test-result 0 (thread-pool-get-num-tasks pool))
  (thread-pool-stop! pool)
  (print-result))

;; Test 4: thread-pool-add! with fail handler

(let ([pool (make-thread-pool 3)]
      [mutex (make-mutex)]
      [condvar (make-condition)]
      [count 0])
  (thread-pool-add! pool
		    (lambda ()
		      ;; sleep so that we can test num-tasks below
		      (sleep (make-time 'time-duration 100000000 0))
		      (with-mutex mutex
			(set! count (1+ count))
			(condition-signal condvar)))
		    (lambda (c)
		      (assert #f))) ;; we should never reach here
  (thread-pool-add! pool
		    (lambda ()
		      ;; sleep so that we can test num-tasks below
		      (sleep (make-time 'time-duration 100000000 0))
		      (with-mutex mutex
			(set! count (1+ count))
			(condition-signal condvar)))
		    (lambda (c)
		      (assert #f))) ;; we should never reach here
  (thread-pool-add! pool
		    (lambda ()
		      ;; sleep so that we can test num-tasks below
		      (sleep (make-time 'time-duration 100000000 0))
		      (with-mutex mutex
			(set! count (1+ count))
			(condition-signal condvar)))
		    (lambda (c)
		      (assert #f))) ;; we should never reach here
  (thread-pool-add! pool
		    (lambda ()
		      (with-mutex mutex
			(set! count (1+ count))
			(condition-signal condvar)))
		    (lambda (c)
		      (assert #f))) ;; we should never reach here
  (thread-pool-add! pool
		    (lambda ()
		      (with-mutex mutex
			(set! count (1+ count))
			(condition-signal condvar)))
		    (lambda (c)
		      (assert #f))) ;; we should never reach here
  (test-result 5 (thread-pool-get-num-tasks pool))
  (test-result 3 (thread-pool-get-num-threads pool))
  (with-mutex mutex
    (do ()
	((= count 5))
      (condition-wait condvar mutex)))
  (test-result 5 count)
  (thread-pool-stop! pool)
  (print-result))

;; Test 5: thread-pool-add! with throwing task

(let ([pool (make-thread-pool 4)]
      [mutex (make-mutex)]
      [condvar (make-condition)]
      [count 0])
  (thread-pool-add! pool
		    (lambda ()
		      (with-mutex mutex
			(set! count (1+ count))
			(condition-signal condvar)))
		    (lambda (c)
		      (assert #f))) ;; we should never reach here
  (thread-pool-add! pool
		    (lambda ()
		      (raise 'quelle-horreur)
		      (assert #f))  ;; we should never reach here
		    (lambda (c)
		      (test-result 'quelle-horreur c)
		      (with-mutex mutex
			(set! count (1+ count))
			(condition-signal condvar))))
  (with-mutex mutex
    (do ()
	((= count 2))
      (condition-wait condvar mutex)))
  (test-result 2 count)
  (thread-pool-stop! pool)
  (print-result))

;; Test 6: thread-pool-stop! with queued tasks (blocking)

(let ([pool (make-thread-pool 4 #f)]
      [mutex (make-mutex)]
      [count 0])
  (thread-pool-add! pool (lambda ()
			   (sleep (make-time 'time-duration 50000000 0))
			   (with-mutex mutex
			     (set! count (1+ count)))))
  (thread-pool-add! pool (lambda ()
			   (sleep (make-time 'time-duration 50000000 0))
			   (with-mutex mutex
			     (set! count (1+ count)))))
  (thread-pool-add! pool (lambda ()
			   (sleep (make-time 'time-duration 50000000 0))
			   (with-mutex mutex
			     (set! count (1+ count)))))
  (thread-pool-add! pool (lambda ()
			   (sleep (make-time 'time-duration 50000000 0))
			   (with-mutex mutex
			     (set! count (1+ count)))))
  (thread-pool-stop! pool)
  (test-result 4 count)
  (print-result))

;; Test 7: thread-pool-stop! with queued tasks (non-blocking)

(let ([pool (make-thread-pool 4 #t)]
      [mutex (make-mutex)]
      [condvar (make-condition)]
      [count 0])
  (thread-pool-add! pool (lambda ()
			   ;; sleep so when first tested, count is 0
			   (sleep (make-time 'time-duration 100000000 0))
			   (with-mutex mutex
			     (set! count (1+ count))
			     (condition-signal condvar))))
  (thread-pool-add! pool (lambda ()
			   ;; sleep so when first tested, count is 0
			   (sleep (make-time 'time-duration 100000000 0))
			   (with-mutex mutex
			     (set! count (1+ count))
			     (condition-signal condvar))))
  (thread-pool-add! pool (lambda ()
			   ;; sleep so when first tested, count is 0
			   (sleep (make-time 'time-duration 100000000 0))
			   (with-mutex mutex
			     (set! count (1+ count))
			     (condition-signal condvar))))
  (thread-pool-add! pool (lambda ()
			   ;; sleep so when first tested, count is 0
			   (sleep (make-time 'time-duration 100000000 0))
			   (with-mutex mutex
			     (set! count (1+ count))
			     (condition-signal condvar))))
  (thread-pool-stop! pool)
  (test-result 0 count)
  (with-mutex mutex
    (do ()
	((= count 4))
      (condition-wait condvar mutex)))
  (test-result 4 count)
  (print-result))

;; Test 8: await-task-in-thread-pool! without handler

(define main-loop (make-event-loop))

(let ([pool (make-thread-pool 2)])
  (event-loop-block! #t main-loop)
  (a-sync (lambda (await resume)
	    (let ([res
		   (await-task-in-thread-pool! await resume main-loop pool
					       (lambda ()
						 (+ 5 10)))])
	      (test-result 15 res)
	      (print-result)
	      (event-loop-quit! main-loop)
	      (event-loop-block! #f main-loop))))
  (event-loop-run! main-loop)
  (thread-pool-stop! pool))

;; Test 9: await-task-in-thread-pool! with handler

(let ([pool (make-thread-pool 2)])
  (event-loop-block! #t main-loop)
  (a-sync (lambda (await resume)
	    (let ([res
		   (await-task-in-thread-pool!
		    await resume main-loop pool
		    (lambda () (raise 'test-exception))
		    (lambda (c)
		      (test-result 'test-exception c)
		      5))])
	      (test-result 5 res)
	      (print-result)
	      (event-loop-quit! main-loop)
	      (event-loop-block! #f main-loop))))
  (event-loop-run! main-loop)
  (thread-pool-stop! pool))

;; Test 10: await-generator-in-thread-pool! without handler

(let ([pool (make-thread-pool 2)]
      [lst '()])
  (event-loop-block! #t main-loop)
  (a-sync (lambda (await resume)
	    (let ([res
		   (await-generator-in-thread-pool! await resume main-loop pool
						    (lambda (yield)
						      (let loop ((count 0))
							(when (< count 5)
							  (yield (* 2 count))
							  (loop (1+ count)))))
						    (lambda (val)
						      (set! lst (cons val lst))))])
	      (test-result (car lst) 8)
	      (test-result (length lst) 5)
	      (test-result res #f)
	      (print-result)
	      (event-loop-block! #f main-loop))))
  (event-loop-run! main-loop)
  (thread-pool-stop! pool))

;; Test 11: await-generator-in-thread-pool! with handler

(let ((pool (make-thread-pool 2))
      (lst '()))
  (event-loop-block! #t main-loop)
  (a-sync (lambda (await resume)
	    (let ((res
		   (await-generator-in-thread-pool! await resume main-loop pool
						    (lambda (yield)
						      (let loop ((count 0))
							(cond
							 [(< count 5)
							  (yield (* 2 count))
							  (loop (1+ count))]
							 [(= count 5)
							  (raise 'my-exception)
							  ;; we never reach here
							  (yield (* 2 count))
							  (loop (1+ count))]
							 [else
							  (assert #f)]))) ;; we should never reach here
						    (lambda (val)
						      (set! lst (cons val lst)))
						    (lambda (c)
						      (test-result c 'my-exception)
						      (set! lst (cons 100 lst))))))
	      (test-result (car lst) 100)
	      (test-result (length lst) 6)
	      (test-result res 'chez-a-sync-thread-error)
	      (print-result)
	      (event-loop-block! #f main-loop))))
  (event-loop-run! main-loop)
  (thread-pool-stop! pool))

;;;;;;;;;; now the same await tests with a default event loop ;;;;;;;;;;

(event-loop-block! #f main-loop)
(set-default-event-loop! main-loop)

;; Test 12: await-task-in-thread-pool! without handler

(define main-loop (make-event-loop))

(let ([pool (make-thread-pool 2)])
  (event-loop-block! #t)
  (a-sync (lambda (await resume)
	    (let ([res
		   (await-task-in-thread-pool! await resume pool
					       (lambda ()
						 (+ 5 10)))])
	      (test-result 15 res)
	      (print-result)
	      (event-loop-quit!)
	      (event-loop-block! #f))))
  (event-loop-run!)
  (thread-pool-stop! pool))
		    
;; Test 13: await-task-in-thread-pool! with handler

(let ([pool (make-thread-pool 2)])
  (event-loop-block! #t)
  (a-sync (lambda (await resume)
	    (let ([res
		   (await-task-in-thread-pool!
		    await resume pool
		    (lambda () (raise 'test-exception))
		    (lambda (c)
		      (test-result 'test-exception c)
		      5))])
	      (test-result 5 res)
	      (print-result)
	      (event-loop-quit!)
	      (event-loop-block! #f))))
  (event-loop-run!)
  (thread-pool-stop! pool))

;; Test 14: await-generator-in-thread-pool! without handler

(let ([pool (make-thread-pool 2)]
      [lst '()])
  (event-loop-block! #t)
  (a-sync (lambda (await resume)
	    (let ([res
		   (await-generator-in-thread-pool! await resume pool
						    (lambda (yield)
						      (let loop ((count 0))
							(when (< count 5)
							  (yield (* 2 count))
							  (loop (1+ count)))))
						    (lambda (val)
						      (set! lst (cons val lst))))])
	      (test-result (car lst) 8)
	      (test-result (length lst) 5)
	      (test-result res #f)
	      (print-result)
	      (event-loop-block! #f))))
  (event-loop-run!)
  (thread-pool-stop! pool))

;; Test 15: await-generator-in-thread-pool! with handler

(let ([pool (make-thread-pool 2)]
      [lst '()])
  (event-loop-block! #t)
  (a-sync (lambda (await resume)
	    (let ([res
		   (await-generator-in-thread-pool! await resume pool
						    (lambda (yield)
						      (let loop ((count 0))
							(cond
							 [(< count 5)
							  (yield (* 2 count))
							  (loop (1+ count))]
							 [(= count 5)
							  (raise 'my-exception)
							  ;; we never reach here
							  (yield (* 2 count))
							  (loop (1+ count))]
							 [else
							  (assert #f)]))) ;; we should never reach here
						    (lambda (val)
						      (set! lst (cons val lst)))
						    (lambda (c)
						      (test-result c 'my-exception)
						      (set! lst (cons 100 lst))))])
	      (test-result (car lst) 100)
	      (test-result (length lst) 6)
	      (test-result res 'chez-a-sync-thread-error)
	      (print-result)
	      (event-loop-block! #f))))
  (event-loop-run!)
  (thread-pool-stop! pool))

;; Test 16: with-thread-pool-increment

(let ([pool (make-thread-pool 1)]
      [mutex (make-mutex)]
      [condvar (make-condition)]
      [count 0])
  (thread-pool-add! pool (lambda ()
			   (with-thread-pool-increment
			    pool
			    (sleep (make-time 'time-duration 100000000 0))
			    (with-mutex mutex
			      (set! count (1+ count))
			      (condition-signal condvar)))))
  (thread-pool-add! pool (lambda ()
			   (sleep (make-time 'time-duration 100000000 0))
			   (with-mutex mutex
			     (set! count (1+ count))
			     (condition-signal condvar))))
  (test-result 2 (thread-pool-get-num-tasks pool))
  ;; allow first task to start and increment max-thread value
  (sleep (make-time 'time-duration 50000000 0))
  (test-result 2 (thread-pool-get-size pool))
  (test-result 2  (thread-pool-get-num-threads pool))
  (with-mutex mutex
    (do ()
	((= count 2))
      (condition-wait condvar mutex)))
  (test-result 2 count)
  (thread-pool-stop! pool)
  (print-result))
