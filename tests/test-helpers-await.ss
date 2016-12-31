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
	(a-sync compose)
	(a-sync try)
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
       (format #t "~a: Test ~a OK\n" "test-helpers-await.ss" count)
       (set! count (1+ count))))))

;; bv1 must be smaller than or equal to bv2 to obtain a #t result
(define (test-bytevector bv1 bv2)
  (if (> (bytevector-length bv1) (bytevector-length bv2))
      #f
      (let ([sz (bytevector-length bv1)])
	(call/cc (lambda (k)
		   (let loop ([index 0])
		     (cond
		      [(= index sz)
		       #t]
		      [(= (bytevector-s8-ref bv1 index)
			  (bytevector-s8-ref bv2 index))
		       (loop (+ index 1))]
		      [(k #f)])))))))

;; Test 1: await-task!

(define main-loop (make-event-loop))

(a-sync (lambda (await resume)
	  (let ((res
		 (await-task! await resume main-loop
			      (lambda ()
				(+ 5 10)))))
	    (test-result 15 res)
	    (print-result))))
(event-loop-run! main-loop)

;; Test 2: await-task-in-thread! without handler

(a-sync (lambda (await resume)
	  (let ((res
	  	 (await-task-in-thread! await resume main-loop
	  				(lambda ()
	  				  (+ 5 10)))))
	    (test-result 15 res)
	    (print-result)
	    (event-loop-quit! main-loop))))
(event-loop-block! #t main-loop)
(event-loop-run! main-loop)
(event-loop-block! #f main-loop)
  
;; Test 3: await-task-in-thread! with handler

(a-sync (lambda (await resume)
	  (let ((res
	  	 (await-task-in-thread!
		  await resume main-loop
		  (lambda () (raise 'test-exception))
		  (lambda (c)
		    (test-result 'test-exception c)
		    5))))
	    (test-result 5 res)
	    (print-result)
	    (event-loop-quit! main-loop))))
(event-loop-block! #t main-loop)
(event-loop-run! main-loop)
(event-loop-block! #f main-loop)

;; Test 4: await-task-in-event-loop!

(let ()
  (define worker (make-event-loop 10 100000))

  (event-loop-block! #t main-loop)
  (event-loop-block! #t worker)

  (fork-thread
   (lambda ()
     (event-loop-run! worker)))

  (a-sync (lambda (await resume)
	    (let ([res
		   (await-task-in-event-loop! await resume main-loop worker
					      (lambda ()
						(+ 5 10)))])
	      (test-result 15 res)
	      (print-result)
	      (event-loop-block! #f main-loop)
	      (event-loop-block! #f worker))))
  (event-loop-run! main-loop))

;; Test 5: await-yield!
(let ()
  (define val 0)
  (a-sync (lambda (await resume)
	    (await-yield! await resume main-loop)
	    (test-result 1 val)
	    (print-result)))
  (set! val 1)
  (event-loop-run! main-loop))

;; Test 6: await-generator!

(let ()
  (define lst '())
  (a-sync (lambda (await resume)
	    (await-generator! await resume main-loop
			      (lambda (yield)
				(let loop ([count 0])
				  (when (< count 5)
				    (yield (* 2 count))
				    (loop (+ count 1)))))
			      (lambda (val)
				(set! lst (cons val lst))))
	    (test-result (car lst) 8)
	    (test-result (length lst) 5)
	    (print-result)))
  (event-loop-run! main-loop))

;; Test 7: await-generator-in-thread! without handler

(let ()
  (define lst '())
  (a-sync (lambda (await resume)
	    (let ([res
		   (await-generator-in-thread! await resume main-loop
					       (lambda (yield)
						 (let loop ([count 0])
						   (when (< count 5)
						     (yield (* 2 count))
						     (loop (+ count 1)))))
					       (lambda (val)
						 (set! lst (cons val lst))))])
	      (test-result (car lst) 8)
	      (test-result (length lst) 5)
	      (test-result res #f)
	      (print-result)
	      (event-loop-block! #f main-loop))))
  (event-loop-block! #t main-loop)
  (event-loop-run! main-loop))

;; Test 8: await-generator-in-thread! with handler

(let ()
  (define lst '())
  (a-sync (lambda (await resume)
	    (let ([res
		   (await-generator-in-thread! await resume main-loop
					       (lambda (yield)
						 (let loop ([count 0])
						   (cond
						    [(< count 5)
						     (yield (* 2 count))
						     (loop (+ count 1))]
						    [(= count 5)
						     (raise 'my-exception)
						     ;; we never reach here
						     (yield (* 2 count))
						     (loop (+ count 1))]
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
	      (event-loop-block! #f main-loop))))
  (event-loop-block! #t main-loop)
  (event-loop-run! main-loop))

;; Test 9: await-generator-in-event-loop!

(let ()
  (define lst '())
  (define worker (make-event-loop 10 100000))

  (event-loop-block! #t main-loop)
  (event-loop-block! #t worker)

  (fork-thread
   (lambda ()
     (event-loop-run! worker)))

  (a-sync (lambda (await resume)
	    (await-generator-in-event-loop! await resume main-loop worker
					    (lambda (yield)
					      (let loop ([count 0])
						(when (< count 5)
						  (yield (* 2 count))
						  (loop (+ count 1)))))
					    (lambda (val)
					      (set! lst (cons val lst))))
	    (test-result (car lst) 8)
	    (test-result (length lst) 5)
	    (print-result)
	    (event-loop-block! #f main-loop)))
  (event-loop-block! #t main-loop)
  (event-loop-run! main-loop))

;; Test 10: await-timeout!

(a-sync (lambda (await resume)
	  (let ((res
		 (await-timeout! await resume main-loop 10
				 (lambda ()
				   (+ 5 10)))))
	    (test-result 15 res)
	    (print-result))))
(event-loop-run! main-loop)

;; Test 11: await-sleep!
(let ()
  (define val 0)
  (a-sync (lambda (await resume)
	    (await-sleep! await resume main-loop 100)
	    (test-result 1 val)
	    (print-result)))
  (set! val 1)
  (event-loop-run! main-loop))

;; Test 12: await-getline! (also tests a-sync-read-watch!)

(let-values ([(in out) (make-pipe (buffer-mode block)
				  (buffer-mode block)
                                  (make-transcoder (latin-1-codec)))])
  (a-sync (lambda (await resume)
	    (let ((res (await-getline! await resume
				       main-loop
				       in)))
	      (assert (string=? res "test-string"))
	      (print-result))))
  (put-string out "test-string")
  (newline out)
  (flush-output-port out)
  (event-loop-run! main-loop)
  (close-port out)
  (close-port in))

;; Test 13: await-geteveryline! (also tests a-sync-read-watch!)

(let-values ([(in out) (make-pipe (buffer-mode block)
				  (buffer-mode block)
                                  (make-transcoder (latin-1-codec)))])
  (define count 0)
  (a-sync (lambda (await resume)
	    (let ((res (await-geteveryline! await resume
					    main-loop
					    in
					    (lambda (line)
					      (set! count (1+ count))
					      (when (= count 1)
						(assert (string=? line "test-string1")))
					      (when (= count 2)
						(assert (string=? line "test-string2"))
						(close-port out))))))
	      (assert (eof-object? res))
	      (test-result 2 count)
	      (print-result))))
  (put-string out "test-string1")
  (newline out)
  (put-string out "test-string2")
  (newline out)
  (flush-output-port out)
  (event-loop-run! main-loop)
  (close-port in))

;; Test 14: await-getsomelines! (also tests a-sync-read-watch!)

(let-values ([(in out) (make-pipe (buffer-mode block)
				  (buffer-mode block)
                                  (make-transcoder (latin-1-codec)))])
  (define count 0)
  (a-sync (lambda (await resume)
	    (let ((res (await-getsomelines! await resume
					    main-loop
					    in
					    (lambda (line k)
					      (set! count (1+ count))
					      (when (= count 1)
						    (assert (string=? line "test-string1")))
					      (when (= count 2)
						    (assert (string=? line "test-string2")))
					      (when (= count 3)
						    (assert (string=? line "test-string3"))
						    (k 'exit-await))))))
	      (assert (eq? res 'exit-await))
	      (test-result 3 count)
	      (print-result))))
  (put-string out "test-string1")
  (newline out)
  (put-string out "test-string2")
  (newline out)
  (put-string out "test-string3")
  (newline out)
  (flush-output-port out)
  (event-loop-run! main-loop)
  (close-port out)
  (close-port in))

;; Test 15: await-getsomelines! exception handling (also tests strategy for await-geteveryline!)
;; exception propagates out of event-loop-run!
(let-values ([(in out) (make-pipe (buffer-mode block)
				  (buffer-mode block)
                                  (make-transcoder (latin-1-codec)))])
  (define count 0)
  (a-sync (lambda (await resume)
	    (await-getsomelines! await resume
				 main-loop
				 in
				 (lambda (line k)
				   (set! count (1+ count))
				   (when (= count 1)
					 (assert (string=? line "test-string1")))
				   (when (= count 2)
					 (raise 'exit-exception))
				   (when (= count 3)
					 (assert #f)))) ;; we should never reach here
	    (assert #f))) ;; we should never reach here
  (put-string out "test-string1")
  (newline out)
  (put-string out "test-string2")
  (newline out)
  (put-string out "test-string3")
  (newline out)
  (flush-output-port out)
  (try
   (event-loop-run! main-loop)
   (assert #f) ;; we should never reach here
   (except c 
	   [else
	    (assert (eq? c 'exit-exception))]))
  (close-port out)
  (close-port in)
  (test-result 2 count)
  (print-result))

;; Test 16: await-getsomelines! exception handling (also tests 'try' and strategy for await-geteveryline!)
;; exception caught within a-sync block
(let-values ([(in out) (make-pipe (buffer-mode block)
				  (buffer-mode block)
                                  (make-transcoder (latin-1-codec)))])
  (define count 0)
  (a-sync (lambda (await resume)
	    (try
	     (await-getsomelines! await resume
				  main-loop
				  in
				  (lambda (line k)
				    (set! count (1+ count))
				    (when (= count 1)
					  (assert (string=? line "test-string1")))
				    (when (= count 2)
					  (raise 'exit-exception))
				    (when (= count 3)
					  (assert #f)))) ;; we should never reach here
	     (assert #f) ;; we should never reach here
	     (except c
		     [else
		      (assert (eq? c 'exit-exception))]))))
  (put-string out "test-string1")
  (newline out)
  (put-string out "test-string2")
  (newline out)
  (put-string out "test-string3")
  (newline out)
  (flush-output-port out)
  (event-loop-run! main-loop)
  (close-port out)
  (close-port in)
  (test-result 2 count)
  (print-result))

;; Test 17: await-getblock! (also tests a-sync-read-watch!)

(let-values ([(in out) (make-pipe (buffer-mode block)
				  (buffer-mode block))])
  (define bv1 (make-bytevector 10 20))
  (define bv2 (make-bytevector 10 21))
  (a-sync (lambda (await resume)
	    (let ([res (await-getblock! await resume
					main-loop
					in
					10)])
	      (test-result 10 (cdr res))
	      (assert (test-bytevector bv1 (car res))))
	    (let ([res (await-getblock! await resume
					main-loop
					in
					20)])
	      (test-result 10 (cdr res))
	      (test-result 20 (bytevector-length (car res)))
	      (assert (test-bytevector bv2 (car res))))
	    (let ([res (await-getblock! await resume
					main-loop
					in
					10)])
	      (assert (eof-object? (car res)))
	      (test-result #f (cdr res)))
	    (print-result)))
  (a-sync (lambda (await resume)
	    (await-timeout! await resume main-loop 50 (lambda () #f))
	    (put-bytevector out bv1)
	    (flush-output-port out)
	    (await-timeout! await resume main-loop 50 (lambda () #f))
	    (put-bytevector out bv2)
	    (flush-output-port out)
            (close-port out)))
  (event-loop-run! main-loop)
  (close-port in))

;; Test 18: await-geteveryblock! (also tests a-sync-read-watch!)

(let-values ([(in out) (make-pipe (buffer-mode block)
				  (buffer-mode block))])
  (define bv1 (make-bytevector 10 20))
  (define bv2 (make-bytevector 10 21))
  (define bv3 (make-bytevector 10 22))
  (define count 0)
  (a-sync (lambda (await resume)
	    (let ([res (await-geteveryblock! await resume
					     main-loop
					     in
					     20
					     (lambda (bv-in len)
					       (set! count (1+ count))
					       (when (= count 1)
						     (test-result 20 len)
						     (assert (test-bytevector bv1 bv-in)))
					       (when (= count 2)
						     (test-result 10 len)
						     (assert (test-bytevector bv3 bv-in)))))])
	      (assert (eof-object? res)))
	    (let ([res (await-geteveryblock! await resume
					     main-loop
					     in
					     10
					     (lambda (bv-in len)
					       (assert #f)))]) ;; we should never reach here
	      (assert (eof-object? res)))
	    (print-result)))
  (a-sync (lambda (await resume)
	    (await-timeout! await resume main-loop 50 (lambda () #f))
	    (put-bytevector out bv1)
	    (flush-output-port out)
	    (await-timeout! await resume main-loop 50 (lambda () #f))
	    (put-bytevector out bv2)
	    (flush-output-port out)
	    (await-timeout! await resume main-loop 50 (lambda () #f))
	    (put-bytevector out bv3)
	    (flush-output-port out)
            (close-port out)))
  (event-loop-run! main-loop)
  (test-result 2 count)
  (close-port in))

;; Test 19: await-getsomeblocks! (also tests a-sync-read-watch!)

(let-values ([(in out) (make-pipe (buffer-mode block)
				  (buffer-mode block))])
  (define bv1 (make-bytevector 10 20))
  (define bv2 (make-bytevector 10 21))
  (define bv3 (make-bytevector 10 22))
  (define count 0)
  (a-sync (lambda (await resume)
	    (let ([res (await-getsomeblocks! await resume
					     main-loop
					     in
					     10
					     (lambda (bv-in len k)
					       (set! count (1+ count))
					       (when (= count 1)
						     (test-result 10 len)
						     (assert (test-bytevector bv1 bv-in)))
					       (when (= count 2)
						     (test-result 10 len)
						     (assert (test-bytevector bv2 bv-in))
						     (k 'test))))])
	      (test-result 'test res))
	    (let ([res (await-getsomeblocks! await resume
	    				     main-loop
	    				     in
					     20
	    				     (lambda (bv-in len k)
					       (test-result 10 len)
					       (test-result (bytevector-s8-ref bv-in 0) 22)
					       (assert (test-bytevector bv3 bv-in))))])
	      (assert (eof-object? res)))
	    (let ([res (await-getsomeblocks! await resume
	    				     main-loop
	    				     in
					     10
	    				     (lambda (bv-in len k)
	    				       (assert #f)))]) ;; we should never reach here
	      (assert (eof-object? res)))
	    (print-result)))
  (a-sync (lambda (await resume)
	    (await-timeout! await resume main-loop 50 (lambda () #f))
	    (put-bytevector out bv1)
	    (flush-output-port out)
	    (await-timeout! await resume main-loop 50 (lambda () #f))
	    (put-bytevector out bv2)
	    (flush-output-port out)
	    (await-timeout! await resume main-loop 50 (lambda () #f))
	    (put-bytevector out bv3)
	    (flush-output-port out)
            (close-port out)))
  (event-loop-run! main-loop)
  (test-result 2 count)
  (close-port in))

;; Test 20: await-put-string! (also tests a-sync-write-watch! and await-put-bytevector!)

(let-values ([(in out) (make-pipe (buffer-mode block)
				  (buffer-mode none)
                                  (make-transcoder (latin-1-codec)))])
  (define res #f)
  (set-port-nonblocking! out #t)
  (a-sync (lambda (await resume)
	    (a-sync (lambda (await resume)
		      (set! res (await-task-in-thread! await resume main-loop
						       (lambda ()
							 (get-string-all in))))
		      (event-loop-block! #f main-loop)))
	    (await-put-string! await resume main-loop out (string #\a #\b #\c))
	    (close-port out)))
  (event-loop-block! #t main-loop)
  (event-loop-run! main-loop)
  (test-result (string-length res) 3)
  (test-result (string-ref res 0) #\a)
  (test-result (string-ref res 1) #\b)
  (test-result (string-ref res 2) #\c)
  (close-port in)
  (print-result))

;; Test 21: compose-a-sync and no-await

(compose-a-sync main-loop ((res (await-task-in-thread! (lambda ()
							 (+ 5 10)))))
	      ((no-await (test-result 15 res)
			 (print-result)
			 (event-loop-quit! main-loop))))
(event-loop-block! #t main-loop)
(event-loop-run! main-loop)

;;;;;;;;;; now the same tests with a default event loop ;;;;;;;;;;

(event-loop-block! #f main-loop)
(set-default-event-loop! main-loop)

;; Test 22: await-task!

(a-sync (lambda (await resume)
	  (let ((res
		 (await-task! await resume
			      (lambda ()
				(+ 5 10)))))
	    (test-result 15 res)
	    (print-result))))
(event-loop-run!)

;; Test 23: await-task-in-thread! without handler

;; set a new default event loop
(set-default-event-loop!)

(a-sync (lambda (await resume)
	  (let ((res
	  	 (await-task-in-thread! await resume
	  				(lambda ()
	  				  (+ 5 10)))))
	    (test-result 15 res)
	    (print-result)
	    (event-loop-quit!))))
(event-loop-block! #t)
(event-loop-run!)
(event-loop-block! #f)
  
;; Test 24: await-task-in-thread! without handler (explicit loop argument)

(a-sync (lambda (await resume)
	  (let ((res
	  	 (await-task-in-thread! await resume #f
	  				(lambda ()
	  				  (+ 5 10)))))
	    (test-result 15 res)
	    (print-result)
	    (event-loop-quit!))))
(event-loop-block! #t)
(event-loop-run!)
(event-loop-block! #f)
  
;; Test 25: await-task-in-thread! with handler

(a-sync (lambda (await resume)
	  (let ((res
	  	 (await-task-in-thread!
		  await resume
		  (lambda () (raise 'test-exception))
		  (lambda (c)
		    (test-result 'test-exception c)
		    5))))
	    (test-result 5 res)
	    (print-result)
	    (event-loop-quit!))))
(event-loop-block! #t)
(event-loop-run!)
(event-loop-block! #f)

;; Test 26: await-task-in-event-loop!

(let ()
  (define worker (make-event-loop 10 100000))

  (event-loop-block! #t)
  (event-loop-block! #t worker)

  (fork-thread
   (lambda ()
     (event-loop-run! worker)))

  (a-sync (lambda (await resume)
	    (let ([res
		   (await-task-in-event-loop! await resume worker
					      (lambda ()
						(+ 5 10)))])
	      (test-result 15 res)
	      (print-result)
	      (event-loop-block! #f)
	      (event-loop-block! #f worker))))
  (event-loop-run!))

;; Test 27: await-yield!
(let ()
  (define val 0)
  (a-sync (lambda (await resume)
	    (await-yield! await resume)
	    (test-result 1 val)
	    (print-result)))
  (set! val 1)
  (event-loop-run!))

;; Test 28: await-generator!

(let ()
  (define lst '())
  (a-sync (lambda (await resume)
	    (await-generator! await resume
			      (lambda (yield)
				(let loop ([count 0])
				  (when (< count 5)
				    (yield (* 2 count))
				    (loop (+ count 1)))))
			      (lambda (val)
				(set! lst (cons val lst))))
	    (test-result (car lst) 8)
	    (test-result (length lst) 5)
	    (print-result)))
  (event-loop-run!))

;; Test 29: await-generator-in-thread! without handler

(let ()
  (define lst '())
  (a-sync (lambda (await resume)
	    (let ([res
		   (await-generator-in-thread! await resume
					       (lambda (yield)
						 (let loop ([count 0])
						   (when (< count 5)
						     (yield (* 2 count))
						     (loop (+ count 1)))))
					       (lambda (val)
						 (set! lst (cons val lst))))])
	      (test-result (car lst) 8)
	      (test-result (length lst) 5)
	      (test-result res #f)
	      (print-result)
	      (event-loop-block! #f))))
  (event-loop-block! #t)
  (event-loop-run!))

;; Test 30: await-generator-in-thread! without handler (explicit loop argument)

(let ()
  (define lst '())
  (a-sync (lambda (await resume)
	    (let ([res
		   (await-generator-in-thread! await resume #f
					       (lambda (yield)
						 (let loop ([count 0])
						   (when (< count 5)
						     (yield (* 2 count))
						     (loop (+ count 1)))))
					       (lambda (val)
						 (set! lst (cons val lst))))])
	      (test-result (car lst) 8)
	      (test-result (length lst) 5)
	      (test-result res #f)
	      (print-result)
	      (event-loop-block! #f))))
  (event-loop-block! #t)
  (event-loop-run!))

;; Test 31: await-generator-in-thread! with handler

(let ()
  (define lst '())
  (a-sync (lambda (await resume)
	    (let ([res
		   (await-generator-in-thread! await resume
					       (lambda (yield)
						 (let loop ([count 0])
						   (cond
						    [(< count 5)
						     (yield (* 2 count))
						     (loop (+ count 1))]
						    [(= count 5)
						     (raise 'my-exception)
						     ;; we never reach here
						     (yield (* 2 count))
						     (loop (+ count 1))]
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
  (event-loop-block! #t)
  (event-loop-run!))

;; Test 32: await-generator-in-event-loop!

(let ()
  (define lst '())
  (define worker (make-event-loop 10 100000))

  (event-loop-block! #t)
  (event-loop-block! #t worker)

  (fork-thread
   (lambda ()
     (event-loop-run! worker)))

  (a-sync (lambda (await resume)
	    (await-generator-in-event-loop! await resume worker
					    (lambda (yield)
					      (let loop ([count 0])
						(when (< count 5)
						  (yield (* 2 count))
						  (loop (+ count 1)))))
					    (lambda (val)
					      (set! lst (cons val lst))))
	    (test-result (car lst) 8)
	    (test-result (length lst) 5)
	    (print-result)
	    (event-loop-block! #f)))
  (event-loop-block! #t)
  (event-loop-run!))

;; Test 33: await-timeout!

(a-sync (lambda (await resume)
	  (let ((res
		 (await-timeout! await resume 10
				 (lambda ()
				   (+ 5 10)))))
	    (test-result 15 res)
	    (print-result))))
(event-loop-run!)
  
;; Test 34: await-sleep!
(let ()
  (define val 0)
  (a-sync (lambda (await resume)
	    (await-sleep! await resume main-loop 100)
	    (test-result 1 val)
	    (print-result)))
  (set! val 1)
  (event-loop-run! main-loop))

;; Test 35: await-getline! (also tests a-sync-read-watch!)

(let-values ([(in out) (make-pipe (buffer-mode block)
				  (buffer-mode block)
                                  (make-transcoder (latin-1-codec)))])
  (a-sync (lambda (await resume)
	    (let ((res (await-getline! await resume
				       in)))
	      (assert (string=? res "test-string"))
	      (print-result))))
  (put-string out "test-string")
  (newline out)
  (flush-output-port out)
  (event-loop-run!)
  (close-port out)
  (close-port in))

;; Test 36: await-geteveryline! (also tests a-sync-read-watch!)

(let-values ([(in out) (make-pipe (buffer-mode block)
				  (buffer-mode block)
                                  (make-transcoder (latin-1-codec)))])
  (define count 0)
  (a-sync (lambda (await resume)
	    (let ((res (await-geteveryline! await resume
					    in
					    (lambda (line)
					      (set! count (1+ count))
					      (when (= count 1)
						(assert (string=? line "test-string1")))
					      (when (= count 2)
					      	(assert (string=? line "test-string2"))
						(close-port out))))))
	      (assert (eof-object? res))
	      (test-result 2 count)
	      (print-result))))
  (put-string out "test-string1")
  (newline out)
  (put-string out "test-string2")
  (newline out)
  (flush-output-port out)
  (event-loop-run!)
  (close-port in))

;; Test 37: await-getsomelines! (also tests a-sync-read-watch!)

(let-values ([(in out) (make-pipe (buffer-mode block)
				  (buffer-mode block)
                                  (make-transcoder (latin-1-codec)))])
  (define count 0)
  (a-sync (lambda (await resume)
	    (let ((res (await-getsomelines! await resume
					    in
					    (lambda (line k)
					      (set! count (1+ count))
					      (when (= count 1)
						    (assert (string=? line "test-string1")))
					      (when (= count 2)
						    (assert (string=? line "test-string2")))
					      (when (= count 3)
						    (assert (string=? line "test-string3"))
						    (k 'exit-await))))))
	      (assert (eq? res 'exit-await))
	      (test-result 3 count)
	      (print-result))))
  (put-string out "test-string1")
  (newline out)
  (put-string out "test-string2")
  (newline out)
  (put-string out "test-string3")
  (newline out)
  (flush-output-port out)
  (event-loop-run!)
  (close-port out)
  (close-port in))

;; Test 38: await-getsomelines! exception handling (also tests strategy for await-geteveryline!)
;; exception propagates out of event-loop-run!
(let-values ([(in out) (make-pipe (buffer-mode block)
				  (buffer-mode block)
                                  (make-transcoder (latin-1-codec)))])
  (define count 0)
  (a-sync (lambda (await resume)
	    (await-getsomelines! await resume
				 in
				 (lambda (line k)
				   (set! count (1+ count))
				   (when (= count 1)
					 (assert (string=? line "test-string1")))
				   (when (= count 2)
					 (raise 'exit-exception))
				   (when (= count 3)
					 (assert #f)))) ;; we should never reach here
	    (assert #f))) ;; we should never reach here
  (put-string out "test-string1")
  (newline out)
  (put-string out "test-string2")
  (newline out)
  (put-string out "test-string3")
  (newline out)
  (flush-output-port out)
  (try
   (event-loop-run!)
   (assert #f) ;; we should never reach here
   (except c 
	   [else
	    (assert (eq? c 'exit-exception))]))
  (close-port out)
  (close-port in)
  (test-result 2 count)
  (print-result))

;; Test 39: await-getsomelines! exception handling (also tests 'try' and strategy for await-geteveryline!)
;; exception caught within a-sync block
(let-values ([(in out) (make-pipe (buffer-mode block)
				  (buffer-mode block)
                                  (make-transcoder (latin-1-codec)))])
  (define count 0)
  (a-sync (lambda (await resume)
	    (try
	     (await-getsomelines! await resume
				  in
				  (lambda (line k)
				    (set! count (1+ count))
				    (when (= count 1)
					  (assert (string=? line "test-string1")))
				    (when (= count 2)
					  (raise 'exit-exception))
				    (when (= count 3)
					  (assert #f))))   ;; we should never reach here
	     (assert #f)   ;; we should never reach here
	     (except c
		     [else
		      (assert (eq? c 'exit-exception))]))))
  (put-string out "test-string1")
  (newline out)
  (put-string out "test-string2")
  (newline out)
  (put-string out "test-string3")
  (newline out)
  (flush-output-port out)
  (event-loop-run!)
  (close-port out)
  (close-port in)
  (test-result 2 count)
  (print-result))

;; Test 40: await-getblock! (also tests a-sync-read-watch!)

(let-values ([(in out) (make-pipe (buffer-mode block)
				  (buffer-mode block))])
  (define bv1 (make-bytevector 10 20))
  (define bv2 (make-bytevector 10 21))
  (a-sync (lambda (await resume)
	    (let ([res (await-getblock! await resume
					in
					10)])
	      (test-result 10 (cdr res))
	      (assert (test-bytevector bv1 (car res))))
	    (let ([res (await-getblock! await resume
					in
					20)])
	      (test-result 10 (cdr res))
	      (test-result 20 (bytevector-length (car res)))
	      (assert (test-bytevector bv2 (car res))))
	    (let ([res (await-getblock! await resume
					in
					10)])
	      (assert (eof-object? (car res)))
	      (test-result #f (cdr res)))
	    (print-result)))
  (a-sync (lambda (await resume)
	    (await-timeout! await resume 50 (lambda () #f))
	    (put-bytevector out bv1)
	    (flush-output-port out)
	    (await-timeout! await resume 50 (lambda () #f))
	    (put-bytevector out bv2)
	    (flush-output-port out)
            (close-port out)))
  (event-loop-run!)
  (close-port in))

;; Test 41: await-geteveryblock! (also tests a-sync-read-watch!)

(let-values ([(in out) (make-pipe (buffer-mode block)
				  (buffer-mode block))])
  (define bv1 (make-bytevector 10 20))
  (define bv2 (make-bytevector 10 21))
  (define bv3 (make-bytevector 10 22))
  (define count 0)
  (a-sync (lambda (await resume)
	    (let ([res (await-geteveryblock! await resume
					     in
					     20
					     (lambda (bv-in len)
					       (set! count (1+ count))
					       (when (= count 1)
						     (test-result 20 len)
						     (assert (test-bytevector bv1 bv-in)))
					       (when (= count 2)
						     (test-result 10 len)
						     (assert (test-bytevector bv3 bv-in)))))])
	      (assert (eof-object? res)))
	    (let ([res (await-geteveryblock! await resume
					     in
					     10
					     (lambda (bv-in len)
					       (assert #f)))]) ;; we should never reach here
	      (assert (eof-object? res)))
	    (print-result)))
  (a-sync (lambda (await resume)
	    (await-timeout! await resume 50 (lambda () #f))
	    (put-bytevector out bv1)
	    (flush-output-port out)
	    (await-timeout! await resume 50 (lambda () #f))
	    (put-bytevector out bv2)
	    (flush-output-port out)
	    (await-timeout! await resume 50 (lambda () #f))
	    (put-bytevector out bv3)
	    (flush-output-port out)
            (close-port out)))
  (event-loop-run!)
  (test-result 2 count)
  (close-port in))

;; Test 42: await-getsomeblocks! (also tests a-sync-read-watch!)

(let-values ([(in out) (make-pipe (buffer-mode block)
				  (buffer-mode block))])
  (define bv1 (make-bytevector 10 20))
  (define bv2 (make-bytevector 10 21))
  (define bv3 (make-bytevector 10 22))
  (define count 0)
  (a-sync (lambda (await resume)
	    (let ([res (await-getsomeblocks! await resume
					     in
					     10
					     (lambda (bv-in len k)
					       (set! count (1+ count))
					       (when (= count 1)
						     (test-result 10 len)
						     (assert (test-bytevector bv1 bv-in)))
					       (when (= count 2)
						     (test-result 10 len)
						     (assert (test-bytevector bv2 bv-in))
						     (k 'test))))])
	      (test-result 'test res))
	    (let ([res (await-getsomeblocks! await resume
	    				     in
					     20
	    				     (lambda (bv-in len k)
					       (test-result 10 len)
					       (test-result (bytevector-s8-ref bv-in 0) 22)
					       (assert (test-bytevector bv3 bv-in))))])
	      (assert (eof-object? res)))
	    (let ([res (await-getsomeblocks! await resume
	    				     in
					     10
	    				     (lambda (bv-in len k)
	    				       (assert #f)))]) ;; we should never reach here
	      (assert (eof-object? res)))
	    (print-result)))
  (a-sync (lambda (await resume)
	    (await-timeout! await resume 50 (lambda () #f))
	    (put-bytevector out bv1)
	    (flush-output-port out)
	    (await-timeout! await resume 50 (lambda () #f))
	    (put-bytevector out bv2)
	    (flush-output-port out)
	    (await-timeout! await resume 50 (lambda () #f))
	    (put-bytevector out bv3)
	    (flush-output-port out)
            (close-port out)))
  (event-loop-run!)
  (test-result 2 count)
  (close-port in))

;; Test 43: await-put-string! (also tests a-sync-write-watch! and await-put-bytevector!)

(let-values ([(in out) (make-pipe (buffer-mode block)
				  (buffer-mode none)
                                  (make-transcoder (latin-1-codec)))])
  (define res #f)
  (set-port-nonblocking! out #t)
  (a-sync (lambda (await resume)
	    (a-sync (lambda (await resume)
		      (set! res (await-task-in-thread! await resume
						       (lambda ()
							 (get-string-all in))))
		      (event-loop-block! #f)))
	    (await-put-string! await resume out (string #\a #\b #\c))
	    (close-port out)))
  (event-loop-block! #t)
  (event-loop-run!)
  (test-result (string-length res) 3)
  (test-result (string-ref res 0) #\a)
  (test-result (string-ref res 1) #\b)
  (test-result (string-ref res 2) #\c)
  (close-port in)
  (print-result))

;; Test 44: compose-a-sync and no-await

(compose-a-sync ((res (await-task-in-thread! (lambda ()
					       (+ 5 10)))))
	      ((no-await (test-result 15 res)
			 (print-result)
			 (event-loop-quit!))))
(event-loop-block! #t)
(event-loop-run!)
