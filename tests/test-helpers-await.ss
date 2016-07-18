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

;; Test 4: await-timeout!

(a-sync (lambda (await resume)
	  (let ((res
		 (await-timeout! await resume main-loop 10
				 (lambda ()
				   (+ 5 10)))))
	    (test-result 15 res)
	    (print-result))))
(event-loop-run! main-loop)

;; Test 5: await-getline! (also tests a-sync-read-watch!)

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

;; Test 6: await-geteveryline! (also tests a-sync-read-watch!)

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

;; Test 7: await-getsomelines! (also tests a-sync-read-watch!)

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

;; Test 8: await-getsomelines! exception handling (also tests strategy for await-geteveryline!)
;; exception propagates out of event-loop-run!
(let-values ([(in out) (make-pipe (buffer-mode block)
				  (buffer-mode block)
                                  (make-transcoder (latin-1-codec)))])
  (define count 0)
  (a-sync (lambda (await resume)
	    (guard
	     (c
	      [else
	       (assert (eq? c 'exit-exception))])
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
	     (assert #f)))) ;; we should never reach here
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

;; Test 9: a-sync-write-watch!

(let-values ([(in out) (make-pipe (buffer-mode block)
				  (buffer-mode block)
                                  (make-transcoder (latin-1-codec)))])
  (define count 0)
  (a-sync (lambda (await resume)
	    (a-sync-write-watch! resume out
				 (lambda (status)
				   (test-result 'out status)
				   (if (< count 3)
				       (begin
					 (set! count (1+ count))
					 (write-char #\a out)
					 (flush-output-port out)
					 'more)
				       (begin
					 (write-char #\x out)
					 (flush-output-port out)
					 (event-loop-remove-write-watch! out main-loop)
					 'done)))
				 main-loop)
	    (let loop ((res (await)))
	      (let ((ch (read-char in)))
		(if (not (char=? ch #\x))
		    (begin
		      (test-result 'more res)
		      (test-result #\a ch)
		      (loop (await)))
		    (test-result 'done res))))
	    (test-result 3 count)
	    (print-result)))
  (event-loop-run! main-loop))

;; Test 10: compose-a-sync and no-await

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

;; Test 11: await-task!

(a-sync (lambda (await resume)
	  (let ((res
		 (await-task! await resume
			      (lambda ()
				(+ 5 10)))))
	    (test-result 15 res)
	    (print-result))))
(event-loop-run!)

;; Test 12: await-task-in-thread! without handler

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
  
;; Test 13: await-task-in-thread! without handler (explicit loop argument)

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
  
;; Test 14: await-task-in-thread! with handler

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

;; Test 15: await-timeout!

(a-sync (lambda (await resume)
	  (let ((res
		 (await-timeout! await resume 10
				 (lambda ()
				   (+ 5 10)))))
	    (test-result 15 res)
	    (print-result))))
(event-loop-run!)
  
;; Test 16: await-getline! (also tests a-sync-read-watch!)

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

;; Test 17: await-geteveryline! (also tests a-sync-read-watch!)

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

;; Test 18: await-getsomelines! (also tests a-sync-read-watch!)

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

;; Test 19: await-getsomelines! exception handling (also tests strategy for await-geteveryline!)
;; exception caught within a-sync block
(let-values ([(in out) (make-pipe (buffer-mode block)
				  (buffer-mode block)
                                  (make-transcoder (latin-1-codec)))])
  (define count 0)
  (a-sync (lambda (await resume)
	    (guard
	     (c
	      [else
	       (assert (eq? c 'exit-exception))])
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
	     (assert #f))))   ;; we should never reach here
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

;; Test 20: a-sync-write-watch!

(let-values ([(in out) (make-pipe (buffer-mode block)
				  (buffer-mode block)
                                  (make-transcoder (latin-1-codec)))])
  (define count 0)
  (a-sync (lambda (await resume)
	    (a-sync-write-watch! resume out
				 (lambda (status)
				   (test-result 'out status)
				   (if (< count 3)
				       (begin
					 (set! count (1+ count))
					 (write-char #\a out)
					 (flush-output-port out)
					 'more)
				       (begin
					 (write-char #\x out)
					 (flush-output-port out)
					 (event-loop-remove-write-watch! out)
					 'done))))
	    (let loop ((res (await)))
	      (let ((ch (read-char in)))
		(if (not (char=? ch #\x))
		    (begin
		      (test-result 'more res)
		      (test-result #\a ch)
		      (loop (await)))
		    (test-result 'done res))))
	    (test-result 3 count)
	    (print-result)))
  (event-loop-run!))

;; Test 21: compose-a-sync and no-await

(compose-a-sync ((res (await-task-in-thread! (lambda ()
					       (+ 5 10)))))
	      ((no-await (test-result 15 res)
			 (print-result)
			 (event-loop-quit!))))
(event-loop-block! #t)
(event-loop-run!)
