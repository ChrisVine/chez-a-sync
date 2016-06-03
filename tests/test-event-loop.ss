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

(import (a-sync event-loop)
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
       (format #t "~a: Test ~a OK\n" "test-event-loop.ss" count)
       (set! count (1+ count))))))

;; Test 1: event-post!

(define main-loop (make-event-loop))
(let ()
  (define count 0)
  (event-post! (lambda ()
		 (set! count (1+ count)))
	       main-loop)
  (event-loop-run! main-loop)
  (test-result 1 count)
  (print-result))
  
;; Test 2: timeout-post! and timeout-remove!

(let ()
  (define count1 0)
  (define count2 0)
  (define tag (timeout-post! 60
			     (lambda ()
			       (if (< count1 3)
				   (set! count1 (1+ count1))
				   (timeout-remove! tag main-loop))
			       #t)
			     main-loop))
  (timeout-post! 100
		 (lambda ()
		   (if (< count2 3)
		       (begin
			 (set! count2 (1+ count2))
			 #t)
		       #f))
		 main-loop)
  (event-loop-run! main-loop)
  (test-result 3 count1)
  (test-result 3 count2)
  (print-result))

;; Test 3: event-loop-block! and event-loop-quit!

(let ()
  (define count 0)
  (fork-thread (lambda ()
		 ;; we don't need mutex here as the main thread only
		 ;; accesses count before the thread starts and after
		 ;; it ends
		 (set! count (1+ count))
		 (event-post! (lambda ()
				(event-loop-quit! main-loop))
				       main-loop)))
  (event-loop-block! #t main-loop)
  (event-loop-run! main-loop)
  (test-result 1 count)
  (event-loop-block! #f main-loop)
  (set! count (1+ count))
  ;; this should return immediately with the loop set not to block
  ;; again
  (event-loop-run! main-loop)
  (test-result 2 count)
  (print-result))

;; Test 4: event-loop-add-read-watch!

(let-values ([(in out) (make-pipe (buffer-mode block)
                                  (buffer-mode block)
                                  (make-transcoder (latin-1-codec)))])
  (define count 0)
  (event-loop-add-read-watch! in
			      (lambda (status)
				(test-result 'in status)
				(let ([ch (read-char in)])
				  (if (not (char=? ch #\x))
				      (begin
					(test-result #\a ch)
					(set! count (1+ count))
					#t)
				      #f)))
			      main-loop)
  (let loop ((count 0))
    (if (< count 3)
	(begin
	  (write-char #\a out)
	  (flush-output-port out)
	  (loop (1+ count))
	(begin
	  (write-char #\x out)
	  (flush-output-port out)))))
  (event-loop-run! main-loop)
  (test-result 3 count)
  (close-port out)
  (close-port in)
  (print-result))

;; Test 5: event-loop-add-read-watch! and event-loop-remove-read-watch!

(let-values ([(in out) (make-pipe (buffer-mode block)
                                  (buffer-mode block)
                                  (make-transcoder (latin-1-codec)))])
  (define count 0)
  (event-loop-add-read-watch! in
			      (lambda (status)
				(test-result 'in status)
				(let ([ch (read-char in)])
				  (if (not (char=? ch #\x))
				      (begin
					(test-result #\a ch)
					(set! count (1+ count)))
				      (event-loop-remove-read-watch! in main-loop))
				  #t))
			      main-loop)
  (let loop ((count 0))
    (if (< count 3)
	(begin
	  (write-char #\a out)
	  (flush-output-port out)
	  (loop (1+ count))
	(begin
	  (write-char #\x out)
	  (flush-output-port out)))))
  (event-loop-run! main-loop)
  (test-result 3 count)
  (close-port out)
  (close-port in)
  (print-result))

;; Test 6: event-loop-add-write-watch!

(let-values ([(in out) (make-pipe (buffer-mode block)
                                  (buffer-mode block)
                                  (make-transcoder (latin-1-codec)))])
  (define count 0)
  (event-loop-add-write-watch! out
			       (lambda (status)
				 (test-result 'out status)
				 (if (< count 3)
				     (begin
				       (set! count (1+ count))
				       (write-char #\a out)
				       (flush-output-port out)
				       #t)
				     (begin
				       (write-char #\x out)
				       (flush-output-port out)
				       #f)))
			       main-loop)
  (event-loop-run! main-loop)
  (let loop ((ch (read-char in))
	     (count 0))
    (if (not (char=? ch #\x))
	(begin
	  (test-result #\a ch)
	  (loop (read-char in) (1+ count)))
	(test-result 3 count)))
  (test-result 3 count)
  (close-port out)
  (close-port in)
  (print-result))

;; Test 7: event-loop-add-write-watch! and event-loop-remove-write-watch!

(let-values ([(in out) (make-pipe (buffer-mode block)
                                  (buffer-mode block)
                                  (make-transcoder (latin-1-codec)))])
  (define count 0)
  (event-loop-add-write-watch! out
			       (lambda (status)
				 (test-result 'out status)
				 (if (< count 3)
				     (begin
				       (set! count (1+ count))
				       (write-char #\a out)
				       (flush-output-port out))
				     (begin
				       (write-char #\x out)
				       (flush-output-port out)
				       (event-loop-remove-write-watch! out main-loop)))
				 #t)
			       main-loop)
  (event-loop-run! main-loop)
  (let loop ((ch (read-char in))
	     (count 0))
    (if (not (char=? ch #\x))
	(begin
	  (test-result #\a ch)
	  (loop (read-char in) (1+ count)))
	(test-result 3 count)))
  (test-result 3 count)
  (close-port out)
  (close-port in)
  (print-result))

;;;;;;;;;; now the same tests with a default event loop ;;;;;;;;;;

(set-default-event-loop! main-loop)

;; Test 8: event-post!

(let ()
  (define count 0)
  (event-post! (lambda ()
		 (set! count (1+ count))))
  (event-loop-run!)
  (test-result 1 count)
  (print-result))
  
;; Test 9: timeout-post! and timeout-remove!

;; set a new default event loop
(set-default-event-loop!)

(let ()
  (define count1 0)
  (define count2 0)
  (define tag (timeout-post! 60
			     (lambda ()
			       (if (< count1 3)
				   (set! count1 (1+ count1))
				   (timeout-remove! tag))
			       #t)))
  (timeout-post! 100
		 (lambda ()
		   (if (< count2 3)
		       (begin
			 (set! count2 (1+ count2))
			 #t)
		       #f)))
  (event-loop-run!)
  (test-result 3 count1)
  (test-result 3 count2)
  (print-result))

;; Test 10: event-loop-block! and event-loop-quit!

(let ()
  (define count 0)
  (fork-thread (lambda ()
		 ;; we don't need mutex here as the main thread only
		 ;; accesses count before the thread starts and after
		 ;; it ends
		 (set! count (1+ count))
		 (event-post! (lambda ()
				(event-loop-quit!)))))
  (event-loop-block! #t)
  (event-loop-run!)
  (test-result 1 count)
  (event-loop-block! #f)
  (set! count (1+ count))
  ;; this should return immediately with the loop set not to block
  ;; again
  (event-loop-run!)
  (test-result 2 count)
  (print-result))

;; Test 11: event-loop-add-read-watch!

(let-values ([(in out) (make-pipe (buffer-mode block)
                                  (buffer-mode block)
                                  (make-transcoder (latin-1-codec)))])
  (define count 0)
  (event-loop-add-read-watch! in
			      (lambda (status)
				(test-result 'in status)
				(let ([ch (read-char in)])
				  (if (not (char=? ch #\x))
				      (begin
					(test-result #\a ch)
					(set! count (1+ count))
					#t)
				      #f))))
  (let loop ((count 0))
    (if (< count 3)
	(begin
	  (write-char #\a out)
	  (flush-output-port out)
	  (loop (1+ count))
	(begin
	  (write-char #\x out)
	  (flush-output-port out)))))
  (event-loop-run!)
  (test-result 3 count)
  (close-port out)
  (close-port in)
  (print-result))

;; Test 12: event-loop-add-read-watch! and event-loop-remove-read-watch!

(let-values ([(in out) (make-pipe (buffer-mode block)
                                  (buffer-mode block)
                                  (make-transcoder (latin-1-codec)))])
  (define count 0)
  (event-loop-add-read-watch! in
			      (lambda (status)
				(test-result 'in status)
				(let ([ch (read-char in)])
				  (if (not (char=? ch #\x))
				      (begin
					(test-result #\a ch)
					(set! count (1+ count)))
				      (event-loop-remove-read-watch! in))
				  #t)))
  (let loop ((count 0))
    (if (< count 3)
	(begin
	  (write-char #\a out)
	  (flush-output-port out)
	  (loop (1+ count))
	(begin
	  (write-char #\x out)
	  (flush-output-port out)))))
  (event-loop-run!)
  (test-result 3 count)
  (close-port out)
  (close-port in)
  (print-result))

;; Test 13: event-loop-add-write-watch!

(let-values ([(in out) (make-pipe (buffer-mode block)
                                  (buffer-mode block)
                                  (make-transcoder (latin-1-codec)))])
  (define count 0)
  (event-loop-add-write-watch! out
			       (lambda (status)
				 (test-result 'out status)
				 (if (< count 3)
				     (begin
				       (set! count (1+ count))
				       (write-char #\a out)
				       (flush-output-port out)
				       #t)
				     (begin
				       (write-char #\x out)
				       (flush-output-port out)
				       #f))))
  (event-loop-run!)
  (let loop ((ch (read-char in))
	     (count 0))
    (if (not (char=? ch #\x))
	(begin
	  (test-result #\a ch)
	  (loop (read-char in) (1+ count)))
	(test-result 3 count)))
  (test-result 3 count)
  (close-port out)
  (close-port in)
  (print-result))

;; Test 14: event-loop-add-write-watch! and event-loop-remove-write-watch!

(let-values ([(in out) (make-pipe (buffer-mode block)
                                  (buffer-mode block)
                                  (make-transcoder (latin-1-codec)))])
  (define count 0)
  (event-loop-add-write-watch! out
			       (lambda (status)
				 (test-result 'out status)
				 (if (< count 3)
				     (begin
				       (set! count (1+ count))
				       (write-char #\a out)
				       (flush-output-port out))
				     (begin
				       (write-char #\x out)
				       (flush-output-port out)
				       (event-loop-remove-write-watch! out)))
				 #t))
  (event-loop-run!)
  (let loop ((ch (read-char in))
	     (count 0))
    (if (not (char=? ch #\x))
	(begin
	  (test-result #\a ch)
	  (loop (read-char in) (1+ count)))
	(test-result 3 count)))
  (test-result 3 count)
  (close-port out)
  (close-port in)
  (print-result))
