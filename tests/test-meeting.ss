;; Copyright (C) 2017 Chris Vine

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

(import (a-sync coroutines)
	(a-sync event-loop)
	(a-sync meeting)
	(chezscheme))

;; helpers

(define print-result
  ((lambda ()
     (define count 1)
     (lambda ()
       (format #t "~a: Test ~a OK\n" "test-meeting.ss" count)
       (set! count (1+ count))))))

;; Test 1: start sending before receiving

(define main-loop (make-event-loop))

(let ()
  (define m1 (make-meeting main-loop))
  (define res '())

  (a-sync (lambda (await resume)
	    (let next ([datum (meeting-receive await resume main-loop m1)])
	      (when (not (eq? datum 'stop-iteration))
		(set! res (cons datum res))
		(next (meeting-receive await resume main-loop m1))))))

  (a-sync (lambda (await resume)
	    (let next ([count 0])
	      (if (< count 4)
		  (begin
		    (meeting-send await resume main-loop m1 count)
		    (next (+ count 1)))
		  (meeting-close m1)))))

  (event-loop-run! main-loop)
  (assert (equal? res '(3 2 1 0)))
  (print-result))

;; Test 2: start receiving before sending

(let ()
  (define m1 (make-meeting main-loop))
  (define res '())

  (a-sync (lambda (await resume)
	    (let next ([count 0])
	      (if (< count 4)
		  (begin
		    (meeting-send await resume main-loop m1 count)
		    (next (+ count 1)))
		  (meeting-close m1)))))

  (a-sync (lambda (await resume)
	    (let next ([datum (meeting-receive await resume main-loop m1)])
	      (when (not (eq? datum 'stop-iteration))
		(set! res (cons datum res))
		(next (meeting-receive await resume main-loop m1))))))

  (event-loop-run! main-loop)
  (assert (equal? res '(3 2 1 0)))
  (print-result))

;;;;;;;;;; now the same tests with a default event loop ;;;;;;;;;;

(set-default-event-loop! main-loop)

;; Test 3: start sending before receiving

(let ()
  (define m1 (make-meeting))
  (define res '())

  (a-sync (lambda (await resume)
	    (let next ([datum (meeting-receive await resume m1)])
	      (when (not (eq? datum 'stop-iteration))
		(set! res (cons datum res))
		(next (meeting-receive await resume m1))))))

  (a-sync (lambda (await resume)
	    (let next ([count 0])
	      (if (< count 4)
		  (begin
		    (meeting-send await resume m1 count)
		    (next (+ count 1)))
		  (meeting-close m1)))))

  (event-loop-run!)
  (assert (equal? res '(3 2 1 0)))
  (print-result))

;; Test 4: start receiving before sending

(let ()
  (define m1 (make-meeting))
  (define res '())

  (a-sync (lambda (await resume)
	    (let next ([count 0])
	      (if (< count 4)
		  (begin
		    (meeting-send await resume m1 count)
		    (next (+ count 1)))
		  (meeting-close m1)))))

  (a-sync (lambda (await resume)
	    (let next ([datum (meeting-receive await resume m1)])
	      (when (not (eq? datum 'stop-iteration))
		(set! res (cons datum res))
		(next (meeting-receive await resume m1))))))

  (event-loop-run!)
  (assert (equal? res '(3 2 1 0)))
  (print-result))

