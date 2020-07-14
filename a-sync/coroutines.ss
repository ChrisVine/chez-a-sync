;; Copyright (C) 2016 and 2019 Chris Vine
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

(library (a-sync coroutines)
  (export 
   make-iterator
   make-coroutine
   a-sync)
  (import (chezscheme))


;; this procedure takes a generator procedure, namely a procedure
;; which has a 'yield' parameter for its first or only argument,
;; followed by such other arguments (other than the one for the
;; 'yield' parameter) as the generator procedure requires, and
;; constructs an iterator from them.  When the iterator (which
;; optionally takes no or one argument) is invoked, it will begin
;; executing the procedure unless and until the argument comprising
;; the yield procedure is called, which will cause the iterator to
;; suspend computation and instead return the value passed to yield,
;; if any (yield is a procedure taking no or one argument).  If
;; invoked again, the iterator will resume computation at the point
;; where it last left off (the yield procedure returning the value, if
;; any, passed to the iterator on resuming), following which yield may
;; be applied again, and so on.  When the generator procedure has
;; executed to the end, the iterator returns 'stop-iteration.  This
;; procedure has some resemblance to call/ec, except that (i) instead
;; of executing the passed procedure immediately, it returns an
;; iterator which will do so, (ii) it is resumable, and (iii) the
;; procedure to be executed can receive starting arguments in addition
;; to the yield/break argument, to provide an alternative to binding
;; them with a lambda closure.  It is similar to ECMAScript generators
;; and python generators.
;;
;; If 'proc' raises a continuable exception, it will be converted by
;; this procedure into a non-continuable exception.
(define (make-iterator proc . args)
  (define prompt-cont #f)
  (define done #f)
  (define (prompt-and-null)
    (let ([prompt prompt-cont])
      (set! prompt-cont #f)
      prompt))
  (define iter-cont (lambda (ignore)
		      ;; any exception must propagate in the context
		      ;; of the current call to the iterator, not the
		      ;; first
		      (with-exception-handler
			(lambda (c)
			  ((prompt-and-null) (lambda () (raise c))))
			(lambda () (apply proc yield args)))
		      (set! done #t)
		      ((prompt-and-null) (lambda () 'stop-iteration))))
  (define yield
    (case-lambda
      [() (yield #f)]
      [(arg)
       (when (not prompt-cont)
	 (error "yield" "yield attempted to expired prompt"))
       (call/cc
	(lambda (k)
	  (set! iter-cont k)
	  ((prompt-and-null) (lambda () arg))))]))
  (define iter
    (case-lambda
      [() (iter #f)]
      [(send-back)
       (if done
	   'stop-iteration
	   ((call/1cc
	     (lambda (k)
	       (set! prompt-cont k)
	       (iter-cont send-back)))))]))
  iter)

;; this procedure takes a generator procedure, namely a procedure
;; which has a 'yield' parameter for its first or only argument,
;; followed by such other arguments (other than the one for the
;; 'yield' parameter) as the generator procedure requires, and
;; constructs a coroutine.  It is similar to make-iterator, in that it
;; takes a generator procedure and returns a lambda object (a
;; coroutine) which when called will begin executing the generator
;; procedure unless and until the argument comprising the yield
;; procedure is called, which will cause computation to be suspended.
;; However unlike make-iterator, the resumption continuation generated
;; on yielding is returned by the coroutine when yielding rather than
;; being stored internally in an iterator, so there is less explicit
;; retained mutable state.  The return value of the coroutine
;; comprises two values: first the resumption continuation, and second
;; the value (if any) passed to 'yield' when called.  If the returned
;; resumption continuation is subsequently called again, computation
;; will be resumed at the point where it last left off (the yield
;; procedure returning the value, if any, passed to the continuation
;; on resuming) until it completes or it again calls the yield
;; procedure.
;;
;; Upon the generator procedure finally completing, the value to which
;; it evaluates is returned by the resumption continuation together
;; with a continuation value of #f.  This differs from the behaviour
;; of make-iterator, which returns 'stop-iteration when the generator
;; procedure finishes to completion and ignores its return value.
;;
;; If 'proc' raises a continuable exception, it will be converted by
;; this procedure into a non-continuable exception.
(define (make-coroutine proc . args)
  (define prompt-cont #f)
  (define (prompt-and-null)
    (let ([prompt prompt-cont])
      (set! prompt-cont #f)
      prompt))
  (define yield
    (case-lambda
      [() (yield #f)]
      [(ret)
       (when (not prompt-cont)
	 (error "yield" "yield attempted to expired prompt"))
       (call/cc
	(lambda (ky)
	  (define resume
	    (case-lambda
	      [() (resume #f)]
	      [(arg)
	       ((call/1cc
		 (lambda (kr)
		   (set! prompt-cont kr)
		   (ky arg))))]))
	  ((prompt-and-null) (lambda () (values resume ret)))))]))
  ;; 'arg' is ignored - it is provided only for consistency with the
  ;; interface of resume
  (define cor
    (case-lambda
      [() (cor #f)]
      [(arg)
       ((call/1cc
	 (lambda (k)
	   (set! prompt-cont k)
	   ;; any exception must propagate in the context of the
	   ;; current call to the coroutine, not the first
	   (let ([ret (with-exception-handler
			(lambda (c)
			  ((prompt-and-null) (lambda () (raise c))))
			(lambda () (apply proc yield args)))])
	     ((prompt-and-null) (lambda () (values #f ret)))))))]))
  cor)
     
;; a-sync takes a waitable procedure (namely a procedure which takes
;; 'await' as its first parameter, which is a yield procedure obtained
;; by a call to make-iterator, and 'resume' as its second parameter,
;; which is an iterator constructed by make-iterator), followed by
;; such other arguments (if any) as the waitable procedure requires to
;; be passed on to it.  The 'resume' argument must only be called by
;; an asynchronous callback, and the 'await' argument must only be
;; called by the waitable procedure in order to block until the
;; callback is ready to let it resume.  When it unblocks, the 'await'
;; argument returns the value (if any) passed to 'resume' by the
;; callback.  This async procedure must be called in the same thread
;; as that in which the event loop runs (as must 'await' and
;; 'resume').
;;
;; None of the code in the waitable procedure should block on other
;; things in the program, except by calls to await (which do not in
;; fact block, even though they appear to do so).
;;
;; The way it works is that the call to a-sync will begin executing
;; the waitable procedure and will return as soon as the first (or
;; only) call to 'await' is made by that procedure, or as soon as the
;; waitable procedure returns if it makes no calls to 'await'.  Any
;; subsequent resumptions of the waitable procedure will take place in
;; the event loop concerned as calls to a relevant continuation
;; object, via the calls to 'resume' made by the callbacks.  The
;; effect of the waitable procedure subsequently ending, or of further
;; calls to 'await' being made within the same waitable procedure, is
;; to return control to the event loop by recommencing execution at
;; the point where the most recent previous call to 'resume' was made
;; by the last callback to execute.
;;
;; An exception raised in a waitable procedure before the first call
;; to 'await' to be made by it which is not handled locally will
;; propagate out of the a-sync procedure where it may be caught
;; normally.  However, if so caught but a callback established by the
;; same waitable procedure call still runs and invokes 'resume', the
;; waitable procedure will begin running again in the callback.  If
;; the same exception is raised again in consequence, it will
;; propagate out of 'resume' in that callback, and then out of the
;; event loop - if the event loop in the event-loop library file is
;; used, this means that it will propagate out of the call to
;; event-loop-run!.  It is therefore best if such exceptions are
;; handled locally within the waitable procedure.  Any exception
;; raised in the waitable procedure after the first call to 'await'
;; which is not handled locally will propagate into the previously
;; called callback at the point where 'resume' was last called.  If
;; this is handled in the callback, then control will be returned to
;; the event loop and the remainder of the waitable procedure will not
;; execute.  If that exception is not handled locally in the callback,
;; or if the callback raises an exception of its own, then it will
;; propagate out of the event loop - if the event loop in the
;; event-loop library file is used, this means that it will propagate
;; out of the call to event-loop-run!.  If an exception propagates out
;; of event-loop-run! for that or some other reason, then the event
;; loop will be left in a valid state and it will be as if
;; event-loop-quit! had been called on it, but it is then up to the
;; user to catch that exception once it is out of event-loop-run! if
;; she does not want the program to terminate.
;;
;; With respect to what has been said about exceptions, one further
;; feature is that if the waitable procedure raises a continuable
;; exception which is not handled locally, it will be converted by
;; this procedure into a non-continuable exception.
;;
;; After the call to 'resume', the callback should normally just
;; return (with a #t or #f value in the case of a file watch or a
;; timeout on an event-loop object from the event loop module).  If
;; a-sync is used with a file watch or timeout on an event-loop object
;; constructed by make-event-loop, the watch callback or timeout
;; callback should normally, when the call to 'resume' returns, either
;; always return #f (so the callback only fires once) or always return
;; #t (so it is responsibility of the waitable procedure to terminate
;; the watch or timeout repetitions).  That way, there can never be a
;; case where the callback has been removed from the event loop by
;; returning false but the waitable procedure still thinks it has a
;; call to 'await' to be made.  The event-loop library file has
;; await-task!, await-task-in-thread!, await-task-in-event-loop!,
;; await-yield!, await-generator!, await-generator-in-thread!,
;; await-generator-in-event-loop!, await-timeout!, await-sleep!,
;; a-sync-read-watch!, await-getline!, await-geteveryline!,
;; await-getsomelines!, await-getblock!, await-geteveryblock!,
;; await-getsomeblocks!, a-sync-write-watch!, await-put-bytevector!
;; and await-put-string! convenience procedures, and the thread-pool
;; library file has await-task-in-thread-pool! and
;; await-generator-in-thread-pool! procedures, which will correctly
;; set this up for you automatically.  If those convenience procedures
;; are used, exceptions should always be handled locally in the
;; waitable procedure (and if the callback might throw, in the
;; callback also) if it is undesirable that uncaught exceptions
;; propagate out of event-loop-run!.  In the case of
;; await-task-in-thread!, await-task-in-thread-pool!,
;; await-generator-in-thread! and await-generator-in-thread-pool!,
;; those procedures also take an optional handler argument which will
;; handle any exceptions thrown by the task or generator: otherwise a
;; throwing thread would terminate the program if not caught.
;;
;; There can be as many calls to 'await' and asynchronous callbacks in
;; any one waitable procedure as wanted, to enable composition of
;; asynchronous operations.  However, you cannot run two or more
;; asynchronous tasks at the same time with the same await-resume pair
;; without an intervening call to await except by doing extra work,
;; because the first call to 'await' will match the first callback
;; which happens to call 'resume', and so on.  In such cases, 'resume'
;; would need to return something like a key-value pair so that the
;; result can be correctly identified.  Accordingly this practice is
;; discouraged.  Instead, when composing asynchronous tasks within any
;; one waitable procedure, operate on a 'start-task -->
;; await-on-result --> start-task --> await-on-result ...' basis, and
;; make calls to a-sync on separate waitable procedures for tasks
;; which are to run independently.  The convenience procedures
;; mentioned above make this easy for many use cases (see the
;; examples).  A waitable procedure can itself call a-sync to
;; construct another await-resume pair for the purpose of starting
;; other asynchronous events.
(define (a-sync waitable . args)
  (letrec ((resume (make-iterator (lambda (await)
				  (apply waitable await resume args)))))
    (resume)))

) ;; library
