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

#!r6rs

(library (a-sync thread-pool)
  (export
   make-thread-pool
   thread-pool?
   thread-pool-get-num-tasks
   thread-pool-get-num-threads
   thread-pool-get-size
   thread-pool-change-size!
   thread-pool-get-non-blocking
   thread-pool-set-non-blocking!
   thread-pool-stop!
   thread-pool-add!
   with-thread-pool-increment
   await-task-in-thread-pool!
   await-generator-in-thread-pool!)
  (import (a-sync try)
	  (a-sync coroutines) ;; for make-iterator
	  (a-sync event-loop) ;; for event-post!
	  (chezscheme))

(include "helper/queue.ss")
(include "helper/match.ss")

(define-record-type (a-queue _make-a-queue a-queue?)
  (fields (immutable mutex aq-mutex-get)
	  (immutable condvar aq-condvar-get)
	  (immutable q q-get)))

   
(define (make-a-queue)
  (_make-a-queue (make-mutex)
		 (make-condition)
		 (make-q)))

(define (a-queue-pop! aq)
  (let ((mutex (aq-mutex-get aq)))
    (with-mutex mutex
      (let ((q (q-get aq)))
        (let lp ()
          (if (q-empty? q)
              (begin
                (condition-wait (aq-condvar-get aq) mutex)
                (lp))
	      (q-deq! q)))))))

(define (a-queue-push! aq item)
  (with-mutex (aq-mutex-get aq)
    (q-enq! (q-get aq) item))
  (condition-signal (aq-condvar-get aq)))


(define-record-type (thread-pool _make-thread-pool thread-pool?)
  (fields (immutable mutex mutex-get)
	  (immutable condvar condvar-get)
	  (immutable aq aq-get)
	  (mutable size size-get size-set!)
	  ;; 'num-threads' may be different to 'size' if 'size' has
	  ;; recently been changed by the user
	  (mutable num-threads num-threads-get num-threads-set!)
	  (mutable num-tasks num-tasks-get num-tasks-set!)
	  (mutable blocking blocking-get blocking-set!)
	  (mutable stopped stopped-get stopped-set!)))

;; This procedure constructs a thread pool object of native OS
;; threads.  The 'size' argument specifies the number of threads which
;; will run in the pool, and must be greater than 0.  The
;; 'non-blocking' argument is optional and affects the operation of
;; the thread-pool-stop! procedure.  When set to #f, which is the
;; default, that procedure will not return until all tasks previously
;; added to the pool have completed.  If set to #t, the
;; thread-pool-stop! procedure will return immediately, before all
;; tasks have finished.
;;
;; The 'size' and 'non-blocking' settings may subsequently be altered
;; by applying the thread-pool-change-size! or
;; thread-pool-set-non-blocking! procedure to the pool.
;;
;; Thread pool objects are usually best kept as top level objects,
;; because threads in the pool will keep alive until thread-pool-stop!
;; is called.  If a thread pool is constructed within a local lexical
;; scope, then either thread-pool-stop! must be applied to the pool
;; before that scope is exited, or the last task added to the pool
;; should itself apply thread-pool-stop! to the pool (which it can do
;; if 'non-blocking' is #t).  Otherwise, the threads in the pool will
;; remain alive uselessly in blocked condition until the program
;; terminates, even though the pool may be inaccessible.
;;
;; This procedure will throw an exception if a 'size' argument of less
;; than 1 is given, or if the system is unable to start the number of
;; threads given as the 'size' argument.  If unable to start the
;; number of threads so given, any threads which have in fact started
;; in the pool will be killed.
;;
;; This procedure is first available in version 0.16 of this library.
(define make-thread-pool
  (case-lambda
    [(size) (make-thread-pool size #f)]
    [(size non-blocking)
     (when (< size 1)
       (raise (condition (make-violation)
			 (make-who-condition "make-thread-pool")
			 (make-message-condition
			  "size argument for make-thread-pool is less than 1"))))
     (let ([pool (_make-thread-pool (make-mutex)
				    (make-condition)
				    (make-a-queue)
				    size
				    0
				    0
				    (not non-blocking)
				    #f)])
       (with-exception-handler
	 (lambda (c)
	   ;; if starting any new threads failed, kill them all before
	   ;; re-raising the exception
	   (let ([thread-count (num-threads-get pool)])
	     (do ([kill-count 0 (+ kill-count 1)])
		 ((= kill-count thread-count))
	       (a-queue-push! (aq-get pool) (cons (lambda () (raise 'kill-thread)) #f)))
	     (raise c)))
	 (lambda ()
	   (do ([count 0 (+ count 1)])
	       ((= count size))
	     (fork-thread (lambda () (thread-loop pool)))
	     (num-threads-set! pool (+ (num-threads-get pool) 1)))))
       pool)]))

;; This is the thread loop which each thread will execute, taking
;; tasks from the task queue as they become available.
(define (thread-loop pool)
  (define (object->string obj)
    (call-with-string-output-port
     (lambda (p) (put-datum p obj))))
  (call/1cc
   (lambda (return)
     (let ([mutex (mutex-get pool)])
       (let lp ([task (a-queue-pop! (aq-get pool))])
	 (try
	  ((car task))
	  (except c
		  [(eq? c 'kill-thread)
		   ;; we don't decrement 'tasks' here, as adding a
		   ;; 'kill-thread callback does not increment the
		   ;; number of tasks
		   (with-mutex mutex
		     (num-threads-set! pool (- (num-threads-get pool) 1))
		     (when (and (stopped-get pool)
				(blocking-get pool))
		       (condition-broadcast (condvar-get pool)))
		     (return #f))]
		  [else
		   (let ([fail-handler (cdr task)])
		     (if fail-handler
			 (fail-handler c)
			 (error
			  "thread-loop"
			  (string-append "Exception raised by thread pool task with no fail-handler: "
					 (object->string c)))))]))
	 (with-mutex mutex
	   (num-tasks-set! pool (- (num-tasks-get pool) 1))
	   ;; cater for a case where the number of threads in the pool
	   ;; has been reduced by the user
	   (when (> (num-threads-get pool) (size-get pool))
	     (num-threads-set! pool (- (num-threads-get pool) 1))
	     (when (and (stopped-get pool)
			(blocking-get pool))
	       (condition-broadcast (condvar-get pool)))
	     (return #f)))
	 ;; we are outside the mutex here
	 (lp (a-queue-pop! (aq-get pool))))))))

;; This procedure returns the number of tasks which the thread pool
;; object is at present either running in the pool or has queued for
;; execution.  This procedure will not throw.  It is also thread safe,
;; although it accesses the task number field outside the pool mutex
;; and therefore with relaxed memory ordering.  That enables this
;; procedure to be applied more efficiently for rate limiting purposes
;; but the result might at any one time be marginally out of date.
;;
;; This procedure is first available in version 0.16 of this library.
(define (thread-pool-get-num-tasks pool)
  ;; we do not use the thread pool mutex to protect the num-tasks
  ;; record field here for efficiency reasons, as user code may want
  ;; to call it for rate-limiting reasons.  The field could therefore
  ;; be modified by another thread concurrently with this read, but
  ;; chez guarantees that record field mutators are thread-safe.  This
  ;; gives us an equivalent to relaxed (unsynchronized) atomic memory
  ;; ordering, which is sufficient for our purpose.
  (num-tasks-get pool))

;; We do not document this procedure in the info docs or in the wiki,
;; because it is only exported for use when testings
(define (thread-pool-get-num-threads pool)
  (with-mutex (mutex-get pool)
    (num-threads-get pool)))

;; This procedure returns the current size setting for the thread pool
;; (namely the number of threads that the pool runs).
;;
;; This procedure is thread safe (any thread may call it).
;;
;; This procedure is first available in version 0.16 of this library.
(define (thread-pool-get-size pool)
  (with-mutex (mutex-get pool)
    (size-get pool)))

;; This procedure will increase, or if 'delta' is negative reduce, the
;; number of threads which the thread pool object will run by the
;; value of 'delta'.  This procedure does nothing if thread-pool-stop!
;; has previously been called.  This procedure is thread safe - any
;; thread may call it.
;;
;; One use for dynamic sizing of this kind is for a task to increment
;; the thread number where it is about to enter a call which may block
;; (non-asynchronously) for some time, with a view to decrementing it
;; later when it has finished making blocking calls, so as to enable
;; another thread to keep a core active.  A with-thread-pool-increment
;; macro is available which will do this for you automatically in an
;; exception-safe way (see the documentation on that macro below).
;;
;; Alternatively, this procedure can be used to reduce thread usage
;; when a full set of threads is no longer required by the program.
;;
;; If 'delta' is positive, this procedure may raise an exception if
;; the system is unable to start the required new threads.  Because
;; starting new threads can be time consuming, to minimize contention
;; new threads are started outside the pool's mutex, although internal
;; book-keeping is done within the mutex.  One consequence is that if
;; such an exception is raised while another thread has concurrently
;; tried to reduce the size of the pool, the size of the pool may be
;; smaller than it was when this procedure was called.  In certain
;; circumstances, after an exception where no new threads can be
;; started, the pool could have no running threads in it (so that
;; thread-pool-get-size returns 0) even though some tasks previously
;; added to the pool remain pending.  If the system can start no new
;; threads even though none are running in the pool, it will be
;; significantly broken so it is not usually worth troubling about
;; this - the program is doomed in that event whatever.  However if
;; that is wrong and the cause of the failure to start any threads can
;; be addressed, then the thread pool can be brought back into use by
;; calling this procedure again.
;;
;; This procedure is first available in version 0.16 of this library.
(define (thread-pool-change-size! pool delta)
  (let ([mutex (mutex-get pool)])
    ;; to minimize contention, we want to start any new threads
    ;; outside the mutex, but do the book-keeping within the mutex
    (let ([start-threads
	   (with-mutex mutex
	     (cond
	      [(stopped-get pool) #f]
	      ;; if size is 0 because of an exception when trying to
	      ;; start new threads (see below) and a negative delta is
	      ;; passed, just do nothing
	      [(and (zero? (size-get pool)) (< delta 0)) #f]
	      [(< delta 0)
	       (let* ([cur-size (size-get pool)]
		      [new-size (max 1 (+ cur-size delta))]
		      [num-tasks (num-tasks-get pool)]
		      [push-tasks (let ([diff (- cur-size new-size)])
				    (if (>= num-tasks diff)
					0
					(- diff num-tasks)))])
		 (size-set! pool new-size)
		 (do ([count 0 (+ count 1)])
		     ((= count push-tasks))
		   (a-queue-push! (aq-get pool) (cons (lambda () #f) #f))
		   (num-tasks-set! pool (+ num-tasks 1))))
	       #f]
	      [(> delta 0)
	       ;; don't return 'delta' directly as the 'start-threads'
	       ;; value - the pool size cannot be more than
	       ;; num-threads, but it can be less if the pool size has
	       ;; recently been reduced but insufficient tasks have
	       ;; yet finished to reduce num-threads to the pool size.
	       ;; It is more efficient only to start the number of
	       ;; threads representing the ones actually still
	       ;; required.
	       (let* ([new-size (+ (size-get pool) delta)]
		      [start-threads (- new-size (num-threads-get pool))])
		 (size-set! pool new-size)
		 (if (> start-threads 0)
		     (begin
		       (num-threads-set! pool new-size)
		       start-threads)
		     #f))]
	      [else #f]))])
      (when start-threads
	(do ([count 0 (+ count 1)])
	    ((= count start-threads))
	  (with-exception-handler
	    (lambda (c)
	      ;; roll back for any unstarted threads
	      (with-mutex mutex
		(num-threads-set! pool (+ (- (num-threads-get pool) start-threads) count))
		;; We could be down to 0 threads if all of these
		;; unfortunate events have occurred together: (i) in
		;; the period between this calling thread releasing
		;; the mutex acquired on entry to this procedure and
		;; acquiring it again on handling this exception,
		;; another thread tried, concurrently with this
		;; attempted increase, to reduce the size of the pool
		;; by an amount equal to or more than its original
		;; size, (ii) during that period a number of tasks
		;; equal to that original size have finished, and
		;; (iii) the attempt to launch new threads failed with
		;; an exception without launching even one of them.
		;; In such a case we should be able to launch a rescue
		;; thread within the mutex because no other threads
		;; could be running in the pool.  If we still cannot
		;; launch a thread the program and/or system must be
		;; totally borked and there is little we can do.
		(when (zero? (num-threads-get pool))
		  ;; if this fails, all is lost (that is, we may have
		  ;; queued tasks in the pool with no thread startable
		  ;; to run them)
		  (try 
		   (fork-thread (lambda () (thread-loop pool)))
		   (num-threads-set! pool 1)
		   (except c
			   [else #f])))
		;; reset size to the actual number of threads
		;; now running
		(size-set! pool (num-threads-get pool))
		(when (and (stopped-get pool)
			   (blocking-get pool))
		  (condition-broadcast (condvar-get pool)))
		(raise c)))
	    (lambda ()
	      (fork-thread (lambda () (thread-loop pool))))))))))

;; This procedure returns the current non-blocking status of the
;; thread pool.  (See the documentation on the thread-pool-stop!
;; procedure for more information about what that means.)
;;
;; This procedure is thread safe (any thread may call it).
;;
;; This procedure is first available in version 0.16 of this library.
(define (thread-pool-get-non-blocking pool)
  (with-mutex (mutex-get pool)
    (not (blocking-get pool))))

;; This procedure sets the non-blocking status of the thread pool.  If
;; 'val' is #f, the thread-pool-stop procedure will block, if #t it
;; will not.  (See the documentation on the thread-pool-stop!
;; procedure for more information about this.)
;;
;; This procedure is thread safe (any thread may call it).
;;
;; This procedure will raise a &violation exception if it is invoked
;; after the thread pool object concerned has been closed by a call to
;; thread-pool-stop!.
;;
;; This procedure is first available in version 0.16 of this library.
(define (thread-pool-set-non-blocking! pool val)
  (with-mutex (mutex-get pool)
    (when (stopped-get pool)
      (raise (condition (make-violation)
			(make-who-condition "thread-pool-set-non-blocking!")
			(make-message-condition
			 "thread-pool-set-non-blocking! applied to a thread pool which has been closed"))))
    (blocking-set! pool (not val))))

;; This procedure will cause the thread-pool object to stop running
;; tasks.  However, all tasks already running or queued for execution
;; will be permitted to execute and complete normally.  If the
;; thread-pool's non-blocking setting is set to #f, this procedure
;; will wait until all the tasks still to execute have finished before
;; returning, and if #t it will return straight away.
;;
;; After this procedure has been called, any attempt to add further
;; tasks with the thread-pool-add! procedure will fail, and that
;; procedure will raise a &violation exception.  The same exception
;; will be raised if this procedure is applied to a thread pool to
;; which this procedure has previously been applied.
;;
;; This procedure is thread safe (any thread may call it) unless the
;; non-blocking setting is #f, in which case no task running on the
;; thread-pool object may call this procedure.
;;
;; This procedure is first available in version 0.16 of this library.
(define (thread-pool-stop! pool)
  (let ([mutex (mutex-get pool)])
    (with-mutex mutex
      (when (stopped-get pool)
	(raise (condition (make-violation)
			  (make-who-condition "thread-pool-stop!")
			  (make-message-condition
			   "thread-pool-stop! applied to a thread pool which has been closed"))))
      (stopped-set! pool #t)
      (let ([thread-count (num-threads-get pool)])
	;; we could be adding more 'kill-thread callbacks than
	;; necessary here, because as we are doing this a concurrent
	;; call to thread-pool-change-size! may have failed to start a
	;; thread and raised an exception.  However, that doesn't
	;; matter - we just get left with a redundant callback in 'aq'
	;; which never gets used and disappears when the pool is
	;; garbage collected
	(do ([kill-count 0 (+ kill-count 1)])
	    ((= kill-count thread-count))
	  (a-queue-push! (aq-get pool) (cons (lambda () (raise 'kill-thread)) #f)))
	(when (blocking-get pool)
	  (do ()
	      ((= (num-threads-get pool) 0))
	      (condition-wait (condvar-get pool) mutex)))))))

;; This procedure adds a new task to the thread pool.  'task' must be
;; a thunk.  If one or more threads in the pool are currently blocking
;; and waiting for a task, then the task will begin executing
;; immediately in one of the threads.  If not, the task will be queued
;; for execution as soon as a thread becomes available.  Tasks will
;; begin execution in the order in which they are added to the thread
;; pool object.  This procedure is thread safe (any thread may call
;; it, including any task running on the thread pool object).
;;
;; An optional handler procedure may be passed to 'fail-handler' which
;; will be invoked if the task raises an exception.  If a task raises
;; an exception and no handler procedure is provided, the program will
;; terminate.  The 'fail-handler' procedure will be passed the
;; condition object for the exception raised.
;;
;; This procedure will raise a &violation exception if it is invoked
;; after the thread pool object concerned has been closed by a call to
;; thread-pool-stop!.
;;
;; This procedure is first available in version 0.16 of this library.
(define thread-pool-add!
  (case-lambda
    [(pool task) (thread-pool-add! pool task #f)]
    [(pool task fail-handler)
     ;; check the pool has not been closed and increment the task
     ;; count under a single mutex locking operation to ensure
     ;; atomicity within the pool
     (with-mutex (mutex-get pool)
       (when (stopped-get pool)
	 (raise (condition (make-violation)
			   (make-who-condition "thread-pool-add!")
			   (make-message-condition
			    "thread-pool-add! applied to a thread pool which has been closed"))))
       ;; We need to hold the mutex when adding the task so that
       ;; the whole operation is atomic - otherwise if
       ;; thread-pool-stop! is called concurrently with
       ;; thread-pool-add!, we cannot guarantee that a task will
       ;; either run or a violation exception will be raised.  We
       ;; must give this guarantee for await-task-in-thread-pool!
       ;; to work correctly.  That is not too much of an additional
       ;; point of contention, because a-queue-push! is itself
       ;; serialized.
       (a-queue-push! (aq-get pool) (cons task fail-handler))
       (num-tasks-set! pool (+ (num-tasks-get pool) 1)))]))

;; This macro is intended to be called by a task running on a thread
;; pool which is about to make a blocking (non-asynchronous) call.  It
;; will increment the number of threads in 'pool' by 1 (by calling
;; thread-pool-change-size!) so as to enable a queued task to keep a
;; core active, and decrement it again when execution of the body
;; clauses has completed.
;;
;; The (i) increment, (ii) execution of body clauses, and (iii)
;; decrement, form the three branches of a dynamic-wind, so the
;; decrement automatically occurs if control leaves body execution
;; because of an exception or other jump.
;;
;; As this macro starts a new thread, it may raise an exception if the
;; system is unable to start it.  Starting a new thread is also not
;; free of overhead and it may be worth profiling a prospective use
;; case to see if running a few more threads in the thread pool than
;; the number of processors in the system permanently, rather than
;; changing it using this macro, and letting the OS's scheduler handle
;; the situation, is a better option.
;;
;; This macro is first available in version 0.16 of this library.
(define-syntax with-thread-pool-increment
  (syntax-rules ()
    [(_ pool body0 body1 ...)
     (let ([p pool])
       (dynamic-wind
	 (lambda () (thread-pool-change-size! p 1))
	 (lambda () body0 body1 ...)
	 (lambda () (thread-pool-change-size! p -1))))]))

;; This is a convenience procedure whose signature is:
;;
;;   (await-task-in-thread-pool! await resume [loop] pool thunk [handler])
;;
;; The 'loop' argument is optional.  The procedure will run 'thunk' in
;; the thread pool specified by the 'pool' argument.  The result of
;; executing 'thunk' will then be posted to the event loop specified
;; by the 'loop' argument, or to the default event loop if no 'loop'
;; argument is provided or if #f is provided as the 'loop' argument
;; (pattern matching is used to detect the type of the third
;; argument), and will comprise this procedure's return value.  This
;; procedure is intended to be called within a waitable procedure
;; invoked by a-sync (which supplies the 'await' and 'resume'
;; arguments).  It will normally be necessary to call
;; event-loop-block! on 'loop' (or on the default event loop) before
;; invoking this procedure.
;;
;; If the optional 'handler' argument is provided, then that handler
;; will run if 'thunk' raises an exception, and the return value of
;; the handler would become the return value of this procedure;
;; otherwise the program will terminate if an unhandled exception
;; propagates out of 'thunk'.  'handler' should take a single
;; argument, which will be the thrown condition object.  Note that
;; unlike a handler passed to the thread-pool-add! procedure,
;; 'handler' will run in the event loop thread and not in a thread
;; pool thread.  Exceptions raised by the handler procedure will
;; propagate out of event-loop-run! for the 'loop' event loop.
;;
;; This procedure calls 'await' and must (like the a-sync procedure)
;; be called in the same thread as that in which the 'loop' or default
;; event loop runs (as the case may be).
;;
;; This procedure calls event-post! in the 'loop' or default event
;; loop, which could be subject to throttling (see the documentation
;; for the make-event-loop procedure for further information).
;;
;; Exceptions may propagate out of this procedure if they arise while
;; setting up, which shouldn't happen unless the thread pool given by
;; the 'pool' argument has been closed (in which case a &violation
;; exception will be raised), the thread pool tries to start an
;; additional native thread which the operating system fails to supply
;; (which would cause a system exception to arise) or memory is
;; exhausted.
;;
;; This procedure is first available in version 0.16 of this library.
(define (await-task-in-thread-pool! await resume . rest)
  (match rest
    ((,l ,p ,t ,h)
     (await-task-in-thread-pool-impl! await resume l p t h))
    ((#f ,p ,t)
     (await-task-in-thread-pool-impl! await resume #f p t #f))
    ((,l ,p ,t) (guard (event-loop? l))
     (await-task-in-thread-pool-impl! await resume l p t #f))
    ((,p ,t ,h)
     (await-task-in-thread-pool-impl! await resume #f p t h))
    ((,p ,t)
     (await-task-in-thread-pool-impl! await resume #f p t #f))
    (,x
     (error "await-task-in-thread-pool!"
	    "Wrong number of arguments passed to await-task-in-thread-pool!" await resume x))))

(define (await-task-in-thread-pool-impl! await resume loop pool thunk handler)
  (let ([loop (or loop (get-default-event-loop))])
    (when (not loop) 
      (error "await-task-in-thread-pool-impl!"
	     "No default event loop set for call to await-task-in-thread-pool!"))
    (if handler
	(thread-pool-add! pool
			  (lambda ()
			    (let ([res (thunk)])
			      (event-post! (lambda () (resume res))
					   loop)))
			  (lambda (c)
			    (event-post! (lambda () (resume (handler c)))
					 loop)))
	(thread-pool-add! pool
			  (lambda ()
			    (let ([res (thunk)])
			      (event-post! (lambda () (resume res))
					   loop)))))
    (await)))

;; This is a convenience procedure whose signature is:
;;
;;   (await-generator-in-thread-pool! await resume [loop] pool generator proc [handler])
;;
;; The loop argument is optional.  The 'generator' argument is a
;; procedure taking one argument, namely a yield argument (see the
;; documentation on the make-iterator procedure for further details).
;; This await-generator-in-thread-pool! procedure will cause
;; 'generator' to run as a task in the 'pool' thread pool, and
;; whenever 'generator' yields a value this will cause 'proc' to
;; execute in the event loop specified by the 'loop' argument, or in
;; the default event loop if no 'loop' argument is provided or if #f
;; is provided as the 'loop' argument.  'proc' should be a procedure
;; taking a single argument, namely the value yielded by the
;; generator.

;; This procedure is intended to be called within a waitable procedure
;; invoked by a-sync (which supplies the 'await' and 'resume'
;; arguments).  It will normally be necessary to call
;; event-loop-block! on 'loop' (or on the default event loop) before
;; invoking this procedure.
;;
;; If the optional 'handler' argument is provided, then that handler
;; will run if 'generator' raises an exception; otherwise the program
;; will terminate if an unhandled exception propagates out of
;; 'generator'.  'handler' should take a single argument, which will
;; be the thrown condition object.  Note that unlike a handler passed
;; to the thread-pool-add! procedure, 'handler' will run in the event
;; loop thread and not in a thread pool thread.  This procedure will
;; return #f if the generator completes normally, or
;; 'chez-a-sync-thread-error if the generator throws an exception and
;; 'handler' is run (the 'chez-a-sync-thread-error symbol is reserved
;; to the implementation and should not be yielded by the generator).
;; Exceptions thrown by the handler procedure will propagate out of
;; event-loop-run! for the 'loop' event loop.
;;
;; This procedure calls 'await' and will return when the generator has
;; finished or, if 'handler' is provided, upon the generator raising
;; an exception.  This procedure must (like the a-sync procedure) be
;; called in the same thread as that in which the 'loop' or default
;; event loop runs (as the case may be).
;;
;; This procedure calls event-post! in the 'loop' or default event
;; loop, which could be subject to throttling (see the documentation
;; for the make-event-loop procedure for further information).
;;
;; Exceptions may propagate out of this procedure if they arise while
;; setting up, which shouldn't happen unless the thread pool given by
;; the 'pool' argument has been closed (in which case an &violation
;; exception will be raised) or memory is exhausted.  Exceptions
;; arising during the execution of 'proc', if not caught locally, will
;; propagate out of event-loop-run!  for 'loop' or the default event
;; loop (as the case may be).
;;
;; This procedure is first available in version 0.16 of this library.
(define (await-generator-in-thread-pool! await resume . rest)
  (match rest
    ((,l ,p ,g ,pr ,h)
     (await-generator-in-thread-pool-impl! await resume l p g pr h))
    ((#f ,p ,g ,pr)
     (await-generator-in-thread-pool-impl! await resume #f p g pr #f))
    ((,l ,p ,g ,pr) (guard (event-loop? l))
     (await-generator-in-thread-pool-impl! await resume l p g pr #f))
    ((,p ,g ,pr ,h)
     (await-generator-in-thread-pool-impl! await resume #f p g pr h))
    ((,p ,g ,pr)
     (await-generator-in-thread-pool-impl! await resume #f p g pr #f))
    (,x
     (error "await-generator-in-thread-pool!"
	    "Wrong number of arguments passed to await-generator-in-thread-pool!" await resume x))))

(define (await-generator-in-thread-pool-impl! await resume loop pool generator proc handler)
  (let ([loop (or loop (get-default-event-loop))])
    (when (not loop) 
      (error "await-generator-in-thread-pool-impl!"
	     "No default event loop set for call to await-generator-in-thread-pool!"))
    (if handler
       (thread-pool-add! pool
			 (lambda ()
			   (let ([iter (make-iterator generator)])
			     (let next ([res (iter)])
			       (event-post! (lambda () (resume res))
					    loop)
			       (when (not (eq? res 'stop-iteration))
				 (next (iter))))))
			 (lambda (c)
			   (event-post! (lambda ()
					  (handler c)
					  (resume 'chez-a-sync-thread-error))
					loop)))
       (thread-pool-add! pool
			 (lambda ()
			   (let ([iter (make-iterator generator)])
			     (let next ([res (iter)])
			       (event-post! (lambda () (resume res))
					    loop)
			       (when (not (eq? res 'stop-iteration))
				 (next (iter)))))))))
  (let next ([res (await)])
    (cond
     [(eq? res 'stop-iteration)
      #f]
     [(eq? res 'chez-a-sync-thread-error)
      'chez-a-sync-thread-error]
     [else 
      (proc res)
      (next (await))])))

) ;; library
