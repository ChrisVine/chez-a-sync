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

(library (a-sync meeting)
  (export
   make-meeting
   meeting?
   meeting-close
   meeting-ready?
   meeting-send
   meeting-receive)
  (import
   (a-sync event-loop)
   (chezscheme))

(include "helper/queue.ss")

(define-record-type (meeting _make-meeting meeting?)
  (fields (immutable resumptions resumptions-get)
	  (immutable loop loop-get)
	  (mutable status status-get status-set!)))

;; This procedure makes and returns a meeting object.  Meetings are
;; objects on which two a-sync or compose-a-sync blocks running on the
;; same event loop can synchronize by one passing a datum to the
;; other.  The 'loop' argument specifies the event loop (as
;; constructed by make-event-loop in the (a-sync event-loop module))
;; with respect to which the meeting will be held: it is an error if
;; the meeting-send or meeting-receive procedures are passed a
;; different event loop as an argument.  The 'loop' argument is
;; optional - if none is passed, or #f is passed, then the default
;; event loop will be chosen.
;;
;; Strictly speaking this procedure can be called in any native OS
;; thread, but since it carries out no synchronization of native
;; threads the user would have to provide her own synchronization if
;; called in other than the thread of the event loop with respect to
;; which the meeting will be held; so it is best if this procedure is
;; called in the thread of that event loop.
;;
;; This procedure is first available in version 0.13 of this library.
(define make-meeting
  (case-lambda
    [() (make-meeting #f)]
    [(loop)
     (let ((loop (or loop (get-default-event-loop))))
       (when (not loop)
	 (error "make-meeting"
		"No default event loop set for call to make-meeting"))
       (_make-meeting (make-q) loop 'unset))]))

;; This closes a meeting object.  It's purpose is to wake up any
;; "pseudo-thread" (that is, any a-sync or compose-a-sync block)
;; waiting in meeting-send or meeting-receive by causing either
;; procedure to return with a 'stop-iteration value.
;;
;; Where that is not necessary (say, the receiver already knows how
;; many items are to be sent), then this procedure does not need to be
;; applied.  It is not needed in order to release resources.
;;
;; This procedure is first available in version 0.13 of this library.
(define (meeting-close m)
  (let ([res (resumptions-get m)]
	[loop (loop-get m)])
    (when (not (q-empty? res))
      (let lp ((elt (q-deq! res)))
	(event-post! (lambda () ((car elt) 'stop-iteration))
		     loop)
	(when (not (q-empty? res))
	  (lp (q-deq! res)))))
    (status-set! m 'closed)))

;; This indicates whether applying message-send or message-receive (as
;; the case may be) to the meeting object 'm' will return immediately:
;; in other words, this procedure will return #t if another a-sync or
;; compose-a-sync block is already waiting on the object or the
;; meeting object has been closed, otherwise #f.
;;
;; This procedure is first available in version 0.13 of this library.
(define (meeting-ready? m)
  (or (not (q-empty? (resumptions-get m)))
      (eq? (status-get m) 'closed)))

;; This sends a datum to a receiver which is running on the same event
;; loop as the sender, via the meeting object 'm'.  If no receiver is
;; waiting for the datum, this procedure waits until a receiver calls
;; meeting-receive to request the datum.  If a receiver is already
;; waiting, this procedure passes on the datum and returns
;; immediately.
;;
;; The 'loop' argument is optional.  If not supplied, or #f is passed,
;; this procedure will use the default event loop.  It is an error if
;; this procedure is given a different event loop than the one which
;; was passed to make-meeting on constructing the 'meeting' object.
;;
;; This procedure is intended to be called within a waitable procedure
;; invoked by a-sync (which supplies the 'await' and 'resume'
;; arguments).
;;
;; With version 0.13 of this library, a sender could not invoke this
;; procedure when another a-sync or compose-a-sync block running on
;; the event loop concerned was already waiting to send on the same
;; 'meeting' object.  From version 0.14, multiple senders may wait on
;; a meeting object.  The provided datum of each sender will be passed
;; to a receiver (as and when a receiver becomes available) in the
;; order in which this procedure was invoked.
;;
;; Once a datum exchange has taken place, the meeting object can be
;; reused for making another exchange (provided the meeting object has
;; not been closed).
;;
;; This procedure must be called in the native OS thread in which the
;; event loop concerned runs.  To have other native OS threads
;; communicate with an event-loop, use await-task-in-thread!,
;; await-task-in-event-loop!, await-generator-in-thread! or
;; await-generator-in-event-loop!.
;;
;; This procedure always returns #f unless meeting-close has been
;; applied to the meeting object, in which case 'stop-iteration is
;; returned.
;;
;; This procedure is first available in version 0.13 of this library.
(define meeting-send
  (case-lambda 
    [(await resume m datum)
     (meeting-send await resume #f m datum)]
    [(await resume loop m datum)
     ;; If status is already at 'set, another sender is already
     ;; waiting on this meeting and we should add ourselves to the
     ;; queue and also wait.  Otherwise, if resumptions is not empty
     ;; then it must contain resumption iterators for one or more
     ;; waiting receivers so we can proceed, or if it is empty then
     ;; nothing is waiting and this sender must add itself to the
     ;; queue and wait instead.
     (let ([loop (or loop (get-default-event-loop))]
	   [res (resumptions-get m)])
       (when (not (eq? loop (loop-get m))) 
	 (error "meeting-send"
		"meeting-send passed an event loop object for which the meeting was not constructed"))
       (case (status-get m)
	 ((closed)
	  'stop-iteration)
	 ((set)
	  (when (q-empty? res)
	    (error "meeting-send"
		   "meeting-send encountered a set meeting object with no sender resumption iterator"))
	  (q-enq! res (cons resume datum))
	  (await))
	 ((unset)
	  (if (q-empty? res)
	      (begin
		(status-set! m 'set)
		(q-enq! res (cons resume datum))
		(await))
	      (let ((elt (q-deq! res)))
		(event-post! (lambda ()
			       ((car elt) datum))
			     loop)
		#f)))))]))

;; This receives a datum from a sender running on the same event loop
;; as the receiver, via the meeting object 'm'.  If no sender is
;; waiting to pass the datum, this procedure waits until a sender
;; calls meeting-send to provide the datum.  If a sender is already
;; waiting, this procedure returns immediately with the datum
;; supplied.
;;
;; The 'loop' argument is optional.  If not supplied, or #f is passed,
;; this procedure will use the default event loop.  It is an error if
;; this procedure is given a different event loop than the one which
;; was passed to make-meeting on constructing the 'meeting' object.
;;
;; This procedure is intended to be called within a waitable procedure
;; invoked by a-sync (which supplies the 'await' and 'resume'
;; arguments).
;;
;; With version 0.13 of this library, a receiver could not invoke this
;; procedure when another a-sync or compose-a-sync block running on
;; the event loop concerned was already waiting to receive from the
;; same 'meeting' object.  From version 0.14, multiple receivers may
;; wait on a meeting object.  The waiting receivers will be released
;; (as and when a sender provides a datum) in the order in which this
;; procedure was invoked.
;;
;; Once a datum exchange has taken place, the meeting object can be
;; reused for making another exchange (provided the meeting object has
;; not been closed).
;;
;; This procedure must be called in the native OS thread in which the
;; event loop concerned runs.  To have other native OS threads
;; communicate with an event-loop, use await-task-in-thread!,
;; await-task-in-event-loop!, await-generator-in-thread! or
;; await-generator-in-event-loop!.
;;
;; This procedure always returns the datum value supplied by
;; meeting-send unless meeting-close has been applied to the meeting
;; object, in which case 'stop-iteration is returned.
;;
;; This procedure is first available in version 0.13 of this library.
(define meeting-receive
  (case-lambda 
    [(await resume m)
     (meeting-receive await resume #f m)]
    [(await resume loop m)
     ;; We can only enter this procedure under two circumstances:
     ;; either status is 'unset, which means that a sender is not
     ;; waiting and we must add ourselves to the queue and wait, or
     ;; status is 'set and resumptions is not empty, in which case a
     ;; sender is waiting and we can proceed.
     (let ([loop (or loop (get-default-event-loop))]
	   [res (resumptions-get m)])
       (when (not (eq? loop (loop-get m)))
	 (error "meeting-receive"
		"meeting-receive passed an event loop object for which the meeting was not constructed"))
       (case (status-get m)
	 ((closed)
	  'stop-iteration)
	 ((unset)
	  (q-enq! res (cons resume #f))
	  (await))
	 ((set)
	  (when (q-empty? res)
	    (error "meeting-receive"
		   "meeting-receive encountered a set meeting object with no sender resumption iterator"))
	  (let ((elt (q-deq! res)))
	    (event-post! (lambda ()
			   ((car elt) #f))
			 loop)
	    (when (q-empty? res) (status-set! m 'unset))
	    (cdr elt)))))]))

) ;; library
