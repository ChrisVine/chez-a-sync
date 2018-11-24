#!/usr/bin/scheme --script

;; Copyright (C) 2014 and 2016 Chris Vine
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

;; You can run this file from this directory with:
;;
;; CHEZSCHEMELIBDIRS=.. ./example.ss

(import (a-sync event-loop)
	(a-sync coroutines)
	(a-sync compose)
	(chezscheme))

(set-default-event-loop!)
;; because one task runs in another thread
(event-loop-block! #t)

(a-sync (lambda (await resume)

	  ;; invoke a one second timeout which does not block the
	  ;; event loop
	  (display "Beginning timeout\n")
	  (display (await-timeout! await resume 1000
				   (lambda ()
				     "Timeout ended\n")))

	  ;; launch asynchronous task: let's pretend its time
	  ;; consuming so we need to run it in a worker thread
	  ;; to avoid blocking any other events in the main loop
	  ;; (there aren't any in this example)
	  (display (await-task-in-thread! await resume
					  (lambda ()
					    (sleep (make-time 'time-duration 0 1))
					    (display "In worker thread, work done\n")
					    ;; this is the result of our extensive computation
					    "Hello via async\n")))

	  ;; obtain a line of text from a port (in this case, the
	  ;; keyboard)
	  (display "Enter a line of text at the keyboard\n")
	  (system "stty --file=/dev/tty cbreak")
	  (format #t
		  "The line was: ~A\n"
		  (await-getline! await resume
				  (open-input-file "/dev/tty")))
	  (system "stty --file=/dev/tty -cbreak")

	  ;; launch another asynchronous task, this time in the event loop thread
	  (display (await-task! await resume
				(lambda ()
				  (event-loop-quit!)
				  "Quitting\n")))))

(event-loop-run!)

;; this is the identical code using compose-a-sync for composition:

(display "\nBeginning timeout\n")
(compose-a-sync ([ret-timeout (await-timeout! 1000 (lambda ()
                                                     "Timeout ended\n"))]
		 ;; the return value here can be ignored
		 [ignore ((no-await (display ret-timeout)))]
		 [ret-task (await-task-in-thread! (lambda ()
						    (sleep (make-time 'time-duration 0 1))
						    (display "In worker thread, work done\n")
						    "Hello via async\n"))]
		 ;; ditto
		 [ignore ((no-await (display ret-task)
				    (display "Enter a line of text at the keyboard\n")
				    (system "stty --file=/dev/tty cbreak")))]
		 [ret-getline (await-getline! (open-input-file "/dev/tty"))])
		;; body clauses begin here
		((no-await (format #t
				   "The line was: ~A\n"
				   ret-getline)
			   (system "stty --file=/dev/tty -cbreak")))
		(await-task! (lambda ()
			       (event-loop-quit!)
			       (display "Quitting\n"))))

(event-loop-run!)
