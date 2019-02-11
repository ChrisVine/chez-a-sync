#!/usr/bin/env scheme-script

;; Copyright (C) 2016 to 2018 Chris Vine
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is an example file for using asynchronous reads and writes on
;; sockets.  It will provide the caller's IPv4 internet address from
;; checkip.dyndns.com.  Normally if you wanted to do this from a
;; utility script, you would do it synchronously with blocking
;; operations.  However in a program using an event loop, you would
;; need to do it asynchronously.  This does so.
;;
;; This file uses the chez-simple-sockets library from
;; https://github.com/ChrisVine/chez-simple-sockets

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(import (a-sync coroutines)
	(a-sync event-loop)
	(simple-sockets basic)
	(simple-sockets a-sync)
	(chezscheme))

(define check-ip "checkip.dyndns.com")

(define (await-read-response await resume sockport)
  (define header-done #f)
  (define header "")
  (define body "")
  (await-geteveryline! await resume sockport
		       (lambda (line)
			 (if header-done
			     (if (string=? body "")
				 (set! body line)
				 (set! body (string-append body "\n" line)))
			     (if (string=? line "")
				 (set! header-done #t)
				 (if (string=? header "")
				     (set! header line)
				     (set! header (string-append header "\n" line)))))))
  (values header body))

(define (await-send-get-request await resume host path sockport)
  (await-put-string! await resume sockport
		     (string-append "GET " path " HTTP/1.1\nHost: " host "\nConnection: close\n\n")))

(define (make-sockport codec socket)
  ;; we can construct a port for input only as await-put-string! does
  ;; not use the port's output buffers
  (let ([sockport (open-fd-input-port socket
				      (buffer-mode block)
				      (make-transcoder codec 'crlf))])
    ;; and make the socket non-blocking
    (set-port-nonblocking! sockport #t)
    sockport))

(set-default-event-loop!)
(event-loop-block! #t)  ;; we invoke await-task-in-thread!

(a-sync
 (lambda (await resume)
   (set-ignore-sigpipe)
   ;; getaddrinfo can block, so call it up with await-task-in-thread!,
   ;; await-task-in-event-loop! or await-task-in-thread-pool!
   (let* ([socket (await-task-in-thread! await resume
					(lambda ()
					  (connect-to-ipv4-host check-ip "http" 0)))]
	  [sockport (make-sockport (utf-8-codec) socket)])
     (await-send-get-request await resume check-ip "/" sockport)
     (let-values ([(header body) (await-read-response await resume sockport)])
       (display body)
       (newline))
     (event-loop-block! #f)
     (close-port sockport))))

(event-loop-run!)
