#!/usr/bin/scheme --script

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is an example file for using asynchronous reads and writes on
;; sockets.  It will provide the caller's IPv4 internet address from
;; myip.dnsdynamic.org.  Normally if you wanted to do this from a
;; utility script, you would do it synchronously with blocking
;; operations.  However in a program using an event loop, you would
;; need to do it asynchronously.  This does so.
;;
;; This file uses the chez-sockets implementation from
;; https://github.com/arcfide/chez-sockets

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(import (arcfide sockets)
	(a-sync coroutines)
	(a-sync event-loop)
	(chezscheme))

(define check-ip "myip.dnsdynamic.org")

(define (read-response-async await resume sockport)
  (define header "")
  (define body "")
  (await-geteveryline! await resume sockport
		       (lambda (line)
			 (cond
			  [(not (string=? body ""))
			   (set! body (string-append body "\n" line))]
			  [(string=? line "")
			   (set! body (string (integer->char 0)))] ;; marker
			  [else
			   (set! header (if (string=? header "")
					    line
					    (string-append header "\n" line)))])))
  ;; get rid of marker (with \n) in body
  (set! body (substring body 2 (string-length body)))
  (values header body))

(define (send-get-request-async await resume host path sockport)
  (await-put-string! await resume sockport
		     (string-append "GET " path " HTTP/1.1\nHost: "host"\n\n")))

(define (make-sockport codec socket)
  (let ([sockport (open-fd-input/output-port (socket-fd socket) 
					     (buffer-mode block)
					     (make-transcoder codec 'crlf))])
    ;; make the output port unbuffered
    (set-textual-port-output-size! sockport 0)
    sockport))

(set-default-event-loop!)

(a-sync
 (lambda (await resume)
   (let ([socket (create-socket socket-domain/internet
				socket-type/stream
				socket-protocol/auto)]
	 ;; getaddrinfo in particular can block, so call it up with
	 ;; either await-task-in-thread! or await-task-in-event-loop!
	 [addr (await-task-in-thread! await resume
				      (lambda ()
					(let-values([(a b) (get-address-info check-ip "http")])
					  (address-info-address (car a)))))])
     ;; chez-sockets' sockets are non-blocking by default, but let's
     ;; make sure
     (set-socket-nonblocking! socket #t)
     (let ([sockport (make-sockport (utf-8-codec) socket)])
       ;; connect-socket doesn't block on non-blocking port
       (connect-socket socket addr)
       (send-get-request-async await resume check-ip "/" sockport)
       (let-values ([(header body) (read-response-async await resume sockport)])
	 (display body)
	 (newline))
       (event-loop-block! #f)))))

(event-loop-block! #t)
(event-loop-run!)
