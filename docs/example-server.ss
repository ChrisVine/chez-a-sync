#!/usr/bin/env scheme-script

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

;; This is an example file for an asynchronous socket server.  It is
;; just an echo-bot - it will echo back whatever is sent to it.
;;
;; You can send connect to it with: 'telnet ::1 8000'.  Any number of
;; telnet sessions (up to the number of file descriptors permitted by
;; the system) can be run concurrently.  When the last telnet client
;; has disconnected, the server will finish.
;;
;; This file uses the chez-simple-sockets library from
;; https://github.com/ChrisVine/chez-simple-sockets

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(import (a-sync coroutines)
	(a-sync event-loop)
	(simple-sockets basic)
	(simple-sockets a-sync)
	(chezscheme))

(set-default-event-loop!)

(define count 0)
(define server-sock #f)

(define (handle-conversation port)
  (a-sync (lambda (await resume)
            (await-getsomelines! await resume port
                                 (lambda (line k)
                                   (if (not (string=? line ""))
                                       (await-put-string! await resume port (string-append "echo: " line "\n"))
                                       (k #f))))
	    ;; we must call clear-input-port before applying
	    ;; close-port
	    (clear-input-port port)
	    (close-port port)
	    (set! count (- count 1))
	    (when (zero? count)
	      (event-loop-quit!)))))

(define (start-server)
  (set-ignore-sigpipe)
  (a-sync (lambda (await resume)
	    (set! server-sock (listen-on-ipv6-socket #t 8000 5))
	    (let loop ()
	      (let* ([vec (make-bytevector 16)]
		     [accept (await-accept-ipv6-connection! await resume server-sock vec)]
		     [port (open-fd-input/output-port accept
						      (buffer-mode block)
						      (make-transcoder (utf-8-codec) 'crlf))])
		(set! count (+ count 1))
		(set-textual-port-output-buffer! port "")
		(await-put-string! await resume port
				   (string-append
				    "Hello.  Please send some text and I will echo it back\n"
				    "Enter an empty line to finish\n"))
		(format #t "connected to ~a~%" (ipv6-address->string vec))
		(handle-conversation port)
		(loop))))))

(start-server)
(event-loop-run!)
(display "Closing server\n")
(shutdown server-sock 'rdwr)
(close-fd server-sock)
