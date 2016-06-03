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


(define a-sync-pipe (foreign-procedure "a_sync_pipe"
				       (u8* u8*)
				       int))

;; this procedure returns two values, first a port for the read end of
;; a unix pipe, and second a port for its write end.  If creating the
;; pipe gives rise to an error, a scheme exception of type &serious
;; will be raised.  The buffer mode arguments are optional (if not
;; specified the read port will be block buffered and the write port
;; will be unbuffered).  The transcoder argument is also optional: if
;; provided the ports will be textual ports, otherwise they will be
;; binary ports.  The ports are initially in blocking mode - use
;; set-blocking-mode! to change this.
(define make-pipe
  (case-lambda
    [() (make-pipe (buffer-mode block) (buffer-mode none) #f)]
    [(read-b-mode) (make-pipe read-b-mode (buffer-mode none) #f)]
    [(read-b-mode write-b-mode) (make-pipe read-b-mode write-b-mode #f)]
    [(read-b-mode write-b-mode transcoder)
     (let ([read-bv (make-bytevector 4)]
	   [write-bv (make-bytevector 4)])
       (let ([res (a-sync-pipe read-bv write-bv)])
	 (when (= res -1)
	   (raise (condition (make-serious-condition)
			     (make-who-condition "make-pipe")
			     (make-message-condition "C pipe() function returned an error")
			     (make-irritants-condition `(errno ,(a-sync-errno))))))
	 (values
	  (open-fd-input-port (bytevector-s32-native-ref read-bv 0)
			      read-b-mode transcoder)
	  (open-fd-output-port (bytevector-s32-native-ref write-bv 0)
			       write-b-mode transcoder))))]))
