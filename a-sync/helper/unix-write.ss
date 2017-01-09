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


;; Signature: (a-sync-c-write fd bytevector begin end)
;;
;; This forwards to unix write, but provides a begin parameter
;; indicating the start of the bytes to be written.  The sum of begin
;; and count must not be more than the size of the bytevector.  This
;; enables a bytevector to be used repeatedly until all of it has been
;; sent.
;;
;; Returns: 0 on EAGAIN or EWOULDBLOCK, -1 on any other error (with
;; errno set), otherwise the number of bytes written

(define a-sync-c-write (foreign-procedure "a_sync_c_write"
					  (int u8* size_t size_t)
					  ssize_t))

;; This procedure is used by await-put-bytevector! (and so by
;; await-put-string!) and is exported by event-loop.ss so that it can
;; be used by other asynchronous procedures.  It makes a block write
;; directly to output, bypassing any output buffers, using unix write.
;; Although it can be used with blocking file descriptors, it is
;; mainly intended for use with asynchronous procedures which must not
;; block and must write immediately without requiring a subsequent
;; flush to do so (chez scheme's textual ports always implement some
;; buffering and will not write without a flush, irrespective of their
;; buffering status on construction).
;;
;; This procedure provides a 'begin' parameter indicating the start of
;; the sequence of bytes to be written, as an index.  'fd' is the file
;; descriptor of the device to be written to, and it should normally
;; be non-blocking (say, 'fd' is derived from a port to which
;; set-port-nonblocking! has been applied with an argument of #t).
;; 'bv' is a bytevector containing the bytes to be written.  'count'
;; is the maximum number of bytes to be written.  This procedure
;; returns the number of bytes actually written, which can be less
;; than 'count' bytes.  The sum of 'begin' and 'count' must not be
;; more than the length of the bytevector.  The use of a separate
;; 'begin' index enables the same bytevector to be written from
;; repeatedly until all of it has been sent.
;;
;; Provided 'fd' is non-blocking, this procedure returns straight away
;; (so 0 may be returned if the file descriptor is not available for
;; writing because of insufficient space).  On a write error other
;; than EAGAIN, EWOULDBLOCK or EINTR, a &i/o-write-error exception is
;; raised which will give the errno number as an irritant (prior to
;; version 0.11 a &serious exception was raised).  EINTR is handled
;; internally and is not an error.
;;
;; This procedure is first available in version 0.8 of this library.
(define (c-write fd bv begin count)
  (let ([res (a-sync-c-write fd bv begin count)])
    (when (= res -1)
	  (raise (condition (make-i/o-write-error)
			    (make-who-condition "c-write")
			    (make-message-condition "C write() function returned an error")
			    (make-irritants-condition `(errno ,(a-sync-errno))))))
    res))
	
(define a-sync-regular-file-p (foreign-procedure "a_sync_regular_file_p"
						 (int)
						 int))

(define (raise-exception-if-regular-file fd)
  (case (a-sync-regular-file-p fd)
    [(0) #f]
    [(1) (raise (condition (make-i/o-write-error)
			   (make-who-condition "raise-condition-if-regular-file")
			   (make-message-condition "await-put-bytevector! procedure cannot be used with regular files")))]
    [(-1) (raise (condition (make-i/o-write-error)
			    (make-who-condition "raise-condition-if-regular-file")
			    (make-message-condition "C fstat() function returned an error")
			    (make-irritants-condition `(errno ,(a-sync-errno)))))]))
