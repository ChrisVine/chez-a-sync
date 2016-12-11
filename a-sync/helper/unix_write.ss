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

;; Signature: (a-sync-c-write fd bytevector begin end)
;;
;; This forwards to unix write, but provides a begin parameter
;; indicating the start of the bytes to be written.  The sum of begin
;; and count must not be more than the size of the bytevector.  This
;; enables a bytevector to be used repeatedly until all of it has been
;; sent.
;;
;; Returns: the number of bytes written (so 0 returned on EAGAIN or
;; EWOULDBLOCK), or on error a serious-condition is raised
(define (c-write fd bv begin count)
  (let ([res (a-sync-c-write fd bv begin count)])
    (when (= res -1)
	  (raise (condition  (make-serious-condition)
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
    [(1) (raise (condition (make-serious-condition)
			   (make-who-condition "raise-condition-if-regular-file")
			   (make-message-condition "await-put-string! procedure cannot be used with regular files")))]
    [(-1) (raise (condition (make-serious-condition)
			    (make-who-condition "raise-condition-if-regular-file")
			    (make-message-condition "C fstat() function returned an error")
			    (make-irritants-condition `(errno ,(a-sync-errno)))))]))
