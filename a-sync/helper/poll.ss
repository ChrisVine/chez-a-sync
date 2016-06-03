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


(define a-sync-make-poll-table (foreign-procedure "a_sync_make_poll_table"
						  (u8* u8* u8*)
						  int))
(define a-sync-poll (foreign-procedure "a_sync_poll"
				       (u8* int u8* int u8* int u8* int int)
				       int))
(define a-sync-sizeof-pollfd (foreign-procedure "a_sync_sizeof_pollfd"
						()
						int))

;; this converts a list of file descriptors to a bytevector of file
;; descriptors suitable for the a-sync-make-poll-table procedure.
;; R6RS bytevectors assume bytes are octets and negative numbers are
;; 2's complement.
(define (fdlist->bytevector lst)
  (let ([bv (make-bytevector (* 4 (1+ (length lst))))])
    (let loop ([remaining lst]
	       [byte-count 0])
      (if (not (null? remaining))
	  (begin
	    (bytevector-s32-native-set! bv byte-count (car remaining))
	    (loop (cdr remaining) (+ 4 byte-count)))
	  (begin
	    (bytevector-s32-native-set! bv byte-count -1)
	    bv)))))

;; this converts a bytevector of -1 teminated file descriptors to a
;; list of descriptors, suitable for constructing the return values of
;; the poll procedure.  R6RS bytevectors assume bytes are octets and
;; negative numbers are 2's complement.
(define (bytevector->fdlist bv)
  (let loop ([byte-count 0])
    (let ([val (bytevector-s32-native-ref bv byte-count)])
      (if (= -1 val)
	'()
	(cons val (loop (+ 4 byte-count)))))))

;; this removes duplicate entries from an input list of file
;; descriptors.  Ordering of entries is not retained.
(define (unique-fds lst)
  (let loop ([prev #f]
	     [remaining (list-sort > lst)]
	     [acc '()])
    (if (null? remaining)
	acc
	(let ([cur (car remaining)])
	  (if (eqv? prev cur)
	      (loop cur (cdr remaining) acc)
	      (loop cur (cdr remaining) (cons cur acc)))))))

;; this maps over a list of file descriptors, and converts any file
;; descriptor in the input list into a port in the output list using
;; the hashtable ht, if the file descriptor exists as a key in the
;; hashtable.  It is used by the poll procedure to construct its
;; return values.
(define (fdlist->port-and-fd-list lst ht)
  (map (lambda (fd)
	 (or (hashtable-ref ht fd #f) fd))
       lst))

;; this maps over a list of file descriptors and ports, and converts
;; any port in the input list into a file descriptor, suitable for
;; passing to the fdlist->bytevector procedure for the
;; a-sync-make-poll-table procedure
(define (port-and-fd-list->fdlist lst)
  (map (lambda (elt)
	 (if (and (port? elt) (file-port? elt))
	     (port-file-descriptor elt)
	     elt))
       lst))

;; this takes a list of file descriptors and ports, and extracts the
;; ports into a hashtable with its file descriptor as key and the port
;; as value, suitable for passing to the poll procedure
(define (port-and-fd-list->port-hash lst)
  (define ht (make-eqv-hashtable))
  (let loop ([remaining (filter (lambda (elt)
				  (and (port? elt)
				       (file-port? elt)))
				lst)])
    (if (null? remaining)
	ht
	(let ([port (car remaining)])
	  (hashtable-set! ht (port-file-descriptor port) port)
	  (loop (cdr remaining))))))
    

;; poll-table must be a bytevector representing an array of pollfd
;; objects, as constructed by the a-sync-make-poll-table procedure.
;; poll-table-size is the number of pollfd elements in the array, as
;; returned by a-sync-make-poll-table.  out-read-bv is a bytevector
;; which will filled with the file descriptors which the poll() system
;; call finds are ready for reading and terminated by -1, so it should
;; be one more than the maximum number of file descriptors/ports for
;; which there is a read watch which this procedure is intended to
;; return on any one iteration.  The maximum number of read watches
;; which this procedure is intended to return on any one iteration
;; would normally be the number of file descriptors for reading in
;; poll-table, but can be less: where there is insufficient space for
;; all of them which are ready in the bytevector, the ones not
;; returned can be attended to in the next iteration of this
;; procedure.  out-write-bv and out-except-bv behave similarly.  Note
;; however that an exceptional condition watch only applies to read
;; watches, and because exceptional conditions are rare to
;; non-existent, out-except-bv can reasonably be smaller than the
;; number of read watches.  Passing in pre-formed bytevectors to hand
;; on to a-sync-poll is safe but somewhat cludgey, and is done because
;; it avoids this procedure having to construct new bytevectors for
;; a-sync-poll every time it is called, so improving efficiency (they
;; can be cached and only regenerated when a file watch changes), and
;; enables memory to be managed by the chez scheme heap manager and
;; not the C heap allocator.  timeout should be the duration of the
;; timeout in milliseconds, or a negative number for no timeout (a
;; timeout of 0 will cause this procedure to return immediately).
;; read-ports-hash is a hashtable of the read watches which relate to
;; ports rather than raw file descriptors, and likewise
;; write-ports-hash for write watches.  Although on return
;; out-read-bv, out-write-bv and out-except-bv will be filled with a
;; -1 terminated array of the file descriptors which the poll() system
;; call has found ready, you wouldn't normally need to access these
;; directly, as this procedure returns a list of three sub-lists, each
;; sub-list containing the read, write and exceptional conditions
;; respectively which have been found to be ready, after making
;; allowance for the fact that a read watch on a port will be found to
;; be ready if there is something to be read in its buffer even if the
;; port's file descriptor is not itself ready.  The entries in the
;; read and exceptional condition sublists comprise ports where the
;; port concerned appears in read-ports-hash, and otherwise the file
;; descriptor, and the write condition sublist behaves likewise with
;; respect to write-ports-hash.  On timeout, EINTR, EAGAIN or
;; EWOULDBLOCK the return value will be a list of empty lists.  Any
;; other error arising from the call to a-sync-poll will give rise to
;; a scheme exception of type &serious.
(define (poll poll-table poll-table-size read-ports-hash write-ports-hash
	      out-read-bv out-write-bv out-except-bv timeout)
  (let ([out-read-size (exact (truncate (/ (bytevector-length out-read-bv) 4)))]
	[out-write-size (exact (truncate (/ (bytevector-length out-write-bv) 4)))]
	[out-except-size (exact (truncate (/ (bytevector-length out-except-bv) 4)))]
	;; ready-read-ports is a list of file descriptors for any
	;; ports in read-ports-hash which have items available in
	;; their buffers and so are ready for reading
	[ready-read-ports
	 ;; surely there ought to be a better way to iterate over a
	 ;; hash table than by constructing two new vectors?
	 (let-values([(keys values) (hashtable-entries read-ports-hash)]
		     [(size) (hashtable-size read-ports-hash)])
	   (let loop ([index 0]
		      [acc '()])
	     (if (< index size)
		 (let ([port (vector-ref values index)])
		   (if (and (input-port? port) (input-port-ready? port))
		       (loop (1+ index) (cons (vector-ref keys index) acc))
		       (loop (1+ index) acc)))
		 acc)))])
    (when (or (< out-read-size 1)
	      (< out-write-size 1)
	      (< out-except-size 1))
      (assertion-violation "poll" "poll called with incorrectly sized bytevectors"))
    (let ([res (a-sync-poll poll-table poll-table-size out-read-bv
			    out-read-size out-write-bv out-write-size
			    out-except-bv out-except-size
			    (if (null? ready-read-ports) timeout 0))])
      (case res
	[(-1) (raise (condition (make-serious-condition)
				(make-who-condition "poll")
				(make-message-condition "C poll() function returned an error")
				(make-irritants-condition `(errno ,(a-sync-errno)))))]
	[(0) (list (fdlist->port-and-fd-list
		    (if (null? ready-read-ports)
			(bytevector->fdlist out-read-bv)
			(unique-fds (append ready-read-ports (bytevector->fdlist out-read-bv))))
		    read-ports-hash)
		   (fdlist->port-and-fd-list
		    (bytevector->fdlist out-write-bv)
		    write-ports-hash)
		   (fdlist->port-and-fd-list
		    (bytevector->fdlist out-except-bv)
		    ;; POLLPRI is only set for file descriptors with
		    ;; a read watch, and a POLLPRI condition is only
		    ;; reported to the read watch action callback
		    read-ports-hash))]
	[(1 2) (if (null? ready-read-ports)
		   '(()()())
		   (list (fdlist->port-and-fd-list
			  ready-read-ports
			  read-ports-hash)
			 '()
			 '()))]
	[else (error "poll" "a-sync-poll procedure returned invalid value")]))))
