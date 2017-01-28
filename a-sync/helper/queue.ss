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


;; This is a 'tconc' queue, which comprises two lists ('data' and
;; 'back') held in a record, where the list in 'back' ('shared', which
;; contains a placeholder for the next item to be enqueued) also forms
;; the last cdr of the list in 'data'.  'back' can therefore be used
;; to splice new items into the back of the (singly-linked) list
;; comprising 'data' without having to iterate through the 'data' list
;; to do so.

;; For large queues, efficiency can be improved by having a wrapper
;; queue comprising a tconc'ed queue of fixed-size indexed vectors
;; (requiring the holding of an additional reference to the last
;; vector so it is double ended).  However, the implementation below
;; is fine for the event loop.

;; If q-deq! is called on an empty queue, an exception of type
;; &assertion is raised.

(define-record-type (queue _make-queue queue?)
  (fields (mutable data _data-get _data-set)
	  (mutable back _back-get _back-set)))

(define (make-q)
  (let ([shared (list 'placeholder)])
    (_make-queue shared
		 shared)))

(define (q-enq! q item)
  (let ([shared (list 'placeholder)])
    (set-car! (_back-get q) item)
    ;; reinsert an enqueueing placeholder
    (set-cdr! (_back-get q) shared)
    (_back-set q shared)))

(define (q-deq! q)
  (when (q-empty? q)
    (assertion-violation "q-deq!" "q-deq! called on empty queue"))
  (let* ([data (_data-get q)]
	 [ret (car data)])
    (_data-set q (cdr data))
    ret))

(define (q-empty? q)
  (null? (cdr (_data-get q))))

;; removes every datum in the queue which is eq? to item (if any)
(define (q-remove! q item)
  (define (q-filter lst)
    (let ([first (car lst)]
	  [rest (cdr lst)])
      (if (null? rest)
	  (_back-get q)
	  (if (eq? first item)
	      (q-filter rest)
	      (cons first (q-filter rest))))))
  (_data-set q (q-filter (_data-get q))))
  
	  
