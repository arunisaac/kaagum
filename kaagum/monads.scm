;;; kaagum --- Tiny, security-focused AI agent in Guile
;;; Copyright © 2026 Arun Isaac <arunisaac@systemreboot.net>
;;;
;;; This file is part of kaagum.
;;;
;;; kaagum is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; kaagum is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with kaagum.  If not, see <https://www.gnu.org/licenses/>.

(define-module (kaagum monads)
  #:use-module (rnrs records syntactic)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (state-bind
            state-return
            state-let*
            state-begin
            state-when
            state-sequence
            state-map
            state-append-map
            current-state
            set-current-state
            run-with-state))

(define-syntax mlet*
  (syntax-rules ()
    ((_ bind return () body ...)
     (begin
       body ...))
    ((_ bind return ((var mvalue) other-bindings ...) body ...)
     (bind mvalue
           (lambda (var)
             (mlet* bind return (other-bindings ...)
                    body ...))))))

(define-syntax mbegin
  (syntax-rules ()
    ((_ bind return expression)
     expression)
    ((_ bind return first-expression body ...)
     (bind first-expression
           (lambda _
             (mbegin bind return
                     body ...))))))

(define-syntax-rule (mwhen bind return condition body ...)
  "When @var{condition} is true, evaluate @var{body} in monad. The monadic return
value must be ignored. @var{bind} and @var{return} describe the monad in
question."
  (if condition
      (mbegin bind return body ...)
      (return #f)))

(define-syntax-rule (sequence bind return mvalues)
  "Convert a list of monadic @var{mvalues} into a monadic list
of values. @var{bind} and @var{return} describe the monad in question."
  (mlet* bind return ((reverse-list
                       (fold (lambda (mvalue mresult)
                               (mlet* bind return ((result mresult)
                                                   (value mvalue))
                                      (return (cons value result))))
                             (return (list))
                             mvalues)))
         (return (reverse reverse-list))))

(define-syntax-rule (mmap bind return mproc lists ...)
  "Map monadic funcion @var{mproc} over @var{lists} and return a monadic
list. @var{bind} and @var{return} describe the monad in question."
  (sequence bind return (map mproc lists ...)))

(define-syntax-rule (mappend-map bind return mproc lists ...)
  "Map monadic funcion @var{mproc} over @var{lists} like
@code{mmap}, but return a monadic list of the results appended together.
@var{bind} and @var{return} describe the monad in question."
  (mlet* bind return ((mapped (mmap bind return mproc lists ...)))
         (return (apply append mapped))))

(define-record-type (<stateful-value> stateful-value stateful-value?)
  (fields (immutable state stateful-value-state)
          (immutable value stateful-value-value)))

;; We force inlining so that the source location of state-bind calls is
;; preserved correctly. FIXME: It would be preferable to capture the source
;; location and somehow write it into the metadata of the lambda function.
(define-inlinable (state-bind mvalue mproc)
  (lambda (state)
    (match (mvalue state)
      (($ <stateful-value> next-state value)
       ((mproc value) next-state)))))

(define-inlinable (state-return value)
  (cut stateful-value <> value))

(define-syntax-rule (state-let* bindings body ...)
  (mlet* state-bind state-return bindings
    body ...))

(define-syntax-rule (state-begin body ...)
  (mbegin state-bind state-return
    body ...))

(define-syntax-rule (state-when condition body ...)
  (mwhen state-bind state-return condition
    body ...))

(define-inlinable (state-sequence mvalues)
  (sequence state-bind state-return mvalues))

(define-syntax-rule (state-map mproc lists ...)
  (mmap state-bind state-return mproc lists ...))

(define-syntax-rule (state-append-map mproc lists ...)
  (mappend-map state-bind state-return mproc lists ...))

(define-inlinable (current-state)
  "Return the current state as a state-monadic value."
  (lambda (state)
    (stateful-value state state)))

(define-inlinable (set-current-state new-state)
  "Set @var{new-state} as the state. The monadic return value must be ignored."
  (lambda _
    (stateful-value new-state #t)))

(define (run-with-state mvalue initial-state)
  "Run state-monadic value @var{mvalue} starting with @var{initial-state}. Return
two values---the value encapsulated in @var{mvalue} and the final state."
  (match (mvalue initial-state)
    (($ <stateful-value> state value)
     (values value state))))
