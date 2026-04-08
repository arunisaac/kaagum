;;; kaakaa --- Tiny, security-focused AI agent in Guile
;;; Copyright © 2026 Arun Isaac <arunisaac@systemreboot.net>
;;;
;;; This file is part of kaakaa.
;;;
;;; kaakaa is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; kaakaa is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with kaakaa.  If not, see <https://www.gnu.org/licenses/>.

(define-module (kaakaa utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:export (->
            foldn
            alist->plist
            call-with-input-pipe))

(define (->-helper x . procs)
  "Thread @var{x} through @var{procs}."
  (match procs
    (() x)
    ((head tail ...)
     (apply ->-helper (head x) tail))))

(define-syntax-rule (-> x (proc ...) ...)
  "Thread @var{x} through @var{procs}.

For example:
(-> 1
    (1+ <>)
    (* 2 <>)
    (expt <> 2))
=> 16"
  (->-helper x (cut proc ...) ...))

(define (foldn proc lst . inits)
  "Apply @var{proc} to the elements of @var{lst} to build a result, and return
that result. @var{proc} may return multiple values, in which case, an equal
number of values are returned. Each @var{proc} call is @code{(proc element
previous ...)} where @code{element} is an element of @var{lst}, and
@code{(previous ...)} is the return from the previous call to @var{proc} or the
given @var{inits} for the first call. For example,

(foldn (lambda (n sum sum-of-squares)
         (values (+ sum n)
                 (+ sum-of-squares (expt n 2))))
       (iota 10)
       0 0)
=> 45
=> 285"
  (apply values
         (fold (lambda (element results)
                 (call-with-values (cut apply proc element results) list))
               inits
               lst)))

(define (alist->plist alist)
  "Convert association list @var{alist} to a property list. Keys in
@var{alist} are converted to keywords."
  (append-map (match-lambda
                ((key . value)
                 (list (symbol->keyword (string->symbol key))
                       value)))
              alist))

(define (call-with-input-pipe command proc)
  "Call @var{proc} with input pipe to @var{command}. @var{command} is a
list of program arguments."
  (match command
    ((prog args ...)
     (let ((port #f))
       (dynamic-wind
         (lambda ()
           (set! port (apply open-pipe* OPEN_READ prog args)))
         (cut proc port)
         (lambda ()
           (unless (zero? (close-pipe port))
             (error "Command invocation failed" command))))))))
