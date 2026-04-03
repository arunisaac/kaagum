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

(define-module (kaakaa records)
  #:use-module (rnrs records procedural)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (lens)
  #:export (define-record-type*
            define-public-record-type*))

(define (make-record-type* record-name make-constructor . fields)
  "Create a record type with @var{record-name} and @var{fields}.
@var{make-constructor} is a function that is passed a basic record constructor
accepting positional arguments and must return the record constructor. Return
@code{length(fields) + 3} values---the record type descriptor, the record
constructor, the record predicate and the field accessors.

@var{fields} is a list of field specifiers each of which is of the form
@code{(field-name accessor-type)}. @var{accessor-type} is either
@code{'accessor} or @code{'lensed}."
  (let* ((rtd (make-record-type-descriptor record-name #f #f #f #f
                                           (list->vector (map (match-lambda
                                                                ((field-name _)
                                                                 field-name))
                                                              fields))))
         (constructor (record-constructor
                       (make-record-constructor-descriptor rtd #f #f)))
         (accessors (map (cut record-accessor rtd <>)
                         (iota (length fields)))))
    (apply values
           rtd
           (make-constructor constructor)
           (record-predicate rtd)
           (map (match-lambda*
                  ((_ accessor (_ 'accessor))
                   accessor)
                  ((index accessor (_ 'lensed))
                   (lens accessor
                         (lambda (record proc)
                           (apply constructor
                                  (append (map (lambda (accessor)
                                                 (accessor record))
                                               (take accessors index))
                                          (list (proc ((list-ref accessors index)
                                                       record)))
                                          (map (lambda (accessor)
                                                 (accessor record))
                                               (drop accessors (1+ index)))))))))
                (iota (length fields))
                accessors
                fields))))

(define-syntax define-record-type*
  (lambda (x)
    "Define a record type. All fields are immutable and may optionally have lenses as
accessors.

Lenses are procedures that combine getters and setters into one structure. They
allow you to conveniently manipulate parts of deeply nested data structures in a
composable and purely functional way.

Consider the following example record definition:
@example
(define-record-type* (<employee> employee employee?)
  (name employee-name)
  (age employee-age lensed)
  (salary employee-salary lensed))
@end example
In this example, @code{employee-name} is a regular accessor, while
@code{employee-age} and @code{employee-salary} are lenses.

@code{employee-name} is a regular accessor. Get with:
@example
(employee-name x)
@end example

@code{employee-age} is a lens. Get with:
@example
(focus employee-age x)
@end example

Functionally update with:
@example
(put employee-age 25 x)
@end example

Record definitions may also optionally specify a @code{make-constructor}
argument which is passed to @code{make-record-type*}. For example:
@example
(define-record-type* (<employee> employee employee?)
  (lambda (constructor)
    (lambda* (name #:key age salary)
      (constructor name age salary)))
  (fields (name employee-name)
          (age employee-age lensed)
          (salary employee-salary lensed)))
@end example
"
    (syntax-case x (fields)
      ((_ (record-name constructor-name predicate-name)
          make-constructor
          (fields field-spec ...))
       #`(define-values (record-name constructor-name predicate-name
                                     #,@(map (lambda (x)
                                               (syntax-case x ()
                                                 ((_ accessor-name _ ...)
                                                  #'accessor-name)))
                                             #'(field-spec ...)))
           (make-record-type* 'record-name
                              make-constructor
                              #,@(map (lambda (x)
                                        (syntax-case x (lensed)
                                          ((field-name accessor-name)
                                           #''(field-name accessor))
                                          ((field-name accessor-name lensed)
                                           #''(field-name lensed))))
                                      #'(field-spec ...)))
           ))
      ((_ (record-name constructor-name predicate-name)
          (fields field-spec ...))
       #'(define-record-type* (record-name constructor-name predicate-name)
           identity
           (fields field-spec ...))))))

(define-syntax define-public-record-type*
  (lambda (x)
    "Like @code{define-record-type*}, but also export the constructor, the predicate
and the accessors."
    (syntax-case x (fields)
      ((_ (record-name constructor-name predicate-name)
          make-constructor
          (fields field-spec ...))
       #`(begin
           (define-record-type* (record-name constructor-name predicate-name)
             make-constructor
             (fields field-spec ...))
           (export constructor-name)
           (export predicate-name)
           #,@(map (lambda (x)
                     (syntax-case x ()
                       ((_ accessor-name _ ...)
                        #'(export accessor-name))))
                   #'(field-spec ...))))
      ((_ (record-name constructor-name predicate-name)
          (fields field-spec ...))
       #'(define-public-record-type* (record-name constructor-name predicate-name)
           identity
           (fields field-spec ...))))))
