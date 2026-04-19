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

(define-module (kaagum lens)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-43)
  #:use-module (lens)
  #:export (vector-nth
            in-json
            push
            add
            prepend-over
            alist-delete-over))

(define (vector-nth n)
  "Like @code{nth}, but for vectors."
  (lens (cut vector-ref <> n)
        (lambda (vec proc)
          (vector-append (vector-copy vec 0 n)
                         (vector (proc (vector-ref vec n)))
                         (vector-copy vec (1+ n))))))

(define in-json
  (case-lambda
    "Like @code{in}, but also allow integer components so that it is
possible to traverse JSON trees."
    (() (id))
    ((key . tail)
     (compose (apply in-json tail)
              (if (string? key)
                  (key-ref key)
                  (vector-nth key))))))

(define (push lens x object)
  "Cons @var{x} onto the part of @var{object} that @var{lens} focuses
on."
  (over lens (cut cons x <>) object))

(define (add lens x object)
  "Add @var{x} onto the part of @var{object} that @var{lens} focuses on."
  (over lens (cut + x <>) object))

(define (prepend-over lens lst object)
  "Prepend @var{lst} to the part of @var{object} that @var{lens} focuses
on."
  (over lens (cut append lst <>) object))

(define (alist-delete-over lens key object)
  "Delete @var{key} from the association list in @var{object} that
@var{lens} focuses on."
  (over lens (cut alist-delete key <>) object))
