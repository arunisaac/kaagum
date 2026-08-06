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

(define-module (kaagum trace)
  #:export (%trace-port
            trace))

(define %trace-port
  (make-parameter #f))

(define (trace subsystem message . args)
  "Print out tracing @var{message} with @var{args} for @var{subsystem}."
  (when (%trace-port)
    (display subsystem (%trace-port))
    (display ": " (%trace-port))
    (apply format (%trace-port) message args)
    (newline (%trace-port))
    ;; fsync the trace port to ensure that the trace is written to disk; just
    ;; preparing for the worst-case scenario where kaagum crashes or is killed
    ;; before the trace can reach the disk.
    (fsync (%trace-port))))
