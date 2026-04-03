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

(define-module (kaakaa container)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match)
  #:use-module (gnu build linux-container)
  #:use-module (guix utils)
  #:use-module (kaakaa records)
  #:export (call-with-container*))

(define-public-record-type* (<container-result> container-result container-result?)
  (fields (output container-result-output)
          (exit-value container-result-exit-value)))

(define (call-with-container* mounts namespaces thunk)
  "Run @var{thunk} as a process in a container with @var{mounts} and
@var{namespaces}, and return a @code{<container-result>} object."
  (call-with-temporary-directory
   (lambda (root)
     (match (pipe)
       ((in . out)
        (match (waitpid (run-container root mounts namespaces 1
                                       (lambda ()
                                         (close-port in)
                                         (with-output-to-port out
                                           ;; TODO: Capture stderr too.
                                           thunk)
                                         (close-port out))))
          ((_ . status)
           (close-port out)
           (let ((result (get-string-all in)))
             (close-port in)
             (container-result result
                               (status:exit-val status))))))))))
