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

(define-module (kaagum container)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match)
  #:use-module (gnu build linux-container)
  #:use-module (guix utils)
  #:use-module (kaagum records)
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
        (let ((child-pid (run-container root mounts namespaces 1
                                        (lambda ()
                                          (close-port in)
                                          (with-output-to-port out
                                            ;; TODO: Capture stderr too.
                                            thunk)
                                          (close-port out)))))
          (close-port out)
          ;; We read the child's output and then wait for the child to exit. It
          ;; is important to do it in this order. If we first wait for the child
          ;; to exit, the pipe buffer could fill up and the child's writes could
          ;; be blocked; we'd have a deadlock.
          (let ((result (get-string-all in)))
            (close-port in)
            (container-result result
                              (match (waitpid child-pid)
                                ((_ . status)
                                 (status:exit-val status)))))))))))
