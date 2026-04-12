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

(define-module (kaakaa tools base)
  #:use-module (rnrs io ports)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-171)
  #:use-module (kaakaa tools)
  #:export (%list-files
            %base-tools))

(define %read
  (tool #:description "Read whole text file, or optionally a subset of its lines.

Line numbers start from 1. Output is the raw file contents without line numbers."
        #:parameters `(("path" . ,(tool-parameter
                                   #:type "string"
                                   #:description "File path to read"
                                   #:required? #t))
                       ("start-line" . ,(tool-parameter
                                         #:type "integer"
                                         #:description "Read file starting from this line number (inclusive). Default is 1."))
                       ("end-line" . ,(tool-parameter
                                       #:type "integer"
                                       #:description "Read up to this line number (inclusive). Default is the last line of the file.")))
        #:proc (lambda* (#:key path (start-line 1) end-line)
                 (call-with-input-file path
                   (cut port-transduce
                        (compose (tdrop (1- start-line))
                                 (if end-line
                                     (ttake (- end-line (1- start-line)))
                                     (tmap identity))
                                 (tlog (lambda (result input)
                                         (display input)
                                         (newline))))
                        (const #t)
                        get-line
                        <>)))
        #:title (lambda* (#:key path (start-line 1) end-line)
                  (format #f "read ~a:~a-~a"
                          path start-line (or end-line "")))
        #:kind (const "read")))

(define %list-files
  (tool #:description "List files in current directory"
        #:parameters (list)
        #:proc (lambda _
                 (for-each (lambda (file)
                             (display file)
                             (newline))
                           (find-files (getcwd))))
        #:title (const "List files")
        #:kind (const "read")))

(define %base-tools
  `(("read" . ,%read)
    ("list-files" . ,%list-files)))

;; TODO: Implement write, grep and find.
