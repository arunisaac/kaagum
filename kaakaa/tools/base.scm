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
  #:use-module (rnrs exceptions)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-171)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (kaakaa tools)
  #:export (%list-files
            %base-tools))

(define (binary-file? file)
  "Return @code{#t} if @var{file} is a binary file. Else, return @code{#f}."
  ;; We use the following heuristic: If there are character decoding errors in
  ;; the first 10K characters, we assume that this is a binary file.
  (guard (c ((or (i/o-decoding-error? c)
                 (eq? (exception-kind c)
                      'decoding-error))
             #t))
    (call-with-input-file file
      (lambda (port)
        (set-port-conversion-strategy! port 'error)
        (get-string-n port (* 10 1024))))
    #f))

(define (make-regexp* pattern)
  "Like @code{make-regexp}, but report an error and abort if @var{pattern} is not a
valid regular expression."
  (guard (c ((eq? (exception-kind c)
                  'regular-expression-syntax)
             (format (current-output-port)
                     "Error: Invalid regular expression: ~s~%"
                     pattern)
             (exit #f)))
    (make-regexp pattern)))

(define (files-recursively directory pattern)
  "Return a list of all files recursively down @var{directory} whose basename
matches regular expression @var{pattern}. Hidden directories are not traversed."
  (cond
   ((not (file-exists? directory))
    (format (current-output-port)
            "Error: Directory ~a does not exist~%"
            directory)
    (exit #f))
   ((not (eq? (stat:type (stat directory))
              'directory))
    (format (current-output-port)
            "Error: ~a is not a directory~%"
            directory)
    (exit #f))
   (else
    (let ((pattern-rx (make-regexp* pattern)))
      (file-system-fold (lambda (path stat result)
                          (not (string-prefix? "." (basename path))))
                        (lambda (path stat result)
                          (if (regexp-exec pattern-rx (basename path))
                              (cons path result)
                              result))
                        (lambda (path stat result)
                          result)
                        (lambda (path stat result)
                          result)
                        (lambda (path stat result)
                          result)
                        (lambda (path stat errno result)
                          (format (current-output-port)
                                  "Error: ~a: ~a~%"
                                  path
                                  (strerror errno))
                          result)
                        (list)
                        (canonicalize-path directory))))))

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
                 (cond
                  ((not (file-exists? path))
                   (format (current-output-port)
                           "Error: File ~a does not exist~%"
                           path)
                   (exit #f))
                  ((binary-file? path)
                   (format (current-output-port)
                           "Error: File ~a is binary, not text~%"
                           path)
                   (exit #f))
                  (else
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
                          <>)))))
        #:title (lambda* (#:key path (start-line 1) end-line)
                  (format #f "read ~a:~a-~a"
                          path start-line (or end-line "")))
        #:kind (const "read")))

(define %list
  (tool #:description "List files recursively.

The output is in three columns—the file type, the file size and the file path."
        #:parameters `(("root" . ,(tool-parameter
                                   #:type "string"
                                   #:description "Root path to list from"
                                   #:required? #t))
                       ("pattern" . ,(tool-parameter
                                      #:type "string"
                                      #:description
                                      "POSIX extended regular expression to match basename (including extension) of
file against. Default matches all files.

For example, to match all scheme (.scm) files, use \"\\.scm$\"")))
        #:proc (lambda* (#:key root (pattern "."))
                 (for-each (lambda (path)
                             (let ((st (stat path)))
                               (format (current-output-port)
                                       "~a~/~a~/~a~%"
                                       (stat:type st)
                                       (stat:size st)
                                       path)))
                           (files-recursively root pattern)))
        #:title (lambda* (#:key root pattern)
                  (if pattern
                      (format #f "list ~s in ~a" pattern root)
                      (format #f "list ~a" root)))
        #:kind (const "search")))

(define %search
  (tool #:description "Print lines that match a pattern (similar to grep)

Similar to grep, the output is three colon separated columns—the file path, the
line number and the matching line. Line numbers start from 1."
        #:parameters `(("pattern" . ,(tool-parameter
                                      #:type "string"
                                      #:description "POSIX extended regular expression to search for"
                                      #:required? #t))
                       ("files-root" . ,(tool-parameter
                                         #:type "string"
                                         #:description "Root path to start search from"
                                         #:required? #t))
                       ("files-pattern" . ,(tool-parameter
                                            #:type "string"
                                            #:description
                                            "POSIX extended regular expression to match basename (including extension) of
file against. Default matches all files.

For example, to match all scheme (.scm) files, use \"\\.scm$\"")))
        #:proc (lambda* (#:key pattern files-root (files-pattern "."))
                 (let* ((pattern-rx (make-regexp* pattern))
                        (line-matcher (match-lambda
                                        ((_ . line)
                                         (regexp-exec pattern-rx line))))
                        (make-line-logger (lambda (file)
                                            (match-lambda*
                                              ((result (line-number . line))
                                               (format (current-output-port)
                                                       "~a:~a:~a~%"
                                                       file line-number line))))))
                   (for-each (lambda (file)
                               (call-with-input-file file
                                 (cut port-transduce
                                      (compose (tenumerate 1)
                                               (tfilter line-matcher)
                                               (tlog (make-line-logger file)))
                                      (const #t)
                                      get-line
                                      <>)))
                             (remove binary-file?
                                     (files-recursively files-root files-pattern)))))
        #:title (lambda* (#:key pattern files-root files-pattern)
                  (if files-pattern
                      (format #f "shortlist files matching ~s in ~a, then search for lines matching ~s"
                              files-pattern files-root pattern)
                      (format #f "search for lines matching ~s in files under ~a"
                              pattern files-root)))
        #:kind (const "search")))

(define %base-tools
  `(("read" . ,%read)
    ("list" . ,%list)
    ("search" . ,%search)))

;; TODO: Implement write.
