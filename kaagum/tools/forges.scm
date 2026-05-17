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

(define-module (kaagum tools forges)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (web client)
  #:use-module (web uri)
  #:use-module (gnu build linux-container)
  #:use-module (gnu system file-systems)
  #:use-module (lens)
  #:use-module (kaagum config)
  #:use-module (kaagum records)
  #:use-module (kaagum tools)
  #:use-module (kaagum web)
  #:export (%forge-tools))

;; Containerized tool calls in this file that make HTTPS queries only work when
;; the (gnutls) module has already been loaded and is part of the process memory
;; before forking. Fortunately, we can reasonably expect the (gnutls) module to
;; have already been loaded thanks to the LLM API requests.

(define-record-type* (<issue> issue issue?)
  (fields (number issue-number)
          (title issue-title)
          (state issue-state)
          (timeline issue-timeline)))

(define-record-type* (<author> author author?)
  (fields (handle author-handle lensed)
          (name author-name lensed)))

(define-record-type* (<post> post post?)
  (fields (author post-author lensed)
          (date post-date)
          (body post-body)))

(define (event->post event)
  "Convert Forgejo @var{event} JSON to @code{<post>} object."
  (post (author (focus (in "user" "login")
                       event)
                (focus (in "user" "full_name")
                       event))
        (string->date (focus (key-ref "created_at")
                             event)
                      "~Y-~m-~dT~H:~M:~S~z")
        (focus (key-ref "body")
               event)))

(define (build-uri* host . parts)
  (build-uri 'https
             #:host host
             #:path (string-append "/" (encode-and-join-uri-path parts))))

(define (forgejo-issue host owner repo issue-number)
  (let ((issue-json
         (json-get (build-uri* host
                               "api" "v1" "repos" owner repo "issues"
                               (number->string issue-number))))
        (timeline
         (vector->list
          (json-get (build-uri* host
                                "api" "v1" "repos" owner repo "issues"
                                (number->string issue-number)
                                "timeline")))))
    (issue (focus (key-ref "number")
                  issue-json)
           (focus (key-ref "title")
                  issue-json)
           (focus (key-ref "state")
                  issue-json)
           (cons (event->post issue-json)
                 (filter-map (lambda (event)
                               (and (string=? (focus (key-ref "type")
                                                     event)
                                              "comment")
                                    (event->post event)))
                             timeline)))))

(define (render-issue issue port)
  "Render @var{issue} as text to @var{port}."
  (format port
          "Issue #~a: ~a
State: ~a~%~%"
          (issue-number issue)
          (issue-title issue)
          (issue-state issue))
  (for-each (lambda (event)
              (format port
                      "--- ~a (~a) (~a) ---~%~a~%~%"
                      (focus (compose author-name post-author)
                             event)
                      (focus (compose author-handle post-author)
                             event)
                      (date->string (post-date event) "~1")
                      (post-body event)))
            (issue-timeline issue)))

(define %forgejo-issue
  (tool #:description "Read conversation in Forgejo issue.

For example, to read https://codeberg.org/guix/maintenance/issues/105, use the following arguments:
{
  \"host\": \"codeberg.org\",
  \"owner\": \"guix\",
  \"repo\": \"maintenance\",
  \"issue-number\": 105
}
"
        #:parameters `(("host" . ,(tool-parameter
                                   #:type "string"
                                   #:description "Host (aka domain name) of Forgejo instance. Default is \"codeberg.org\"."
                                   #:required? #t))
                       ("owner" . ,(tool-parameter
                                    #:type "string"
                                    #:description "Owner (or organization) from the issue URL"
                                    #:required? #t))
                       ("repo" . ,(tool-parameter
                                   #:type "string"
                                   #:description "Repository name from the issue URL"
                                   #:required? #t))
                       ("number" . ,(tool-parameter
                                     #:type "integer"
                                     #:description "Issue number to read"
                                     #:required? #t)))
        #:proc (lambda* (#:key host owner repo number)
                 (render-issue (forgejo-issue host owner repo number)
                               (current-output-port)))
        #:container-mappings (const (cons (file-system-mapping
                                            (source %certificates-directory)
                                            (target (x509-certificate-directory))
                                            (writable? #f))
                                          %network-file-mappings))
        #:container-namespaces (delq 'net %namespaces)
        #:title (lambda* (#:key host owner repo number)
                  (string-append "read "
                                 (uri->string (build-uri* host
                                                          owner repo "issues"
                                                          (number->string number)))))
        #:kind "read"))

(define %forge-tools
  `(("forgejo-issue" . ,%forgejo-issue)))
