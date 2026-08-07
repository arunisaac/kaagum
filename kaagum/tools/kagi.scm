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

(define-module (kaagum tools kagi)
  #:use-module (srfi srfi-43)
  #:use-module (web client)
  #:use-module (gnu build linux-container)
  #:use-module (gnu system file-systems)
  #:use-module (lens)
  #:use-module (kaagum config)
  #:use-module (kaagum lens)
  #:use-module (kaagum tools)
  #:use-module (kaagum web)
  #:export (%kagi-api-key
            %kagi-tools))

(define %kagi-api-key
  (make-parameter #f))

(define (kagi-search query lens after-date before-date)
  "Perform a web search for @var{query} using the Kagi search engine. Apply Kagi
@var{lens} and filter for results published or updated after @var{after-date}
and for results published or updated before @var{before-date}. Use
@var{%kagi-api-key} to authenticate with the Kagi API."
  (json-post "https://kagi.com/api/v1/search"
             #:headers `((authorization
                          . ,(string-append "Bearer " (%kagi-api-key))))
             #:json `(("query" . ,query)
                      ,@(if lens
                            `(("lens_id" . ,lens))
                            '())
                      ("filters"
                       ,@(if after-date
                             `(("after" . ,after-date))
                             '())
                       ,@(if before-date
                             `(("before" . ,before-date))
                             '())))))

(define %kagi-search
  (tool #:description "Perform a web search using the Kagi search engine."
        #:parameters `(("query" . ,(tool-parameter
                                    #:type "string"
                                    #:description "Search query to run"
                                    #:required? #t))
                       ("lens" . ,(tool-parameter
                                   #:type "string"
                                   #:description
                                   "Kagi lens to apply to the search. Valid parameters and their descriptions are listed below:

academic: Searches education (.edu) domains
fediverse: Search Lemmy & kbin fediverse
forums: Search forums from around the web
pdf: Search for PDF files
programming: Search official programming language websites and forums
small_web: Results that favour non-commercial domains and topics
usenet_archive: Search into the depths of Usenet Archives and archive.org non-web collections

Default: no specific lens is used, results from all sources are included"))
                       ("after-date" . ,(tool-parameter
                                         #:type "string"
                                         #:description "Filter for results published or updated after this date (YYYY-MM-DD format). Default: no filtering by after-date"))
                       ("before-date" . ,(tool-parameter
                                          #:type "string"
                                          #:description "Filter for results published or updated before this date (YYYY-MM-DD format). Default: no filtering by before-date")))
        #:proc (lambda* (#:key query lens after-date before-date)
                 (vector-for-each (lambda (_ result)
                                    (format (current-output-port)
                                            "### [~a](~a)~%~%~a~%~%"
                                            (focus (key-ref "title") result)
                                            (focus (key-ref "url") result)
                                            (focus (key-ref "snippet") result)))
                                  (focus (in-json "data" "search")
                                         (kagi-search query lens after-date before-date))))
        #:container-mappings (const (cons (file-system-mapping
                                           (source %certificates-directory)
                                           (target (x509-certificate-directory))
                                           (writable? #f))
                                          %network-file-mappings))
        #:container-namespaces (delq 'net %namespaces)
        #:title (lambda* (#:key query lens after-date before-date)
                  (format #f "kagi-search: ~a" query))
        #:kind "search"))

(define %kagi-tools
  `(("kagi-search" . ,%kagi-search)))
