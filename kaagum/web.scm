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

(define-module (kaagum web)
  #:use-module (rnrs conditions)
  #:use-module (srfi srfi-11)
  #:use-module (web client)
  #:use-module (web http)
  #:use-module (web response)
  #:use-module (kaagum trace)
  #:use-module (json)
  #:export (uri-join
            json-request
            json-get
            json-post))

;; Declare the Authorization header as opaque so that Guile doesn't try to mess
;; with it.
(declare-opaque-header! "Authorization")

;; TODO: URIs must not be operated on using string operations. Replace with a
;; more principled implementation involving (web uri).
(define (uri-join base uri)
  (string-append base uri))

(define* (json-request method url #:key (headers '()) body)
  "Send a HTTP @var{method} request to @var{url} with @var{body} and
additional @var{headers}. Return JSON response."
  (if body
      (trace 'web "-> ~a ~a ~a" method url body)
      (trace 'web "-> ~a ~a" method url))
  (let-values (((response body)
                (http-request url
                              #:method method
                              #:headers headers
                              #:body body
                              #:streaming? #t)))
    ;; Guile does not consider application/json responses as textual, and does
    ;; not automatically set the port encoding to UTF-8.
    (set-port-encoding! body "UTF-8")
    (case (quotient (response-code response)
                    100)
      ((2) (let ((result (json->scm body)))
             (trace 'web "<- ~a ~a" (response-code response) (scm->json-string result))
             result))
      ((4)
       (trace 'web "<- ~a" (response-code response))
       (raise-exception
        (condition (make-violation)
                   (make-irritants-condition (list method url headers body))
                   (make-message-condition
                    (string-append "JSON API request failed with client error code "
                                   (number->string (response-code response)))))))
      (else
       (trace 'web "<- ~a" (response-code response))
       (raise-exception
        (condition (make-error)
                   (make-irritants-condition (list method url headers body))
                   (make-message-condition
                    (string-append "JSON API request failed with code "
                                   (number->string (response-code response))))))))))

(define* (json-get url #:key (headers '()))
  "Send a GET request to @var{url}. Return JSON response."
  (json-request 'GET url
                #:headers headers))

(define* (json-post url #:key (headers '()) json)
  "Send a POST request to @var{url} with @var{json} body and additional
@var{headers}. The @samp{Content-Type} header is set to @samp{application/json}
and need not be specified in @var{headers}. Return JSON response."
  (json-request 'POST url
                #:headers `((content-type application/json)
                            ,@headers)
                #:body (scm->json-string json)))
