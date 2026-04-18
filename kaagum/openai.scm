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

(define-module (kaagum openai)
  #:use-module (rnrs conditions)
  #:use-module (rnrs exceptions)
  #:use-module (srfi srfi-11)
  #:use-module (web client)
  #:use-module (web http)
  #:use-module (web response)
  #:use-module (json)
  #:use-module (lens)
  #:use-module (kaagum records)
  #:export (get-api-key
            openai-query
            openai-models))

(define-public-record-type* (<model> model model?)
  (fields (name model-name)
          (description model-description)
          (context-length model-context-length lensed)))

;; TODO: URIs must not be operated on using string operations. Replace with a
;; more principled implementation involving (web uri).
(define (uri-join base uri)
  (string-append base uri))

(define* (json-request method url #:key (headers '()) body)
  "Send a HTTP @var{method} request to @var{url} with @var{body} and
additional @var{headers}. Return JSON response."
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
      ((2) (json->scm body))
      ((4)
       (raise-exception
        (condition (make-violation)
                   (make-message-condition
                    (string-append "JSON API request failed with client error code "
                                   (number->string (response-code response)))))))
      (else
       (raise-exception
        (condition (make-error)
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

;; Declare the Authorization header as opaque so that Guile doesn't try to mess
;; with it.
(declare-opaque-header! "Authorization")

(define (openai-query base-uri api-key model messages tools)
  "Send a request to the OpenAI completions API and return the JSON response.
@var{base-uri} is the base URI of the OpenAI-compatible service. @var{api-key}
is the API key for authentication. @var{model} is a supported LLM model.
@var{messages} and @var{tools} are respectively lists of JSON messages and tools
compatible with the OpenAI API specification."
  (json-post (uri-join base-uri "/v1/chat/completions")
             #:headers `((authorization
                          . ,(string-append "Bearer " api-key)))
             #:json `(("model" . ,model)
                      ;; Some providers (like Anthropic) require you to
                      ;; explicitly enable automatic prompt caching. See
                      ;; https://openrouter.ai/docs/guides/best-practices/prompt-caching
                      ("cache_control"
                       ("type" . "ephemeral")
                       ;; Anthropic offers cheaper cache writes for a ttl of 5
                       ;; minutes. But, it doesn't make a difference for an
                       ;; extended session and is not worth it for our use case.
                       ;; If we change our mind about this, we can make this a
                       ;; configurable parameter.
                       ("ttl" . "1h"))
                      ("messages" . ,(list->vector messages))
                      ("tools" . ,(list->vector tools)))))

(define (openai-models base-uri api-key)
  "Return available models. The return value is an association list mapping model
IDs to @code{<model>} objects. @var{base-uri} and @var{api-key} are the same as
in @code{openai-query}."
  (map (lambda (tree)
         (cons (focus (key-ref "id") tree)
               (model (focus (key-ref "name") tree)
                      (focus (key-ref "description") tree)
                      (focus (key-ref "context_length") tree))))
       (vector->list (focus (key-ref "data")
                            (json-get (uri-join base-uri "/v1/models")
                                      #:headers `((authorization
                                                   . ,(string-append "Bearer " api-key))))))))
