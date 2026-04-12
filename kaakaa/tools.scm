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

(define-module (kaakaa tools)
  #:use-module ((rnrs base) #:select (assertion-violation))
  #:use-module (rnrs conditions)
  #:use-module (rnrs exceptions)
  #:use-module (rnrs records syntactic)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (gnu build linux-container)
  #:use-module (gnu system file-systems)
  #:use-module (json)
  #:use-module (lens)
  #:use-module (kaakaa container)
  #:use-module (kaakaa records)
  #:use-module (kaakaa utils)
  #:export (tool-call-parse-failure?
            tool-call-parse-failure-message

            tool->spec
            spec->tool-call
            eval-tool-call))

(define (readonly-cwd-only-mappings cwd arguments)
  (list (file-system-mapping
          (source cwd)
          (target source)
          (writable? #f))))

(define-public-record-type* (<tool> tool tool?)
  (lambda (constructor)
    (lambda* (#:key
              description (parameters '()) proc
              (container-mappings readonly-cwd-only-mappings)
              (container-namespaces %namespaces)
              title kind)
      (unless description
        (raise-exception
         (condition (make-violation)
                    (make-message-condition "tool requires description"))))
      (unless proc
        (raise-exception
         (condition (make-violation)
                    (make-message-condition "tool requires proc"))))
      (constructor description parameters proc
                   container-mappings container-namespaces title kind)))
  (fields (description tool-description)
          ;; Association list mapping parameter names to <tool-parameter>
          ;; objects.
          (parameters tool-parameters)
          (proc tool-proc)
          (container-mappings tool-container-mappings)
          (container-namespaces tool-container-namespaces)
          (title tool-title)
          (kind tool-kind)))

(define-public-record-type* (<tool-parameter> tool-parameter tool-parameter?)
  (lambda (constructor)
    (lambda* (#:key type description required?)
      (unless type
        (raise-exception
         (condition (make-violation)
                    (make-message-condition "tool parameter requires type"))))
      (unless description
        (raise-exception
         (condition (make-violation)
                    (make-message-condition "tool parameter requires description"))))
      (constructor type description required?)))
  (fields (type tool-parameter-type)
          (description tool-parameter-description)
          (required? tool-parameter-required?)))

(define-public-record-type* (<array-type> array-type array-type?)
  (fields (subtype array-type-subtype)))

(define (tool->spec name tool)
  "Serialize @var{tool} of @var{name} to OpenAI-compatible JSON spec."
  `(("type" . "function")
    ("function"
     ("name" . ,name)
     ("description" . ,(tool-description tool))
     ("parameters"
      ("type" . "object")
      ("properties" .
       ,(map (match-lambda
               ((name . parameter)
                ;; TODO: Check if the OpenAI API supports arrays of arrays and
                ;; other more deeply nested types.
                (let ((type (tool-parameter-type parameter)))
                  `(,name
                    ("description" . ,(tool-parameter-description parameter))
                    ("type" . ,(if (array-type? type)
                                   "array"
                                   type))
                    ,@(if (array-type? type)
                          `(("items" . ,(array-type-subtype type)))
                          '())))))
             (tool-parameters tool)))
      ("required" .
       ,(list->vector
         (filter-map (match-lambda
                       ((name . parameter)
                        (and (tool-parameter-required? parameter)
                             name)))
                     (tool-parameters tool))))))))

(define-public-record-type* (<tool-call> tool-call tool-call?)
  (fields (session-id tool-call-session-id)
          (session-cwd tool-call-session-cwd)
          (id tool-call-id)
          (function tool-call-function)
          (arguments tool-call-arguments)
          ;; One of 'pending-approval, 'approved, 'rejected or 'cancelled
          (status tool-call-status lensed)))

(define-public-record-type* (<tool-call-result> tool-call-result tool-call-result?)
  (fields (session-id tool-call-result-session-id)
          (call-id tool-call-result-call-id)
          (title tool-call-result-title)
          (kind tool-call-result-kind)
          (arguments tool-call-result-arguments)
          (json tool-call-result-json)
          (success? tool-call-result-success?)))

(define-condition-type &tool-call-parse-failure &serious
  tool-call-parse-failure tool-call-parse-failure?
  (message tool-call-parse-failure-message))

(define-condition-type &tool-call-failure &serious
  tool-call-failure tool-call-failure?
  (message tool-call-failure-message))

(define (spec->tool-call session-id session-cwd tools allowed-tools rejected-tools spec)
  "Deserialize JSON tool call @var{spec} into a @code{<tool-call>}
object. Raise a @code{&tool-call-parse-failure} condition if deserialization
fails.

@var{session-id} and @var{session-cwd} are the ID and current working directory
of the session the tool call pertains to. @var{tools} is an association list
mapping the names of all available tools to their respective @code{<tool>}
objects. @var{allowed-tools} and @var{rejected-tools} are the lists of tool
names that have been respectively allowed and rejected by the user in advance."
  ;; TODO: Assert that type is function, and do more sanitization.
  (let* ((args (guard (c (else
                          (raise-exception
                           (tool-call-parse-failure
                            "Error: Arguments are not valid JSON"))))
                 (json-string->scm (focus (in "function" "arguments")
                                          spec))))
         (name (focus (in "function" "name")
                      spec))
         (tool (cond
                ((focus (key-ref name) tools)
                 => identity)
                (else
                 (raise-exception
                  (tool-call-parse-failure
                   (string-append "Error: Function " name " does not exist")))))))
    (tool-call session-id
               session-cwd
               (focus (key-ref "id")
                      spec)
               name
               ;; Only pick out valid arguments, and error out if any required
               ;; arguments are missing.
               (filter-map (match-lambda
                             ((arg-name . parameter)
                              (cond
                               ((assoc arg-name args)
                                => identity)
                               (else
                                (and (tool-parameter-required? parameter)
                                     (raise-exception
                                      (tool-call-failure
                                       (string-append "Error: Missing required argument "
                                                      arg-name))))))))
                           (tool-parameters tool))
               ;; Set tool call status based on pre-approved and pre-rejected
               ;; tools.
               (cond
                ((member name allowed-tools) 'approved)
                ((member name rejected-tools) 'rejected)
                (else 'pending-approval)))))

(define (eval-tool-call tool-call tools)
  "Evaluate @var{tool-call} and return a @code{<tool-call-result>}
object. @var{tools} is an association list mapping the names of all
available tools to their respective @code{<tool>} objects."
  (let ((tool (focus (key-ref (tool-call-function tool-call))
                     tools)))
    (case (focus tool-call-status tool-call)
      ;; User approved tool call.
      ((approved)
       (guard (c ((tool-call-failure? c)
                  (tool-call-result (tool-call-session-id tool-call)
                                    (tool-call-id tool-call)
                                    #f
                                    #f
                                    (tool-call-arguments tool-call)
                                    `(("role" . "tool")
                                      ("tool_call_id" . ,(tool-call-id tool-call))
                                      ("content" . ,(tool-call-failure-message c)))
                                    #f)))
         (let* ((args (alist->plist (tool-call-arguments tool-call)))
                ;; Actually evaluate tool call.
                (tool-result
                 (call-with-container*
                     (map file-system-mapping->bind-mount
                          ((tool-container-mappings tool)
                           (tool-call-session-cwd tool-call)
                           args))
                     (tool-container-namespaces tool)
                   (lambda ()
                     (chdir (tool-call-session-cwd tool-call))
                     (apply (tool-proc tool)
                            args)))))
           ;; Build result.
           (tool-call-result (tool-call-session-id tool-call)
                             (tool-call-id tool-call)
                             (apply (tool-title tool) args)
                             (apply (tool-kind tool) args)
                             (tool-call-arguments tool-call)
                             `(("role" . "tool")
                               ("tool_call_id" . ,(tool-call-id tool-call))
                               ("content" . ,(container-result-output tool-result)))
                             (zero? (container-result-exit-value tool-result))))))
      ;; User cancelled or rejected tool call.
      ((cancelled rejected)
       (tool-call-result (tool-call-session-id tool-call)
                         (tool-call-id tool-call)
                         #f
                         #f
                         (tool-call-arguments tool-call)
                         `(("role" . "tool")
                           ("tool_call_id" . ,(tool-call-id tool-call))
                           ("content" .
                            ,(case (focus tool-call-status tool-call)
                               ((rejected)
                                "Error: User denied permission for this tool call")
                               ((cancelled)
                                "Error: User cancelled this tool call"))))
                         #f))
      (else
       (assertion-violation (focus tool-call-status tool-call)
                            "Invalid tool call status")))))
