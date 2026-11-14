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

(define-module (kaagum message)
  #:use-module (rnrs base)
  #:use-module (ice-9 match)
  #:use-module (lens)
  #:use-module (kaagum records)
  #:export (message->tree
            tree->message))

(define-public-record-type* (<user-message> user-message user-message?)
  (fields (content user-message-content)))

(define-public-record-type* (<assistant-message> assistant-message assistant-message?)
  (fields (content assistant-message-content)
          (tool-calls assistant-message-tool-calls)))

(define-public-record-type* (<tool-message> tool-message tool-message?)
  (fields (tool-call-id tool-message-tool-call-id)
          (content tool-message-content)))

(define (message->tree message)
  "Convert @var{message}, a @code{<user-message>}, @code{<assistant-message>} or
@code{<tool-message>} object, to a JSON tree."
  (cond
   ((user-message? message)
    `(("role" . "user")
      ("content" . ,(user-message-content message))))
   ((assistant-message? message)
    `(("role" . "assistant")
      ("content" . ,(assistant-message-content message))
      ,@(match (assistant-message-tool-calls message)
          (() '())
          (tool-calls
           `(("tool_calls" . ,(list->vector tool-calls)))))))
   ((tool-message? message)
    `(("role" . "tool")
      ("tool_call_id" . ,(tool-message-tool-call-id message))
      ("content" . ,(tool-message-content message))))
   ;; Unreachable code
   (else
    (assertion-violation message "Invalid message"))))

(define (tree->message tree)
  "Convert JSON @var{tree} to a @code{<user-message>}, @code{<assistant-message>}
or @code{<tool-message>} object."
  (let ((role (focus (key-ref "role")
                     tree)))
    (cond
     ((string=? role "user")
      (user-message (focus (key-ref "content")
                           tree)))
     ((string=? role "assistant")
      (assistant-message (focus (key-ref "content")
                                tree)
                         (cond
                          ;; We keep tool calls in their original JSON tree form
                          ;; since tree->message does not have enough
                          ;; information to parse and validate them.
                          ((focus (key-ref "tool_calls")
                                  tree)
                           => vector->list)
                          (else '()))))
     ((string=? role "tool")
      (tool-message (focus (key-ref "tool_call_id")
                           tree)
                    (focus (key-ref "content")
                           tree)))
     ;; Unreachable code
     (else
      (assertion-violation role "Invalid role")))))
