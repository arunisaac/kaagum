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

(define-module (kaakaa tea)
  #:use-module (rnrs exceptions)
  #:use-module (rnrs io ports)
  #:use-module (rnrs records syntactic)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (json)
  #:use-module (lens)
  #:use-module (kaakaa lens)
  #:use-module (kaakaa openai)
  #:use-module (kaakaa records)
  #:use-module (kaakaa tools)
  #:use-module (kaakaa utils)
  #:export (initial-state
            tea-loop))

(define %tool-allow-once
  '(("optionId" . "allow-once")
    ("name" . "Allow once")
    ("kind" . "allow_once")))

(define %tool-reject-once
  '(("optionId" . "reject-once")
    ("name" . "Reject once")
    ("kind" . "reject_once")))

(define-record-type* (<session> session session?)
  (lambda (constructor)
    (lambda* (cwd #:key cancelling? (messages '()) (pending-tool-calls '()))
      (constructor cwd cancelling? messages pending-tool-calls)))
  (fields (cwd session-cwd lensed)
          (cancelling? session-cancelling? lensed)
          (messages session-messages lensed)
          (tool-calls session-tool-calls lensed)))

(define-record-type* (<state> state state?)
  (fields (client-request-id state-client-request-id lensed)
          (agent-request-id state-agent-request-id lensed)
          (next-session-id state-next-session-id lensed)
          ;; Association list mapping agent request IDs to tool calls for which
          ;; permission is sought
          (requests-alist state-requests-alist lensed)
          (sessions state-sessions lensed)))

(define (initial-state)
  (state #f 0 0 '() '()))

(define-record-type* (<llm-request> llm-request llm-request?)
  (fields (session-id llm-request-session-id)
          (messages llm-request-messages)))

(define-record-type* (<llm-response> llm-response llm-response?)
  (fields (session-id llm-response-session-id)
          (json llm-response-json)))

(define-record-type* (<acp-message> acp-message acp-message?)
  (fields (json acp-message-json)))

(define-record-type* (<command> command command?)
  (fields (description command-description)
          (next-state command-next-state)))

(define (command->spec name command)
  "Serialize @var{command} of @var{name} to ACP-compatible JSON spec."
  `(("name" . ,name)
    ("description" . ,(command-description command))))

(define (agent-message-chunk session-id text)
  "Return an @samp{agent_message_chunk} @samp{session/update} ACP message for
@var{session-id} with @var{text}."
  (acp-message `(("jsonrpc" . "2.0")
                 ("method" . "session/update")
                 ("params"
                  ("sessionId" . ,session-id)
                  ("update"
                   ("sessionUpdate" . "agent_message_chunk")
                   ("content"
                    ("type" . "text")
                    ("text" . ,text)))))))

(define %cwd-command
  (command "Print current working directory of the session"
           (lambda (state session-id _)
             (values state
                     (list (agent-message-chunk session-id
                                                (focus (compose session-cwd
                                                                (key-ref session-id)
                                                                state-sessions)
                                                       state)))))))

(define %commands
  `(("cwd" . ,%cwd-command)))

(define (state-messages session-id)
  "Return a lens to focus on messages of session with @var{session-id} in state."
  (compose session-messages
           (key-ref session-id)
           state-sessions))

(define (state-tool-calls session-id)
  "Return a lens to focus on tool calls of session with @var{session-id} in state."
  (compose session-tool-calls
           (key-ref session-id)
           state-sessions))

(define (state-tool-call tool-call-id session-id)
  "Return a lens to focus on tool call with @var{tool-call-id} of session with
@var{session-id} in state."
  (compose (key-ref tool-call-id)
           (state-tool-calls session-id)))

(define (state-session-cancelling? session-id)
  "Return a lens to focus on the @code{cancelling?} flag of session with
@var{session-id} in state."
  (compose session-cancelling?
           (key-ref session-id)
           state-sessions))

(define (state->llm-requests session-id state)
  "Return a list of @code{<llm-request>} objects for session with @var{session-id}
in @var{state}."
  (if (and (null? (focus (state-tool-calls session-id)
                         state))
           (not (focus (state-session-cancelling? session-id)
                       state)))
      ;; There are no more tool calls in flight and a cancellation is not in
      ;; progress; dispatch to LLM.
      (list (llm-request session-id
                         (map (lambda (message)
                                ;; Strip out all fields (such as reasoning
                                ;; fields) other than role, content and
                                ;; tool_calls.
                                (filter (match-lambda
                                          ((key . _)
                                           (member key (list "role" "content"
                                                             "tool_calls"))))
                                        message))
                              ;; Reverse because we have been prepending new
                              ;; messages onto the list.
                              (reverse (focus (state-messages session-id)
                                              state)))))
      ;; There are tool calls or a cancellation in progress; do nothing.
      (list)))

(define (next-state-slash-command state session-id command-name argument)
  "Given current @var{state} and an invocation of slash var{command-name} with
@var{argument} for @var{session-id}, return the next state and a list of
effects."
  (cond
   ;; command exists
   ((focus (key-ref command-name)
           %commands)
    => (lambda (command)
         ((command-next-state command) state session-id argument)))
   ;; command not found
   (else
    (values state
            (list (agent-message-chunk session-id
                                       "Error: Unknown command"))))))

(define (next-state-client-request state request)
  "Given current @var{state} and a new ACP @var{request}, return the next state and
a list of effects."
  (let ((request-id (focus (key-ref "id")
                           request)))
    (cond
     ;; There is a pending request from the client; process it.
     ((focus (key-ref "method") request)
      => (match-lambda
           ("initialize"
            (values state
                    (list (acp-message `(("jsonrpc" . "2.0")
                                         ("id" . ,request-id)
                                         ("result"
                                          ("protocolVersion" . 1)
                                          ("agentCapabilities")
                                          ("agentInfo"
                                           ("name" . "kaakaa")
                                           ("title" . "Kaakaa")
                                           ("version" . "0.1.0"))
                                          ("authMethods" . #())))))))
           ("session/new"
            (let ((session-id
                   (string-append "session-"
                                  (number->string
                                   (focus state-next-session-id state)))))
              (values (-> state
                          ;; Push new session onto list.
                          (push state-sessions
                                (cons session-id
                                      ;; TODO: Check if cwd is an absolute path.
                                      (session (focus (in "params" "cwd")
                                                      request)))
                                <>)
                          ;; Increment next session ID.
                          (over state-next-session-id
                                1+
                                <>))
                      (map acp-message
                           `(;; Return new session.
                             (("jsonrpc" . "2.0")
                              ("id" . ,request-id)
                              ("result"
                               ("sessionId" . ,session-id)))
                             ;; Advertise available commands.
                             (("jsonrpc" . "2.0")
                              ("method" . "session/update")
                              ("params"
                               ("sessionId" . ,session-id)
                               ("update"
                                ("sessionUpdate" . "available_commands_update")
                                ("availableCommands" .
                                 ,(list->vector (map (match-lambda
                                                       ((name . command)
                                                        (command->spec name command)))
                                                     %commands)))))))))))
           ("session/prompt"
            (let ((session-id (focus (in "params" "sessionId")
                                     request))
                  ;; TODO: Filter to only allow "text" type content blocks.
                  (prompt (focus (in "params" "prompt")
                                 request)))
              (cond
               ;; slash command
               ((string-match "^/([a-z0-9]*)(.*)"
                              (focus (in-json 0 "text")
                                     prompt))
                => (lambda (mtch)
                     (let ((command-name (match:substring mtch 1))
                           (argument (string-trim (match:substring mtch 2))))
                       (let-values (((state effects)
                                     (next-state-slash-command
                                      state session-id command-name argument)))
                         (values state
                                 ;; End prompt turn immediately. This means
                                 ;; slash commands cannot send LLM requests or
                                 ;; initiate other exchanges.
                                 (append effects
                                         (list (acp-message
                                                `(("jsonrpc" . "2.0")
                                                  ("id" . ,request-id)
                                                  ("result"
                                                   ("stopReason" . "end_turn")))))))))))
               ;; regular prompt
               (else
                (let ((state (push (state-messages session-id)
                                   `(("role" . "user")
                                     ("content" . ,prompt))
                                   state)))
                  (values state
                          (state->llm-requests session-id state)))))))
           ("session/cancel"
            (let ((session-id (focus (in "params" "sessionId")
                                     request)))
              ;; Reset state and end the prompt turn.
              (values (-> state
                          (put (state-session-cancelling? session-id)
                               #f
                               <>)
                          (put (state-tool-calls session-id)
                               '()
                               <>)
                          (put state-requests-alist
                               '()
                               <>))
                      (list (acp-message `(("jsonrpc" . "2.0")
                                           ("id" . ,(focus state-client-request-id
                                                           state))
                                           ("result"
                                            ("stopReason" . "cancelled")))))))))))))

(define (next-state-client-response state response)
  "Given current @var{state} and a new ACP @var{response}, return the next state
and a list of effects."
  (let ((request-id (focus (key-ref "id")
                           response)))
    (cond
     ;; The tool call for this response has been found; process it.
     ((focus (compose (key-ref request-id)
                      state-requests-alist)
             state)
      => (lambda (tool-call)
           (let* ((session-id (tool-call-session-id tool-call))
                  (outcome (focus (in "result" "outcome")
                                  response))
                  (outcome-type (focus (key-ref "outcome")
                                       outcome))
                  (state
                   (-> state
                       ;; Update tool status.
                       (put (compose tool-call-status
                                     (state-tool-call (tool-call-id tool-call)
                                                      session-id))
                            ;; TODO: Implement other tool permissions.
                            (cond
                             ((string=? outcome-type "cancelled")
                              'cancelled)
                             ((and (string=? outcome-type "selected")
                                   (string=? (focus (key-ref "optionId")
                                                    outcome)
                                             (focus (key-ref "optionId")
                                                    %tool-allow-once)))
                              'approved)
                             (else 'rejected))
                            <>)
                       ;; If the tool call was cancelled, set the cancelling
                       ;; flag to indicate that a cancellation is in progress.
                       (put (state-session-cancelling? session-id)
                            (string=? outcome-type "cancelled")
                            <>)
                       ;; Unregister request corresponding to this response.
                       (alist-delete-over state-requests-alist
                                          request-id
                                          <>))))
             (values state
                     ;; Request tool call evaluation. eval-tool-call handles
                     ;; cancelled and rejected tool calls correctly. We don't
                     ;; have to worry about it here.
                     (list (focus (state-tool-call (tool-call-id tool-call)
                                                   (tool-call-session-id tool-call))
                                  state))))))
     ;; Client response is stale (it pertains to a request not in
     ;; requests-alist). Silently ignore it.
     (else (values state '())))))

(define (next-state-tool-call state session-id call-json tools)
  "Given current @var{state} and a new tool @var{call-json}, return the next state
and a list of effects.

@var{tools} is the same as in @code{tea-loop}."
  (guard (c ((tool-call-parse-failure? c)
             (let ((call-id (focus (key-ref "id") call-json)))
               (values (-> state
                           ;; Push tool call response onto messages.
                           (push (state-messages session-id)
                                 `(("role" . "tool")
                                   ("tool_call_id" . ,call-id)
                                   ("content" . ,(tool-call-parse-failure-message c)))
                                 <>))
                       ;; Notify client about invalid tool call.
                       (list (acp-message `(("jsonrpc" . "2.0")
                                            ("method" . "session/update")
                                            ("params"
                                             ("sessionId" . ,session-id)
                                             ("update"
                                              ("sessionUpdate" . "tool_call_update")
                                              ("toolCallId" . ,call-id)
                                              ("status" . "failed"))))))))))
    (let ((call (spec->tool-call session-id
                                 (focus (compose session-cwd
                                                 (key-ref session-id)
                                                 state-sessions)
                                        state)
                                 tools
                                 call-json))
          (request-id (focus state-agent-request-id state)))
      (values (-> state
                  ;; Queue tool call.
                  (push (state-tool-calls session-id)
                        (cons (tool-call-id call)
                              call)
                        <>)
                  ;; Register this request so we can recall the details later
                  ;; when the response comes in.
                  (push state-requests-alist
                        (cons request-id call)
                        <>)
                  ;; Bump agent request ID.
                  (over state-agent-request-id
                        1+
                        <>))
              (let ((tool (focus (key-ref (tool-call-function call))
                                 tools))
                    (args (alist->plist (tool-call-arguments call))))
                (map acp-message
                     `(;; Notify client about new tool call.
                       (("jsonrpc" . "2.0")
                        ("method" . "session/update")
                        ("params"
                         ("sessionId" . ,session-id)
                         ("update"
                          ("sessionUpdate" . "tool_call")
                          ("toolCallId" . ,(tool-call-id call))
                          ("title" . ,(apply (tool-title tool) args))
                          ("kind" . ,(apply (tool-kind tool) args))
                          ("rawInput" . ,(tool-call-arguments call))
                          ("status" . "pending"))))
                       ;; Request permission from the client.
                       (("jsonrpc" . "2.0")
                        ("id" . ,request-id)
                        ("method" . "session/request_permission")
                        ("params"
                         ("sessionId" . ,session-id)
                         ("toolCall"
                          ("toolCallId" . ,(tool-call-id call)))
                         ("options" .
                          ,(vector %tool-allow-once
                                   %tool-reject-once)))))))))))

(define (next-state-llm-response state response tools)
  "Given current @var{state} and a new LLM @var{response}, return the next state
and a list of effects.

@var{tools} is the same as in @code{tea-loop}."
  (let* ((session-id (llm-response-session-id response))
         (llm-reply (focus (in-json "choices" 0 "message")
                           (llm-response-json response)))
         (tool-calls-json (cond
                           ((focus (key-ref "tool_calls")
                                   llm-reply)
                            => vector->list)
                           (else '()))))
    (let-values (((state tool-call-effects)
                  (foldn (lambda (call-json state effects)
                           (let-values (((state new-effects)
                                         (next-state-tool-call
                                          state session-id call-json tools)))
                             (values state
                                     (append new-effects effects))))
                         tool-calls-json
                         (-> state
                             ;; Push LLM response onto messages.
                             (push (state-messages session-id)
                                   llm-reply
                                   <>))
                         '())))
      (values state
              (cons (agent-message-chunk session-id
                                         ;; Send LLM's text response.
                                         (focus (key-ref "content")
                                                llm-reply))
                    (append tool-call-effects
                            ;; End prompt turn if there are no further tool
                            ;; calls and a cancellation is not in progress.
                            (if (and (null? tool-calls-json)
                                     (not (focus (state-session-cancelling? session-id)
                                                 state)))
                                (list (acp-message `(("jsonrpc" . "2.0")
                                                     ("id" . ,(focus state-client-request-id
                                                                     state))
                                                     ("result"
                                                      ("stopReason" . "end_turn")))))
                                '())
                            ;; All tool calls may have been invalid. Maybe
                            ;; dispatch LLM requests, but only if there were any
                            ;; in the first place.
                            (if (not (null? tool-calls-json))
                                (state->llm-requests session-id state)
                                '())))))))

(define (next-state-tool-call-result state result)
  "Given current @var{state} and a new tool call @var{result}, return the next
state and a list of effects."
  (let* ((session-id (tool-call-result-session-id result))
         (state (-> state
                    ;; Push tool call result onto messages.
                    (push (state-messages session-id)
                          (tool-call-result-json result)
                          <>)
                    ;; Delete tool call from session tool call list.
                    (alist-delete-over (state-tool-calls session-id)
                                       (tool-call-result-call-id result)
                                       <>))))
    (values state
            ;; Send a notification for each tool call evaluated.
            (cons (acp-message `(("jsonrpc" . "2.0")
                                 ("method" . "session/update")
                                 ("params"
                                  ("sessionId" . ,session-id)
                                  ("update"
                                   ;; TODO: Add locations and rawOutput.
                                   ("sessionUpdate" . "tool_call_update")
                                   ("toolCallId" . ,(tool-call-result-call-id result))
                                   ("title" . ,(tool-call-result-title result))
                                   ("kind" . ,(tool-call-result-kind result))
                                   ("status" .
                                    ,(if (tool-call-result-success? result)
                                         "completed"
                                         "failed"))
                                   ("content"
                                    ("type" . "content")
                                    ("content"
                                     ("type" . "text")
                                     ("text" . ,(focus (key-ref "content")
                                                       (tool-call-result-json result)))))
                                   ("rawInput" . ,(tool-call-result-arguments result))))))
                  (state->llm-requests session-id state)))))

(define (next-state state message tools)
  "Given current @var{state} and a new @var{message}, return the next state and a
list of effects.

@var{tools} is the same as in @code{tea-loop}."
  (cond
   ((acp-message? message)
    (let ((json-message (acp-message-json message)))
      (if (focus (key-ref "result") json-message)
          ;; message is a response from the client.
          (next-state-client-response state json-message)
          ;; message is a request/notification from the client.
          (let-values (((state effects)
                        (next-state-client-request state json-message)))
            (values (cond
                     ;; message is a request from the client.
                     ((focus (key-ref "id") json-message)
                      => (cut put
                              state-client-request-id
                              <>
                              state))
                     ;; message is a notification from the client.
                     (else state))
                    effects)))))
   ((llm-response? message)
    (next-state-llm-response state message tools))
   ((tool-call-result? message)
    (next-state-tool-call-result state message))))

(define (tea-loop state llm-base-uri llm-api-key model tools)
  "Run a @acronym{TEA, The Elm Architecture} loop starting with @var{state}.

@var{llm-base-uri} is the base URI of the LLM provider. @var{llm-api-key} is the
API key to authenticate with the LLM provider. @var{model} is the name of the
model. @var{tools} is the list of tools made available to the LLM. It is an
association list matching tool names to @code{<tool>} objects."
  ;; Read a JSON-RPC message, handle it, and loop.
  (let ((line (get-line (current-input-port))))
    (unless (eof-object? line)
      (tea-loop (handle-event (acp-message (json-string->scm line))
                              state
                              llm-base-uri
                              llm-api-key
                              model
                              tools)
                llm-base-uri
                llm-api-key
                model
                tools))))

(define (handle-event event state llm-base-uri llm-api-key model tools)
  "Handle @var{event} with @var{state} and return a new state.

@var{llm-base-uri}, @var{llm-api-key}, @var{model} and @var{tools} are the same
as in @code{tea-loop}."
  (let-values (((state effects)
                ;; Compute the next state and collect the effects.
                (next-state state event tools)))
    ;; Do the effects.
    (fold (cut do-effect <> <> llm-base-uri llm-api-key model tools)
          state
          effects)))

(define (do-effect effect state llm-base-uri llm-api-key model tools)
  "Do @var{effect} with @var{state} and return a new state.

@var{llm-base-uri}, @var{llm-api-key}, @var{model} and @var{tools} are the same
as in @code{tea-loop}."
  (cond
   ;; Send message to client, and return the state unchanged.
   ((acp-message? effect)
    (display (scm->json-string (acp-message-json effect)))
    (newline)
    (flush-output-port (current-output-port))
    state)
   ;; Send request to LLM, handle the response, and return the new state.
   ((llm-request? effect)
    ;; TODO: Handle network failures in OpenAI query.
    (handle-event (llm-response (llm-request-session-id effect)
                                (openai-query llm-base-uri
                                              llm-api-key
                                              model
                                              (llm-request-messages effect)
                                              (map (match-lambda
                                                     ((name . tool)
                                                      (tool->spec name tool)))
                                                   tools)))
                  state
                  llm-base-uri
                  llm-api-key
                  model
                  tools))
   ;; Evaluate tool, handle the result, and return the new state.
   ((tool-call? effect)
    (handle-event (eval-tool-call effect tools)
                  state
                  llm-base-uri
                  llm-api-key
                  model
                  tools))))
