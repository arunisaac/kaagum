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

(define-module (kaagum tea)
  #:use-module ((rnrs base) #:select (assertion-violation))
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
  #:use-module (kaagum config)
  #:use-module (kaagum lens)
  #:use-module (kaagum monads)
  #:use-module (kaagum openai)
  #:use-module (kaagum records)
  #:use-module (kaagum tools)
  #:use-module (kaagum trace)
  #:use-module (kaagum utils)
  #:use-module (kaagum web)
  #:export (run-tea-loop))

(define %tool-allow-once
  '(("optionId" . "allow-once")
    ("name" . "Allow once")
    ("kind" . "allow_once")))

(define %tool-allow-always
  '(("optionId" . "allow-always")
    ("name" . "Allow always")
    ("kind" . "allow_always")))

(define %tool-reject-once
  '(("optionId" . "reject-once")
    ("name" . "Reject once")
    ("kind" . "reject_once")))

(define %tool-reject-always
  '(("optionId" . "reject-always")
    ("name" . "Reject always")
    ("kind" . "reject_always")))

(define-record-type* (<session> session session?)
  (lambda (constructor)
    (lambda* (cwd model
                  #:key
                  (input-tokens 0) (output-tokens 0) (thought-tokens 0)
                  (cache-read-tokens 0) (cache-write-tokens 0) (cost 0)
                  cancelling? (messages '()) (pending-tool-calls '())
                  (allowed-tools '()) (rejected-tools '()))
      (constructor cwd model input-tokens output-tokens thought-tokens
                   cache-read-tokens cache-write-tokens cost
                   cancelling? messages pending-tool-calls
                   allowed-tools rejected-tools)))
  (fields (cwd session-cwd lensed)
          (model session-model lensed)
          ;; Input tokens used in current turn
          (input-tokens session-input-tokens lensed)
          ;; Output tokens used in current turn
          (output-tokens session-output-tokens lensed)
          ;; Thought/reasoning tokens used in current turn
          (thought-tokens session-thought-tokens lensed)
          ;; Cache read tokens used in current turn
          (cache-read-tokens session-cache-read-tokens lensed)
          ;; Cache write tokens used in current turn
          (cache-write-tokens session-cache-write-tokens lensed)
          ;; Cost of session
          (cost session-cost lensed)
          (cancelling? session-cancelling? lensed)
          (messages session-messages lensed)
          (tool-calls session-tool-calls lensed)
          ;; List of tool names that are allowlisted for the session
          (allowed-tools session-allowed-tools lensed)
          ;; List of tool names that are blocklisted for the session
          (rejected-tools session-rejected-tools lensed)))

(define-record-type* (<state> state state?)
  (fields
   ;; ID of the client request being served
   (client-request-id state-client-request-id lensed)
   (agent-request-id state-agent-request-id lensed)
   (next-session-id state-next-session-id lensed)
   ;; Association list mapping agent request IDs to tool calls for which
   ;; permission is sought
   (requests-alist state-requests-alist lensed)
   (sessions state-sessions lensed)))

(define (state-cwd session-id)
  "Return a lens to focus on current working directory of session with
@var{session-id} in state."
  (compose session-cwd
           (key-ref session-id)
           state-sessions))

(define (state-model session-id)
  "Return a lens to focus on the current model of session with @var{session-id} in
state."
  (compose session-model
           (key-ref session-id)
           state-sessions))

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

(define (state-allowed-tools session-id)
  "Return a lens to focus on allowed tools of session with @var{session-id} in
state."
  (compose session-allowed-tools
           (key-ref session-id)
           state-sessions))

(define (state-rejected-tools session-id)
  "Return a lens to focus on rejected tools of session with @var{session-id} in
state."
  (compose session-rejected-tools
           (key-ref session-id)
           state-sessions))

(define (state-tool-call tool-call-id session-id)
  "Return a lens to focus on tool call with @var{tool-call-id} of session with
@var{session-id} in state."
  (compose (key-ref tool-call-id)
           (state-tool-calls session-id)))

(define (state-session-input-tokens session-id)
  "Return a lens to focus on the input tokens of session with @var{session-id} in
state."
  (compose session-input-tokens
           (key-ref session-id)
           state-sessions))

(define (state-session-output-tokens session-id)
  "Return a lens to focus on the output tokens of session with @var{session-id} in
state."
  (compose session-output-tokens
           (key-ref session-id)
           state-sessions))

(define (state-session-thought-tokens session-id)
  "Return a lens to focus on the thought tokens of session with @var{session-id} in
state."
  (compose session-thought-tokens
           (key-ref session-id)
           state-sessions))

(define (state-session-cache-read-tokens session-id)
  "Return a lens to focus on the cache read tokens of session with @var{session-id}
in state."
  (compose session-cache-read-tokens
           (key-ref session-id)
           state-sessions))

(define (state-session-cache-write-tokens session-id)
  "Return a lens to focus on the cache write tokens of session with
@var{session-id} in state."
  (compose session-cache-write-tokens
           (key-ref session-id)
           state-sessions))

(define (state-session-cost session-id)
  "Return a lens to focus on the total cost of session with @var{session-id} in
state."
  (compose session-cost
           (key-ref session-id)
           state-sessions))

(define (state-session-cancelling? session-id)
  "Return a lens to focus on the @code{cancelling?} flag of session with
@var{session-id} in state."
  (compose session-cancelling?
           (key-ref session-id)
           state-sessions))

(define (llm-requests session-id)
  "Return the state-monadic list of @code{<llm-request>} objects for session with
@var{session-id}."
  (state-let* ((state (current-state)))
    (state-return
     (if (and (null? (focus (state-tool-calls session-id)
                            state))
              (not (focus (state-session-cancelling? session-id)
                          state)))
         ;; There are no more tool calls in flight and a cancellation is not in
         ;; progress; dispatch to LLM.
         (list (llm-request session-id
                            (focus (state-model session-id)
                                   state)
                            (map (lambda (message)
                                   ;; Strip unnecessary fields (such as
                                   ;; reasoning fields) based on role.
                                   (let* ((role (focus (key-ref "role") message))
                                          (allowed-fields
                                           (cond
                                            ((string=? role "user")
                                             '("role" "content"))
                                            ((string=? role "assistant")
                                             '("role" "content" "tool_calls"))
                                            ((string=? role "tool")
                                             '("role" "content" "tool_call_id")))))
                                     (filter (match-lambda
                                               ((key . _)
                                                (member key allowed-fields)))
                                             message)))
                                 ;; Reverse because we have been prepending new
                                 ;; messages onto the list.
                                 (reverse (focus (state-messages session-id)
                                                 state)))))
         ;; There are tool calls or a cancellation in progress; do nothing.
         (list)))))

(define-record-type* (<llm-request> llm-request llm-request?)
  (fields (session-id llm-request-session-id)
          (model llm-request-model)
          (messages llm-request-messages)))

(define-record-type* (<llm-response> llm-response llm-response?)
  (fields (session-id llm-response-session-id)
          (json llm-response-json)))

(define-record-type* (<acp-message> acp-message acp-message?)
  (fields (json acp-message-json lensed)))

(define-record-type* (<command> command command?)
  (fields (description command-description)
          (effects command-effects)))

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
           (lambda (session-id tools argument)
             (state-let* ((state (current-state)))
               (state-return
                (list (agent-message-chunk session-id
                                           (focus (state-cwd session-id)
                                                  state))))))))

(define (markdown-table lines)
  "Return a markdown table built from @var{lines}. Each line is a list of strings,
each string the contents of a cell. The first line is considered the header of
the table."
  (define (cells->line cells)
    (string-append "| "
                   (string-join cells " | ")
                   " |"))

  (match lines
    ((header other-lines ...)
     (string-join (cons* (cells->line header)
                         (cells->line (make-list (length header) "---"))
                         (map cells->line
                              other-lines))
                  "\n"))))

(define %tools-command
  (command "List available tools and their permission status"
           (lambda (session-id tools argument)
             (state-let* ((state (current-state)))
               (let* ((allowed-tools (focus (state-allowed-tools session-id)
                                            state))
                      (rejected-tools (focus (state-rejected-tools session-id)
                                             state))
                      (lines
                       (cons (list "Tool" "Permission")
                             (map (match-lambda
                                    ((name . _)
                                     (list name
                                           (cond
                                            ((member name allowed-tools) "allow")
                                            ((member name rejected-tools) "reject")
                                            (else "prompt user")))))
                                  tools))))
                 (state-return
                  (list (agent-message-chunk session-id
                                             (markdown-table lines)))))))))

(define %commands
  `(("cwd" . ,%cwd-command)
    ("tools" . ,%tools-command)))

(define (slash-command-effects session-id tools command-name argument)
  "Return the state-monadic list of effects invoking slash @var{command-name} with
@var{argument} for @var{session-id}.

@var{tools} is the same as in @code{run-tea-loop}."
  (cond
   ;; command exists
   ((focus (key-ref command-name)
           %commands)
    => (lambda (command)
         ((command-effects command) session-id tools argument)))
   ;; command not found
   (else
    (state-return (list (agent-message-chunk session-id
                                             "Error: Unknown command"))))))

(define (model->spec id model)
  "Serialize @var{model} of @var{id} to ACP-compatible JSON spec."
  `(("value" . ,id)
    ;; The OpenAI API does not have the name and description fields, but the
    ;; OpenRouter API provides them. So, we treat these as optional.
    ,@(if (model-name model)
          `(("name" . ,(model-name model)))
          '())
    ,@(if (model-description model)
          `(("description" . ,(model-description model)))
          '())))

(define (jsonrpc-error request-id code message)
  "Return a JSON-RPC error message for @var{request-id} with @var{code} and
@var{message}."
  (acp-message `(("jsonrpc" . "2.0")
                 ("id" . ,request-id)
                 ("error"
                  ("code" . ,code)
                  ("message" . ,message)))))

(define (config-options current-model-id available-models)
  "Return the @code{configOptions} JSON field with the @var{current-model-id} and
the association list of @var{available-models}."
  (vector `(("id" . "model")
            ("name" . "Model")
            ("category" . "model")
            ("type" . "select")
            ("currentValue" . ,current-model-id)
            ("options" .
             ,(list->vector (map (match-lambda
                                   ((id . model)
                                    (model->spec id model)))
                                 available-models))))))

(define (client-request-effects request models tools)
  "Return the state-monadic list of effects for new ACP @var{request}.

@var{tools} is the same as in @code{run-tea-loop}. @var{models} is the same as
in @code{tea-loop}."
  (state-let* ((state (current-state)))
    (let ((request-id (focus state-client-request-id
                             state)))
      (cond
       ;; There is a pending request from the client; process it.
       ((focus (key-ref "method") request)
        => (match-lambda
             ("initialize"
              (state-return
               (list (acp-message `(("jsonrpc" . "2.0")
                                    ("id" . ,request-id)
                                    ("result"
                                     ("protocolVersion" . 1)
                                     ("agentCapabilities")
                                     ("agentInfo"
                                      ("name" . ,%project)
                                      ("title" . ,%project-title)
                                      ("version" . ,%version))
                                     ("authMethods" . #())))))))
             ("session/new"
              (let ((session-id
                     (string-append "session-"
                                    (number->string
                                     (focus state-next-session-id state))))
                    (model (match models
                             (((model-id . _) . _)
                              model-id))))
                (state-begin
                 (state-let* ((state (current-state)))
                   (set-current-state (-> state
                                          ;; Push new session onto list.
                                          (push state-sessions
                                                (cons session-id
                                                      ;; TODO: Check if cwd is an absolute path.
                                                      (session (focus (in "params" "cwd")
                                                                      request)
                                                               model))
                                                <>)
                                          ;; Increment next session ID.
                                          (over state-next-session-id
                                                1+
                                                <>))))
                 (state-return
                  (map acp-message
                       `( ;; Return new session.
                         (("jsonrpc" . "2.0")
                          ("id" . ,request-id)
                          ("result"
                           ("sessionId" . ,session-id)
                           ("configOptions" . ,(config-options model models))))
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
                                                 %commands))))))))))))
             ("session/set_config_option"
              (let ((session-id (focus (in "params" "sessionId")
                                       request))
                    (config-id (focus (in "params" "configId")
                                      request))
                    (model (focus (in "params" "value")
                                  request)))
                (if (string=? config-id "model")
                    ;; The client is setting the model.
                    (if (focus (key-ref model)
                               models)
                        ;; Set model in state and respond to the client.
                        (state-begin
                         (state-let* ((state (current-state)))
                           (set-current-state (put (state-model session-id)
                                                   (focus (in "params" "value")
                                                          request)
                                                   state)))
                         (state-return
                          (list (acp-message `(("jsonrpc" . "2.0")
                                               ("id" . ,request-id)
                                               ("result"
                                                ("configOptions" .
                                                 ,(config-options model models))))))))
                        ;; The client specified a model that is not one of the
                        ;; available models.
                        (state-return
                         (list (jsonrpc-error request-id
                                              -32602
                                              "Invalid model"))))
                    ;; The client specified an unknown configId parameter.
                    (state-return
                     (list (jsonrpc-error request-id
                                          -32602
                                          "Unsupported configId parameter"))))))
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
                         (state-let* ((slash-command-effects
                                       (slash-command-effects
                                        session-id tools command-name argument))
                                      ;; End prompt turn immediately. This means
                                      ;; slash commands cannot send LLM requests
                                      ;; or initiate other exchanges.
                                      (end-turn-effects
                                       (end-turn-effects session-id models "end_turn")))
                           (state-return
                            (append slash-command-effects
                                    end-turn-effects))))))
                 ;; regular prompt
                 (else
                  (state-begin
                   (state-let* ((state (current-state)))
                     (set-current-state (push (state-messages session-id)
                                              `(("role" . "user")
                                                ("content" . ,prompt))
                                              state)))
                   (llm-requests session-id))))))
             ("session/cancel"
              (let ((session-id (focus (in "params" "sessionId")
                                       request)))
                (state-begin
                 ;; Reset state and end the prompt turn.
                 (state-let* ((state (current-state)))
                   (set-current-state (-> state
                                          (put (state-session-cancelling? session-id)
                                               #f
                                               <>)
                                          (put (state-tool-calls session-id)
                                               '()
                                               <>)
                                          (put state-requests-alist
                                               '()
                                               <>))))
                 (state-return
                  (list (acp-message `(("jsonrpc" . "2.0")
                                       ("id" . ,request-id)
                                       ("result"
                                        ("stopReason" . "cancelled")))))))))))))))

(define (client-response-effects response)
  "Return the state-monadic list of effects for a new ACP @var{response}."
  (state-let* ((state (current-state)))
    (let ((request-id (focus (key-ref "id")
                             response)))
      (cond
       ;; The tool call for this response has been found; process it.
       ((focus (compose (key-ref request-id)
                        state-requests-alist)
               state)
        => (lambda (tool-call)
             (let* ((session-id (tool-call-session-id tool-call))
                    (outcome (focus (in "result" "outcome" "outcome")
                                    response))
                    (option-id (focus (in "result" "outcome" "optionId")
                                      response))
                    ;; Decode permission selection to symbol.
                    (selection
                     (cond
                      ((and (string=? outcome "selected")
                            (string=? option-id
                                      (focus (key-ref "optionId") %tool-allow-once)))
                       'allow-once)
                      ((and (string=? outcome "selected")
                            (string=? option-id
                                      (focus (key-ref "optionId") %tool-allow-always)))
                       'allow-always)
                      ((and (string=? outcome "selected")
                            (string=? option-id
                                      (focus (key-ref "optionId") %tool-reject-once)))
                       'reject-once)
                      ((and (string=? outcome "selected")
                            (string=? option-id
                                      (focus (key-ref "optionId") %tool-reject-always)))
                       'reject-always)
                      ;; We don't explicitly look for "cancelled". We
                      ;; defensively assume anything other than "selected" is
                      ;; "cancelled". This protects us from buggy clients.
                      (else 'cancel))))
               (state-begin
                (set-current-state (-> state
                                       ;; If the tool was "allowed always", add
                                       ;; it to the list of allowed tools.
                                       (over (state-allowed-tools session-id)
                                             (lambda (allowed-tools)
                                               (if (eq? selection 'allow-always)
                                                   (cons (tool-call-function tool-call)
                                                         allowed-tools)
                                                   allowed-tools))
                                             <>)
                                       ;; If the tool was "rejected always", add
                                       ;; it to the list of rejected tools.
                                       (over (state-rejected-tools session-id)
                                             (lambda (rejected-tools)
                                               (if (eq? selection 'reject-always)
                                                   (cons (tool-call-function tool-call)
                                                         rejected-tools)
                                                   rejected-tools))
                                             <>)
                                       ;; Update tool status.
                                       (put (compose tool-call-status
                                                     (state-tool-call (tool-call-id tool-call)
                                                                      session-id))
                                            (cond
                                             ((eq? selection 'cancel) 'cancelled)
                                             ((memq selection '(allow-once allow-always))
                                              'approved)
                                             ((memq selection '(reject-once reject-always))
                                              'rejected)
                                             ;; This branch should be
                                             ;; unreachable.
                                             (else
                                              (assertion-violation selection
                                                                   "Invalid selection")))
                                            <>)
                                       ;; If the tool call was cancelled, set
                                       ;; the cancelling flag to indicate that a
                                       ;; cancellation is in progress.
                                       (put (state-session-cancelling? session-id)
                                            (eq? selection 'cancel)
                                            <>)
                                       ;; Unregister request corresponding to
                                       ;; this response.
                                       (alist-delete-over state-requests-alist
                                                          request-id
                                                          <>)))
                ;; Request tool call evaluation. eval-tool-call handles
                ;; cancelled and rejected tool calls correctly. We don't have to
                ;; worry about it here.
                (state-let* ((state (current-state)))
                  (state-return
                   (list (focus (state-tool-call (tool-call-id tool-call)
                                                 (tool-call-session-id tool-call))
                                state))))))))
       ;; Client response is stale (it pertains to a request not in
       ;; requests-alist). Silently ignore it.
       (else (state-return '()))))))

(define (send-agent-request-effects request context)
  "Return the state-monadic list of effects sending @var{request} from the agent to
the client. Stash @var{context} against request ID in @code{requests-alist} for
future recall."
  (state-let* ((state (current-state)))
    (let ((request-id (focus state-agent-request-id state)))
      (state-begin
       (set-current-state (-> state
                              ;; Register this request so we can recall the
                              ;; details later when the response comes in.
                              (push state-requests-alist
                                    (cons request-id
                                          context)
                                    <>)
                              ;; Bump agent request ID.
                              (over state-agent-request-id
                                    1+
                                    <>)))
       (state-return
        ;; Set request ID in request JSON.
        (list (if (assoc "id" (focus acp-message-json request))
                  ;; The request JSON already has an "id" field; overwrite it.
                  (put (compose (key-ref "id") acp-message-json)
                       request-id
                       request)
                  ;; The request JSON has no "id" field; cons it on;
                  (over acp-message-json
                        (cut cons
                             (cons "id" request-id)
                             <>)
                        request))))))))

(define (tool-call-effects session-id call-json tools)
  "Return the state-monadic list of effects for a new tool @var{call-json} in
session with @var{session-id}.

@var{tools} is the same as in @code{run-tea-loop}."
  (state-let* ((state (current-state)))
    (guard (c ((tool-call-parse-failure? c)
               (let ((call-id (focus (key-ref "id") call-json)))
                 (state-begin
                  (state-let* ((state (current-state)))
                    (set-current-state (-> state
                                           ;; Push tool call response onto messages.
                                           (push (state-messages session-id)
                                                 `(("role" . "tool")
                                                   ("tool_call_id" . ,call-id)
                                                   ("content" . ,(tool-call-parse-failure-message c)))
                                                 <>))))
                  ;; Notify client about invalid tool call.
                  (state-return (list (acp-message `(("jsonrpc" . "2.0")
                                                     ("method" . "session/update")
                                                     ("params"
                                                      ("sessionId" . ,session-id)
                                                      ("update"
                                                       ("sessionUpdate" . "tool_call_update")
                                                       ("toolCallId" . ,call-id)
                                                       ("status" . "failed")))))))))))
      (let ((call (spec->tool-call session-id
                                   (focus (state-cwd session-id)
                                          state)
                                   tools
                                   (focus (state-allowed-tools session-id)
                                          state)
                                   (focus (state-rejected-tools session-id)
                                          state)
                                   call-json)))
        (state-begin
         ;; Queue tool call.
         (set-current-state (push (state-tool-calls session-id)
                                  (cons (tool-call-id call)
                                        call)
                                  state))
         (state-let* ((effects
                       (if (eq? (focus tool-call-status call)
                                'pending-approval)
                           ;; Tool call requires permission from the client;
                           ;; dispatch a request.
                           (send-agent-request-effects
                            (acp-message `(("jsonrpc" . "2.0")
                                           ("method" . "session/request_permission")
                                           ("params"
                                            ("sessionId" . ,session-id)
                                            ("toolCall"
                                             ("toolCallId" . ,(tool-call-id call)))
                                            ("options" .
                                             ,(vector %tool-allow-once
                                                      %tool-allow-always
                                                      %tool-reject-once
                                                      %tool-reject-always)))))
                            call)
                           ;; Tool call is already pre-approved or pre-rejected;
                           ;; schedule it.
                           (state-return (list call)))))
           ;; Notify client about new tool call before other effects.
           (state-return
            (cons (let ((tool (focus (key-ref (tool-call-function call))
                                     tools))
                        (args (alist->plist (tool-call-arguments call))))
                    (acp-message `(("jsonrpc" . "2.0")
                                   ("method" . "session/update")
                                   ("params"
                                    ("sessionId" . ,session-id)
                                    ("update"
                                     ("sessionUpdate" . "tool_call")
                                     ("toolCallId" . ,(tool-call-id call))
                                     ("title" . ,(apply (tool-title tool) args))
                                     ("kind" . ,(tool-kind tool))
                                     ("rawInput" . ,(tool-call-arguments call))
                                     ("status" . "pending"))))))
                  effects))))))))

(define (end-turn-effects session-id models stop-reason)
  "Return the state-monadic list of effects for ending the turn of session with
@var{session-id}.

@var{models} is the same as in @code{tea-loop}."
  (state-let* ((state (current-state)))
    (let ((input-tokens (focus (state-session-input-tokens session-id)
                               state))
          (output-tokens (focus (state-session-output-tokens session-id)
                                state))
          (thought-tokens (focus (state-session-thought-tokens session-id)
                                 state))
          (cache-read-tokens (focus (state-session-cache-read-tokens session-id)
                                    state))
          (cache-write-tokens (focus (state-session-cache-write-tokens session-id)
                                     state)))
      (state-begin
       (set-current-state (-> state
                              ;; Reset per-turn token counters.
                              (put (state-session-input-tokens session-id)
                                   0
                                   <>)
                              (put (state-session-output-tokens session-id)
                                   0
                                   <>)
                              (put (state-session-thought-tokens session-id)
                                   0
                                   <>)
                              (put (state-session-cache-read-tokens session-id)
                                   0
                                   <>)
                              (put (state-session-cache-write-tokens session-id)
                                   0
                                   <>)))
       (state-return
        (list (acp-message `(("jsonrpc" . "2.0")
                             ("id" . ,(focus state-client-request-id
                                             state))
                             ("result"
                              ("stopReason" . ,stop-reason)
                              ;; Report usage if it is non-zero. Usage can
                              ;; be zero if turn was for a slash command.
                              ,@(if (any (negate zero?)
                                         (list input-tokens
                                               output-tokens
                                               thought-tokens
                                               cache-read-tokens
                                               cache-write-tokens))
                                    `(("usage"
                                       ("totalTokens" . ,(+ input-tokens
                                                            output-tokens))
                                       ("inputTokens" . ,input-tokens)
                                       ("outputTokens" . ,output-tokens)
                                       ("thoughtTokens" . ,thought-tokens)
                                       ("cachedReadTokens" . ,cache-read-tokens)
                                       ("cachedWriteTokens" . ,cache-write-tokens)))
                                    '()))))
              (acp-message `(("jsonrpc" . "2.0")
                             ("method" . "session/update")
                             ("params"
                              ("sessionId" . ,session-id)
                              ("update"
                               ("sessionUpdate" . "usage_update")
                               ("used" . ,(+ input-tokens
                                             output-tokens))
                               ("size" . ,(focus (compose model-context-length
                                                          (key-ref (focus (state-model session-id)
                                                                          state)))
                                                 models))
                               ("cost"
                                ("amount" . ,(focus (state-session-cost session-id)
                                                    state))
                                ("currency" . "USD"))))))))))))

(define (llm-response-effects response tools models)
  "Return the state-monadic list of effects for new LLM @var{response}.

@var{tools} is the same as in @code{run-tea-loop}. @var{models} is the same as
in @code{tea-loop}."
  (let* ((session-id (llm-response-session-id response))
         (llm-reply (focus (in-json "choices" 0 "message")
                           (llm-response-json response)))
         (tool-calls-json (cond
                           ((focus (key-ref "tool_calls")
                                   llm-reply)
                            => vector->list)
                           (else '()))))
    (state-begin
     (state-let* ((state (current-state)))
       (set-current-state (-> state
                              (add (state-session-input-tokens session-id)
                                   (focus (in-json "usage" "prompt_tokens")
                                          (llm-response-json response))
                                   <>)
                              (add (state-session-output-tokens session-id)
                                   (focus (in-json "usage" "completion_tokens")
                                          (llm-response-json response))
                                   <>)
                              (add (state-session-thought-tokens session-id)
                                   (or (focus (in-json "usage"
                                                       "completion_tokens_details"
                                                       "reasoning_tokens")
                                              (llm-response-json response))
                                       0)
                                   <>)
                              (add (state-session-cache-read-tokens session-id)
                                   (focus (in-json "usage"
                                                   "prompt_tokens_details"
                                                   "cached_tokens")
                                          (llm-response-json response))
                                   <>)
                              (add (state-session-cache-write-tokens session-id)
                                   (or (focus (in-json "usage"
                                                       "prompt_tokens_details"
                                                       "cache_write_tokens")
                                              (llm-response-json response))
                                       0)
                                   <>)
                              (add (state-session-cost session-id)
                                   (or (focus (in-json "usage" "cost")
                                              (llm-response-json response))
                                       0)
                                   <>)
                              ;; Push LLM response onto messages.
                              (push (state-messages session-id)
                                    llm-reply
                                    <>))))
     (state-let* ((tool-call-effects
                   (state-append-map (lambda (call-json)
                                       (tool-call-effects session-id call-json tools))
                                     tool-calls-json)))
       (let ((effects (cons (agent-message-chunk session-id
                                                 ;; Send LLM's text response.
                                                 (focus (key-ref "content")
                                                        llm-reply))
                            tool-call-effects)))
         (if (null? tool-calls-json)
             ;; There are no further tool calls,
             (state-let* ((state (current-state)))
               (if (not (focus (state-session-cancelling? session-id)
                               state))
                   ;; … and a cancellation is not in progress; end turn.
                   (state-let* ((end-turn-effects
                                 (end-turn-effects session-id models "end_turn")))
                     (state-return (append effects end-turn-effects)))
                   ;; Else, return what we have so far.
                   (state-return effects)))
             ;; Maybe dispatch LLM requests.
             (state-let* ((llm-requests (llm-requests session-id)))
               (state-return (append effects llm-requests)))))))))

(define (tool-call-result-effects result)
  "Return the state-monadic list of effects for new tool call @var{result}."
  (let ((session-id (tool-call-result-session-id result)))
    (state-begin
     (state-let* ((state (current-state)))
       (set-current-state (-> state
                              ;; Push tool call result onto messages.
                              (push (state-messages session-id)
                                    (tool-call-result-json result)
                                    <>)
                              ;; Delete tool call from session tool call list.
                              (alist-delete-over (state-tool-calls session-id)
                                                 (tool-call-result-call-id result)
                                                 <>))))
     ;; Send a notification for each tool call evaluated.
     (state-let* ((llm-requests (llm-requests session-id)))
       (state-return
        (cons (acp-message `(("jsonrpc" . "2.0")
                             ("method" . "session/update")
                             ("params"
                              ("sessionId" . ,session-id)
                              ("update"
                               ;; TODO: Add locations and rawOutput.
                               ("sessionUpdate" . "tool_call_update")
                               ("toolCallId" . ,(tool-call-result-call-id result))
                               ("status" .
                                ,(if (tool-call-result-success? result)
                                     "completed"
                                     "failed"))
                               ("content" .
                                ,(vector `(("type" . "content")
                                           ("content"
                                            ("type" . "text")
                                            ("text" .
                                             ,(focus (key-ref "content")
                                                     (tool-call-result-json result)))))))))))
              llm-requests))))))

(define (acp-effects message models tools)
  "Return the state-monadic list of effects for @var{message}.

@var{tools} is the same as in @code{run-tea-loop}. @var{models} is the same as
in @code{tea-loop}."
  (cond
   ((acp-message? message)
    (let ((json-message (focus acp-message-json message)))
      (if (focus (key-ref "result") json-message)
          ;; message is a response from the client.
          (client-response-effects json-message)
          ;; message is a request/notification from the client.
          (state-begin
           ;; When message is a request from the client, put it in the state.
           (let ((request-id (focus (key-ref "id") json-message)))
             (state-when request-id
               (state-let* ((state (current-state)))
                 (set-current-state (put state-client-request-id
                                         request-id
                                         state)))))
           (client-request-effects json-message models tools)))))
   ;; End turn if message is an LLM response containing an HTTP error.
   ((and (llm-response? message)
         (http-error? (llm-response-json message)))
    (end-turn-effects (llm-response-session-id message)
                      models
                      ;; TODO: The v2 version of the protocol allows custom stop
                      ;; reasons. Perhaps we can use that here to be more
                      ;; specific about the cause of the failure.
                      "refusal"))
   ((llm-response? message)
    (llm-response-effects message tools models))
   ((tool-call-result? message)
    (tool-call-result-effects message))))

(define* (tea-loop state llm-base-uri llm-api-key models tools
                   #:optional (events (let ((line (get-line (current-input-port))))
                                        (and (not (eof-object? line))
                                             (begin
                                               (trace 'acp "<- ~a" line)
                                               (list (acp-message (json-string->scm line))))))))
  "Run a @acronym{TEA, The Elm Architecture} loop starting with @var{state}.

@var{llm-base-uri}, @var{llm-api-key} and @var{tools} are the same as in
@code{run-tea-loop}. @var{models} is an association list mapping model IDs to
@code{<model>} objects, the first element of which is the default model.
@var{events} is an internal recursion variable."
  ;; Read a JSON-RPC message, handle it, and loop.
  (when events
    (let-values (((effects next-state)
                  (run-with-state
                    (state-append-map (cut acp-effects <> models tools)
                                      events)
                    state)))
      (match (append-map (cut do-effect <> llm-base-uri llm-api-key tools)
                         effects)
        (()
         (tea-loop next-state llm-base-uri llm-api-key models tools))
        ((events ...)
         (tea-loop next-state llm-base-uri llm-api-key models tools events))))))

(define (do-effect effect llm-base-uri llm-api-key tools)
  "Do @var{effect}. Return list of events produced by the effect.

@var{llm-base-uri}, @var{llm-api-key} and @var{tools} are the same as in
@code{run-tea-loop}."
  (cond
   ;; Send message to client, and return the state unchanged.
   ((acp-message? effect)
    (let ((json-message (scm->json-string (focus acp-message-json effect))))
      (display json-message)
      (newline)
      (flush-output-port (current-output-port))
      (trace 'acp "-> ~a" json-message))
    (list))
   ;; Send request to LLM, handle the response, and return the new state.
   ((llm-request? effect)
    ;; TODO: Handle network failures in OpenAI query.
    (list (llm-response (llm-request-session-id effect)
                        (openai-query llm-base-uri
                                      llm-api-key
                                      (llm-request-model effect)
                                      (llm-request-messages effect)
                                      (map (match-lambda
                                             ((name . tool)
                                              (tool->spec name tool)))
                                           tools)))))
   ;; Evaluate tool, handle the result, and return the new state.
   ((tool-call? effect)
    (list (eval-tool-call effect tools)))))

(define (run-tea-loop llm-base-uri llm-api-key model tools)
  "Run a @acronym{TEA, The Elm Architecture} loop. @var{llm-base-uri} is the base
URI of the LLM provider. @var{llm-api-key} is the API key to authenticate with
the LLM provider. @var{model} is the name of the model to initialize sessions
with. @var{tools} is the list of tools made available to the LLM. It is an
association list matching tool names to @code{<tool>} objects."
  (let ((models (openai-models llm-base-uri llm-api-key)))
    (tea-loop (state #f 0 0 '() '())
              llm-base-uri
              llm-api-key
              (cons (assoc model models)
                    (alist-delete model models))
              tools)))
