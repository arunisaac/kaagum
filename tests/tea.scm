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

(use-modules (srfi srfi-64)
             (kaagum monads)
             (kaagum openai)
             (kaagum tea))

(define llm-response
  (@@ (kaagum tea) llm-response))

(define acp-effects
  (@@ (kaagum tea) acp-effects))

(define state
  (@@ (kaagum tea) state))

(define session
  (@@ (kaagum tea) session))

(define %models
  `(("foo" . ,(model "foo" "Foobar" (expt 10 6)))))

(test-begin "tea")

(test-assert "handle LLM response"
  (run-with-state
   (acp-effects (llm-response 0
                              `(("id" . "gen-1793743825")
                                ("model" . "anthropic/claude-haiku-5.5")
                                ("choices"
                                 .
                                 #((("message"
                                     ("role" . "assistant")
                                     ("content" . "Hello! How can I help you today?")))))
                                ("usage"
                                 ("prompt_tokens" . 1770)
                                 ("completion_tokens" . 13)
                                 ("total_tokens" . 1783)
                                 ("cost" . 3.601e-4)
                                 ("prompt_tokens_details"
                                  ("cached_tokens" . 0)
                                  ("cache_write_tokens" . 1766))
                                 ("completion_tokens_details"
                                  ("reasoning_tokens" . 0)))))
                %models
                '())
   (state #f 0 1 '()
          `((0 . ,(session (getcwd) "foo"))))))

(test-end "tea")
