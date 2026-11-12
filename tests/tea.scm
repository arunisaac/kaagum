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
                              "gen-1793743825"
                              '(("role" . "assistant")
                                ("content" . "Hello! How can I help you today?"))
                              1770
                              13
                              0
                              0
                              1766
                              3.601e-4
                              #f)
                %models
                '())
   (state #f 0 1 '()
          `((0 . ,(session (getcwd) "foo"))))))

(test-assert "handle LLM response without \"cached_tokens\" field in usage"
  (run-with-state
   (acp-effects (llm-response 0
                              "gen-1793743825"
                              '(("role" . "assistant")
                                ("content" . "Hello! How can I help you today?"))
                              1770
                              13
                              0
                              0
                              1766
                              3.601e-4
                              #f)
                %models
                '())
   (state #f 0 1 '()
          `((0 . ,(session (getcwd) "foo"))))))

(test-end "tea")
