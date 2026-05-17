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
             (kaagum tools))

(test-begin "tools")

(test-equal "tool call with parameter type mismatch must error out"
  "Error: Argument arg of invalid type"
  (guard (c ((tool-call-parse-failure? c)
             (tool-call-parse-failure-message c)))
    (spec->tool-call "foo"
                     "/foo/bar"
                     `(("read" . ,(tool #:description "A dummy test tool"
                                        #:parameters `(("arg" . ,(tool-parameter
                                                                  #:description "an argument"
                                                                  #:type "integer"
                                                                  #:required? #t)))
                                        #:proc identity
                                        #:title identity
                                        #:kind "read")))
                     (list "read")
                     (list)
                     `(("id" . "foo123")
                       ("type" . "function")
                       ("function"
                        ("name" . "read")
                        ("arguments" . "{\"arg\": \"[738\"}"))))))

(test-end "tools")
