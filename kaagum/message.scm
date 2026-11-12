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
  #:use-module (kaagum records))

(define-public-record-type* (<user-message> user-message user-message?)
  (fields (content user-message-content)))

(define-public-record-type* (<assistant-message> assistant-message assistant-message?)
  (fields (content assistant-message-content)
          (tool-calls assistant-message-tool-calls)))

(define-public-record-type* (<tool-message> tool-message tool-message?)
  (fields (tool-call-id tool-message-tool-call-id)
          (content tool-message-content)))
