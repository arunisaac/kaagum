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

(define-module (kaagum-website)
  #:use-module ((gnu packages fonts) #:select (font-charter font-fira-code))
  #:use-module ((gnu packages haskell-xyz) #:select (pandoc))
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((kaagum-package) #:select (kaagum)))

(define kaagum-website-home-page-gexp
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))

        (copy-file #$(file-append (package-source kaagum)
                                  "/README.md")
                   "README.md")
        (invoke #$(file-append pandoc "/bin/pandoc")
                "--standalone"
                "--metadata" "title=kaagum"
                "--metadata" "document-css=false"
                "--css=style.css"
                "--from=gfm"
                (string-append "--output=" #$output)
                "README.md"))))

(define-public kaagum-website
  (file-union "kaagum-website"
              `(("index.html"
                 ,(computed-file "kaagum-website-home-page.html"
                                 kaagum-website-home-page-gexp))
                ("style.css" ,(local-file "../website/style.css"))
                ("fonts/charter_regular.woff2"
                 ,(file-append font-charter
                               "/share/fonts/web/charter_regular.woff2"))
                ("fonts/FiraCode-Regular.woff2"
                 ,(file-append font-fira-code
                               "/share/fonts/web/FiraCode-Regular.woff2"))
                ("fonts/FiraCode-SemiBold.woff2"
                 ,(file-append font-fira-code
                               "/share/fonts/web/FiraCode-SemiBold.woff2")))))

;; A quick wrapper to emulate how guix-forge serves the kaagum website under a
;; /kaagum prefix
(file-union "kaagum-website"
            `(("kaagum" ,kaagum-website)))
