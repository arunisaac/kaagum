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

(define-module (kaagum-package)
  #:use-module ((gnu packages emacs-xyz)
                #:select (emacs-agent-shell))
  #:use-module ((gnu packages guile)
                #:select (guile-next guile-json-4))
  #:use-module ((gnu packages guile-xyz)
                #:select (guile-lens guile-run64))
  #:use-module ((gnu packages nss) #:select (nss-certs))
  #:use-module ((gnu packages package-management)
                #:select (guix))
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public kaagum
  (package
    (name "kaagum")
    (version "0.1.0")
    (source (local-file ".."
                        "kaagum-checkout"
                        #:recursive? #t
                        #:select? (or (git-predicate (dirname (current-source-directory)))
                                      (const #t))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list (string-append "prefix=" #$output)
                   (string-append "CERTIFICATES_DIRECTORY="
                                  #$(this-package-input "nss-certs")
                                  "/etc/ssl/certs"))
           #:modules `(((guix build guile-build-system)
                        #:select (target-guile-effective-version))
                       ,@%default-gnu-imported-modules)
           #:phases
           (with-imported-modules `((guix build guile-build-system)
                                    ,@%default-gnu-imported-modules)
             #~(modify-phases %standard-phases
                 (replace 'patch-source-shebangs
                   (lambda* (#:key inputs #:allow-other-keys)
                     (substitute* "bin/kaagum"
                       (("^exec guile")
                        (string-append "exec "
                                       (search-input-file inputs "/bin/guile"))))))
                 (delete 'configure)
                 (add-after 'install 'wrap
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     (let ((out (assoc-ref outputs "out"))
                           (effective-version (target-guile-effective-version)))
                       (wrap-program (string-append out "/bin/kaagum")
                         `("GUILE_LOAD_PATH" prefix
                           (,(string-append out "/share/guile/site/" effective-version)
                            ,(getenv "GUILE_LOAD_PATH")))
                         `("GUILE_LOAD_COMPILED_PATH" prefix
                           (,(string-append out "/lib/guile/"
                                            effective-version "/site-ccache")
                            ,(getenv "GUILE_LOAD_COMPILED_PATH")))))))))))
    (inputs
     (list guile-next guile-json-4 guile-lens guix nss-certs))
    (native-inputs
     (list guile-run64))
    (home-page "https://forge.systemreboot.net/kaagum")
    (synopsis "Tiny, security-focused AI agent in Guile")
    (description
     "kaagum is a tiny, security-focused AI agent written in Guile with
minimal dependencies. Kaagum works with any LLM that provides an
OpenAI-compatible API.

kaagum runs tool calls securely using containers and capability-based
access. Tool calls have limited or no access to the filesystem and to
the network. Capabilities allow network access to be controlled with
fine granularity. Containers are implemented using Guix's container
API.

kaagum offers no user interface on its own. Instead, it speaks
the @acronym{ACP, Agent Client Protocol} and allows you to use any
compatible user interface of your choice.")
    (license license:gpl3+)))

(define-public emacs-agent-shell-kaagum
  (package
    (name "emacs-agent-shell-kaagum")
    (version "0.1.0")
    (source (package-source kaagum))
    (build-system emacs-build-system)
    (arguments
     (list #:tests? #f  ; no tests
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-elisp-dir
                 (lambda _
                   (chdir "emacs")))
               (add-after 'enter-elisp-dir 'configure
                 (lambda _
                   (emacs-substitute-variables "agent-shell-kaagum.el"
                     ("agent-shell-kaagum-command"
                      (which "kaagum"))))))))
    (inputs
     (list kaagum))
    (propagated-inputs
     (list emacs-agent-shell))
    (home-page (package-home-page kaagum))
    (synopsis "agent-shell integration for the kaagum AI agent")
    (description "This package provides agent-shell integration for the kaagum
AI agent.")
    (license license:gpl3+)))

kaagum
