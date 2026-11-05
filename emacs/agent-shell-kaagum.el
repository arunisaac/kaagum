;;; agent-shell-kaagum.el --- Kaagum ACP agent configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Arun Isaac

;; Author: Arun Isaac <arunisaac@systemreboot.net>
;; Version: 0.1.0

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file includes a kaagum ACP agent configuration for testing.
;;
;; Point `agent-shell-kaagum-command' at the kaagum executable, and
;; set `agent-shell-kaagum-parameters' to a list of parameters to pass
;; to kaagum.

;;; Code:

(require 'agent-shell)
(require 'shell-maker)

(defcustom agent-shell-kaagum-command
  "kaagum"
  "Command for the kaagum agent."
  :type 'string
  :group 'agent-shell)

(defcustom agent-shell-kaagum-parameters
  '("--api-key-command=pass openrouter.ai")
  "Command parameters for the kaagum agent."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-kaagum-environment
  nil
  "Environment variables for the kaagum agent.

This should be a list of environment variables to be used when starting
the kaagum agent.

Example usage to set custom environment variables:

  (setq agent-shell-kaagum-environment
        (agent-shell-make-environment-variables
         \"MY_VAR\" \"some-value\"
         \"MY_OTHER_VAR\" \"another-value\"))"
  :type '(repeat string)
  :group 'agent-shell)

(defun agent-shell-kaagum-make-agent-config ()
  "Create a kaagum ACP agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'kaagum
   :mode-line-name "kaagum"
   :buffer-name "kaagum"
   :shell-prompt "kaagum> "
   :shell-prompt-regexp "kaagum> "
   :welcome-function #'agent-shell-kaagum--welcome-message
   :client-maker #'agent-shell-kaagum-make-client))

(defun agent-shell-kaagum-start-agent ()
  "Start an interactive kaagum agent shell."
  (interactive)
  (agent-shell-start :config (agent-shell-kaagum-make-agent-config)))

(defun agent-shell-kaagum-make-client (buffer)
  "Create a kaagum ACP client using BUFFER as context."
  (agent-shell--make-acp-client :command agent-shell-kaagum-command
                                :command-params agent-shell-kaagum-parameters
                                :environment-variables agent-shell-kaagum-environment
                                :context-buffer buffer))

(defun agent-shell-kaagum--welcome-message (config)
  "Return kaagum welcome message using `shell-maker' CONFIG."
  (concat (propertize " 🐦‍⬛" 'font-lock-face '(:height 5.0))
          (string-trim-left (shell-maker-welcome-message config) "\n")))

(provide 'agent-shell-kaagum)

;;; agent-shell-kaagum.el ends here
