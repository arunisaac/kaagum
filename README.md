kaagum (pronounced *kah-gum*) is a tiny, security-focused AI agent written in Guile with minimal dependencies. Kaagum works with any LLM that provides an OpenAI-compatible API, including local models such as those run via [llama-cpp](https://github.com/ggml-org/llama.cpp)—no data ever needs to leave your machine.

kaagum runs tool calls securely using fine-grained sandboxing based on the principle of least privilege. Tool calls that deal with the filesystem run in containers with no network access and with filesystem access restricted to the directories they need to see. Tool calls that need network access are limited to querying specific hard-coded endpoints. For example, a tool that reads an issue on a software forge (like GitHub or Codeberg) can only query a specific API endpoint on that forge and nothing else. Other AI agents do not provide this level of fine-grained network access control.

Kaagum offers no user interface on its own. Instead, it speaks the [Agent Client Protocol (ACP)](https://agentclientprotocol.com/) and allows you to use any compatible user interface of your choice. In the interest of security, the ACP client's file read/write capabilities are never used.

# Brainstorming future direction

- Session saving and restoring
- Sharing more than one directory with the tool call container
- Streaming LLM responses to the clients
- Streaming tool call output to the LLM?
- Monitoring token usage
- Providing custom tools written in Guile
- Fine-grained enabling/disabling of tools
- Tool-specific container/network access configuration
- More tools! (git clone, git diff, git log, guix build, etc.)

# Getting started

The following instructions get you started running kaagum with Guix and Emacs [agent-shell](https://github.com/xenodium/agent-shell).

Install the `kaagum` and `emacs-agent-shell-kaagum` packages from Guix. Then, put the following in your `~/.emacs`. The `--api-key-command` specifies the password manager command to execute to get the API key for your LLM service provider. The `--model` specifies the name of the model you wish to interact with.
```elisp
(require 'agent-shell-kaagum)

(setq agent-shell-kaagum-command "kaagum"
      agent-shell-kaagum-parameters
      '("--api-key-command=pass openrouter.ai"
        "--model=anthropic/claude-sonnet-4.6"))

(add-to-list 'agent-shell-agent-configs
             (agent-shell-kaagum-make-agent-config))
```
Change to the directory you want to work in. kaagum will only have access to that directory and its contents. Run `M-x agent-shell` and select `kaagum` from the menu.

# Using custom tools with kaagum

kaagum empowers you to equip your agents with custom tools specific to your needs. To do this, you write a scheme file (let's call it `custom-tools.scm`) that defines your custom tools. Then, you pass that file on to kaagum using the `--more-tools=custom-tools.scm` parameter.

What should your custom tools file look like? Here's an example:
```scheme
(use-modules (kaagum tools))

(define %string-length
  (tool #:description "Compute the length of a string."
        #:parameters `(("str" . ,(tool-parameter
                                  #:type "string"
                                  #:description "String whose length to compute"
                                  #:required? #t)))
        #:proc (lambda* (#:key str)
                 (format #t "~a~%" (string-length str)))
        #:title (lambda* (#:key str)
                  (format #f "string-length ~s" str))
        #:kind "other"))

`(("string-length" . ,%string-length))
```
The file must return an association list mapping tool names to definitions (`<tool>` objects) as its final expression.

`<tool>` objects are defined using `tool` from `(kaagum tools)`. `tool` takes the following keyword arguments:
- **description:** string that describes the tool; this is read by the LLM to decide whether and how to use the tool—make it a detailed documentation of the tool and the parameters that it takes.
- **parameters:** association list mapping tool parameter names to `<tool-parameter>` objects.
- **proc:** the actual function run if the tool is executed; parameters are passed to it as keyword arguments, and output must be written to standard output
- **title:** human-readable string describing what the tool is doing; this is used by the ACP client in its UI
- **kind:** category of tool—one of `"read"`, `"edit"`, `"delete"`, `"move"`, `"search"`, `"execute"`, `"think"`, `"fetch"`, `"switch_mode"`, or `"other"`; this is also used by the ACP client in its UI
- **container-mappings:** list of Guix `<file-system-mapping>` objects representing file system paths to mount into the container of the tool call process
- **container-namespaces:** list of namespaces to create for the container—should be like `%namespaces` from Guix's `(gnu build linux-container)`

`<tool-parameter>` objects are defined using `tool-parameter` from `(kaagum tools)`. tool-parameter takes the following keyword arguments:
- **type:** parameter type—`"string"`, `"boolean"`, etc.
- **description:** string describing the tool parameter
- **required?:** `#t` if this parameter is required (that is, mandatory), else `#f`; if the parameter is not required, the default argument must be specified in the `lambda*` function in `#:proc` and `#:title`.

See definitions of built-in tools under `kaagum/tools/*.scm` for examples.

If your custom tools require other Guile packages or modules, you may provide access by setting the `GUILE_LOAD_PATH` and `GUILE_LOAD_COMPILED_PATH` environment variables. If you're using Emacs agent shell, you can do this by setting the `agent-shell-kaagum-environment` variable.
```elisp
(setq agent-shell-kaagum-environment
      (agent-shell-make-environment-variables
       "GUILE_LOAD_PATH" "/some/path/"
       "GUILE_LOAD_COMPILED_PATH" "/some/path/"))
```

# Contributing

Please report issues or send patches to the public inbox at [kaagum@systemreboot.net](https://lists.systemreboot.net/kaagum/).

# Author's note on AI use

This program is not an endorsement of AI or its use. Please think for yourself, and use AI wisely (if at all). The societal, environmental and ethical consequences of AI are real, and I struggle with them myself. I hope we, as the free software community, can come to some reasonable consensus on how we are going to deal with AI. Meanwhile, I have been [nerd sniped](https://xkcd.com/356/) into experimenting with AI, and this is my effort to disentangle myself from popular AI agents that are recklessly biased towards convenience and thoughtless about dependencies (think npm crap).

Here are my current views on the topic. I fully retain the right to change my views on this later, of course.

As a Schemer, I believe that programs must be written for people to read, and only incidentally for machines to execute (from the Preface to SICP). But, I don't particularly care if program authors use AI as a writing/coding/debugging aid, as long as they still subscribe to *high* standards of readability. Large chunks of poorly vetted AI slop are unfair on the reader and are therefore unacceptable. Vibe coding is not a substitute for human skill, but I'm afraid a lot of people are going to think so.

## AI use in this project

Despite being an AI agent, all code in this project is lovingly hand-crafted. I wouldn't leave security to a stochastic machine like an LLM. And of course, readability and the craft are important ideals. In the future, I may get AI to generate boring things like examples and test cases, but that's about it.

# License

kaagum is free software released under the terms of the [GNU General Public License](https://www.gnu.org/licenses/gpl.html), either version 3 of the License, or (at your option) any later version.

# The Name

kaagum (காகம்—pronounced *kah-gum*) means *crow* 🐦‍⬛ in Tamil. It also evokes *kaa* (கா—pronounced *kah*), a verb that means *to protect*. Crows are intelligent birds, and kaagum protects you from reckless/malicious tool use by LLMs.

In colloquial Tamil, crows are also called *kaakaa*—which is similar to the word for shit in many European languages. Just a gentle reminder that LLMs, despite how intelligent they sometimes seem, often produce shitty code—rather like the crows that bombard you with aerial excrement when you least expect it.
