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

Add the following channel to your `channels.scm` and run a `guix pull`. Then install `kaagum` and `emacs-agent-shell-kaagum`.
```scheme
(channel
  (name 'kaagum)
  (url "https://klaus.systemreboot.net/kaagum/")
  (branch "main")
  (introduction
   (make-channel-introduction
    "b71c385f8885eb629ec6ad0c47eeb1f3e12be687"
    (openpgp-fingerprint
     "7F73 0343 F2F0 9F3C 77BF  79D3 2E25 EE8B 6180 2BB3"))))
```
Put the following in your `~/.emacs`. The `--api-key-command` specifies the password manager command to execute to get the API key for your LLM service provider. The `--model` specifies the name of the model you wish to interact with.
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
