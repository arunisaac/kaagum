kaagum (pronounced kah-kah) is a tiny, security-focused AI agent written in Guile with minimal dependencies. Kaagum works with any LLM that provides an OpenAI-compatible API.

kaagum runs tool calls securely using containers and capability-based access. Tool calls have limited or no access to the filesystem and to the network. Capabilities allow network access to be controlled with fine granularity. Containers are implemented using Guix's container API.

Kaagum offers no user interface on its own. Instead, it speaks the [Agent Client Protocol (ACP)](https://agentclientprotocol.com/) and allows you to use any compatible user interface of your choice.

# Author's note on AI use

This program is not an endorsement of AI or its use. Please think for yourself, and use AI wisely (if at all). The societal, environmental and ethical consequences of AI are real, and I struggle with them myself. I hope we, as the free software community, can come to some reasonable consensus on how we are going to deal with AI. Meanwhile, I have been [nerd sniped](https://xkcd.com/356/) into experimenting with AI, and this is my effort to disentangle myself from popular AI agents that are recklessly biased towards convenience and thoughtless about dependencies (think npm crap).

Here are my current views on the topic. I fully retain the right to change my views on this later, of course.

As a Schemer, I believe that programs must be written for people to read, and only incidentally for machines to execute (from the Preface to SICP). But, I don't particularly care if program authors use AI as a writing/coding/debugging aid, as long as they still subscribe to *high* standards of readability. Large chunks of poorly vetted AI slop are unfair on the reader and are therefore unacceptable. Vibe coding is not a substitute for human skill, but I'm afraid a lot of people are going to think so.

## AI use in this project

Despite being an AI agent, all code in this project is lovingly hand-crafted. I wouldn't leave security to a stochastic machine like an LLM. And of course, readability and the craft are important ideals. In the future, I may get AI to generate boring things like examples and test cases, but that's about it.

# License

kaagum is free software released under the terms of the [GNU General Public License](https://www.gnu.org/licenses/gpl.html), either version 3 of the License, or (at your option) any later version.

# The Name

kaagum (காகம்—pronounced kah-gum) means "crow" 🐦‍⬛ in Tamil. It also evokes "kaa" (கா—pronounced kah), a verb that means "to protect". Crows are intelligent birds, and kaagum protects you from reckless/malicious tool use by LLMs.

In colloquial Tamil, crows are also called "kaakaa"—which is similar to the word for shit in many European languages. Just a gentle reminder that LLMs, despite how intelligent they sometimes seem, often produce shitty code—rather like the crows that bombard you with aerial excrement when you least expect it.
