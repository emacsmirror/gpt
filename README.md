# gpt.el

<p align="center">
  <img src="gpt.gif" alt="gpt.el demo" width="600"/>
</p>

gpt.el is an Emacs package that lets you interact with instruction-following language models like GPT-5.2, Claude 4.5 Opus, Claude 4.5 Sonnet, and Gemini 3 Pro from your editor. You can type a natural language command (with history and completion support) and optionally use the current region or buffer contents as input for the model. The package displays the output of the model in a temporary or named buffer, and updates it as the model generates more text. You can issue follow-up commands that provide the interaction history in that buffer as context. You can also browse, save, and clear the command history for later reference.

**This is a pure Elisp implementation** - no Python or external dependencies required beyond Emacs 28.1+ and `curl` for streaming.

## Features

- **Multi-provider support**: OpenAI, Anthropic, and Google Gemini APIs
- **Pure Elisp**: No Python dependencies - everything runs natively in Emacs
- **Seamless Emacs integration**: Use current buffer/region as context
- **Streaming responses**: Real-time output as the model generates text (via curl)
- **Interactive conversations**: Follow-up commands with conversation history
- **Command history**: Browse, save, and clear your command history
- **Flexible context modes**: All buffers, current buffer, or no context
- **Multi-model comparison**: Run the same prompt against multiple models in parallel
- **Extended thinking mode**: Enhanced reasoning for Anthropic models with streaming thought process
- **Web search**: Real-time web search for grounded responses (Anthropic models)

## Installation

### Prerequisites

- Emacs 28.1 or later
- `curl` (for streaming responses) - pre-installed on macOS and most Linux distributions
- Valid API keys for OpenAI, Anthropic, or Google

#### API Keys

You can get API keys from:

- OpenAI: https://platform.openai.com/
- Anthropic: https://console.anthropic.com/
- Google Gemini: https://aistudio.google.com/app/apikey

### From MELPA (Recommended)

MELPA is a popular third-party package repository for Emacs. To install gpt.el from MELPA, first add MELPA as a source in your Emacs init file:

```elisp
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
```

Then, use the built-in package manager to install gpt.el:

```
M-x package-install RET gpt RET
```

### From Source

To install gpt.el from source, clone this repository and add the following to your Emacs init file:

```elisp
(add-to-list 'load-path "/path/to/gpt.el")
(require 'gpt)
```

Alternatively, you can use `use-package`:

```elisp
(use-package gpt
  :load-path "/path/to/gpt.el")
```

## Configuration

You need to set at least one of the variables `gpt-openai-key`, `gpt-anthropic-key`, or `gpt-google-key` to your respective API keys:

```elisp
(setq gpt-openai-key "sk-...")
(setq gpt-anthropic-key "sk-ant-api03-...")
(setq gpt-google-key "AIzaSy...")
```

Optionally, customize the model parameters:

```elisp
(setq gpt-model "claude-opus-4-5")  ; Default model (auto-updates token budget)
(setq gpt-max-tokens "64000")  ; Automatically set based on model
(setq gpt-temperature "0")
```

### Available Models

gpt.el supports the latest models from all providers. The built-in models are defined in `gpt-available-models`:

**OpenAI:**

- `gpt-5.2` - GPT-5.2 (400k max tokens)
- `gpt-5.1` - GPT-5.1 (400k max tokens)
- `gpt-5-mini` - GPT-5 Mini (200k max tokens)

**Anthropic:**

- `claude-opus-4-5` - Claude 4.5 Opus (default, 32k max tokens)
- `claude-sonnet-4-5` - Claude 4.5 Sonnet (64k max tokens)

**Google:**

- `gemini-3-pro-preview` - Gemini 3 Pro Preview (60k max tokens)

You can switch models interactively with `M-x gpt-switch-model` or `C-c C-m` in gpt-mode buffers.

**The API provider is automatically selected based on the model** - you don't need to set it manually. Simply choose your model and gpt.el handles the rest:

```elisp
;; Set default model - API provider auto-selected
(setq gpt-model "claude-opus-4-5")  ; Uses Anthropic
(setq gpt-model "gpt-5.2")          ; Uses OpenAI
(setq gpt-model "gemini-3-pro-preview")  ; Uses Google
```

To add custom models, customize `gpt-available-models`. Each entry contains the API type, model ID, and max output tokens.

## Usage

### Main Commands

- `gpt-chat` - Interactive prompt to choose context mode and enter command
- `gpt-chat-multi-models` - Run the same command against multiple models in parallel, each in its own buffer. Uses defaults from `gpt-multi-models-default`. Use `C-u` prefix to pick models interactively.
- `gpt-chat-all-buffers` - Use all visible buffers as context
- `gpt-chat-current-buffer` - Use only the current buffer as context
- `gpt-edit-current-buffer` - Rewrite the current buffer with GPT, review the diff, and accept or reject the changes
- After generating a diff, `gpt-edit-current-buffer` also lets you supply additional feedback to iterate until the result looks right.
- `gpt-chat-no-context` - Use no buffer context

### Key Bindings

You can bind these functions to keys of your choice:

```elisp
(global-set-key (kbd "M-C-g") 'gpt-chat)
(global-set-key (kbd "M-C-b") 'gpt-chat-all-buffers)
(global-set-key (kbd "M-C-c") 'gpt-chat-current-buffer)
(global-set-key (kbd "M-C-e") 'gpt-edit-current-buffer)
```

### GPT Mode Key Bindings

When in a GPT output buffer (`gpt-mode`), these keys are available:

- `C-c C-c` - Follow-up command with conversation history
- `C-c C-p` - Toggle visibility of User:/Assistant: prefixes
- `C-c C-b` - Copy code block at point to clipboard
- `C-c C-m` - Switch between models interactively
- `C-c C-t` - Generate a title for the current buffer
- `C-c C-k` - Kill the running GPT process in this buffer
- `C-c C-q` - Close current GPT buffer
- `C-c C-x` - Close all GPT buffers
- `C-c C-r` - Regenerate the last assistant response

Thinking mode commands:

- `C-c C-j t` - Toggle extended thinking mode
- `C-c C-j i` - Toggle interleaved thinking mode
- `C-c C-j w` - Toggle web search
- `C-c C-j s` - Show current thinking mode status
- `C-c C-j m` - Run the same command across multiple models (uses defaults; `C-u` to pick)

### Context Modes

- **all-buffers**: Uses all visible buffers as context, with cursor position marked
- **current-buffer**: Uses only the current buffer as context, with cursor position marked
- **none**: Uses no buffer context

### Thinking Mode (Anthropic)

Extended thinking mode allows Claude models to reason step-by-step before responding. This is enabled by default and improves quality for complex tasks.

**Controls:**

- `gpt-thinking-enabled` - Enable/disable extended thinking (default: t)
- `gpt-interleaved-thinking` - Stream thinking blocks in real-time (default: t)
- `gpt-thinking-budget` - Token budget for thinking (auto-set to 1/3 of max_tokens)

**Constraints:**

- Temperature is automatically set to 1.0 when thinking is enabled (API requirement)
- Thinking budget must be less than max_tokens
- Interleaved thinking and the 1M context window beta are mutually exclusive

**Keybindings** (in gpt-mode buffers):

- `C-c C-j t` - Toggle extended thinking
- `C-c C-j i` - Toggle interleaved thinking
- `C-c C-j s` - Show current thinking status

### Web Search (Anthropic)

Web search allows Claude to access current information from the web, useful for questions about recent events.

- `gpt-web-search` - Enable/disable web search (default: t)
- `C-c C-j w` - Toggle web search

## Architecture

gpt.el is implemented entirely in Emacs Lisp with a modular backend architecture:

```
gpt.el                 Main entry point
├── gpt-core.el        Core configuration and model management
├── gpt-api.el         API request orchestration
├── gpt-http.el        HTTP layer (url.el + curl for streaming)
├── gpt-ui.el          User interface and buffer management
├── gpt-mode.el        Major mode for GPT buffers
└── backends/
    ├── gpt-backend.el     Base backend class (EIEIO)
    ├── gpt-openai.el      OpenAI provider
    ├── gpt-anthropic.el   Anthropic provider (with thinking/web search)
    └── gpt-google.el      Google Gemini provider
```

### Backend System

Each provider implements the `gpt-backend` base class using EIEIO (Emacs's object system):

- `gpt-backend-headers` - Generate authentication headers
- `gpt-backend-request-data` - Build API request payload
- `gpt-backend-parse-response` - Parse API response
- `gpt-backend-parse-stream-chunk` - Parse streaming SSE chunks

### HTTP Layer

- **Non-streaming**: Uses built-in `url.el` for standard requests
- **Streaming**: Uses `curl` subprocess for Server-Sent Events (SSE)

## Development

### Running Tests

Unit tests (no API keys required):

```bash
emacs -Q -batch -l test/gpt-test.el -f ert-run-tests-batch-and-exit
```

Integration tests (requires API keys as environment variables):

```bash
export OPENAI_API_KEY="your-key"
export ANTHROPIC_API_KEY="your-key"
export GOOGLE_API_KEY="your-key"
emacs -Q -batch -l test/gpt-integration-test.el -f gpt-integration-run-tests
```

### Code Quality

Run `make check` to validate parentheses, load each file, byte-compile, and lint the Elisp sources. Use `make help` to see all available maintenance targets.

## Troubleshooting

### API Key Errors

If you get errors about missing or invalid API keys:

1. **Check key is set**: Verify that you've set at least one API key:
   ```elisp
   (setq gpt-openai-key "sk-...")     ; For OpenAI
   (setq gpt-anthropic-key "sk-ant-api03-...")  ; For Anthropic
   (setq gpt-google-key "AIzaSy...")  ; For Google
   ```

2. **Check key format**: API keys should be strings without quotes in the actual key value.

3. **Restart Emacs**: After setting keys, restart Emacs to ensure they're loaded.

### curl Not Found

If streaming doesn't work, ensure `curl` is installed and in your PATH:

```bash
which curl
# Should output something like /usr/bin/curl
```

On macOS, curl is pre-installed. On Linux, install via your package manager:

```bash
# Debian/Ubuntu
sudo apt install curl

# Fedora
sudo dnf install curl
```

### Process Hangs or Timeouts

If GPT commands hang or don't complete:

1. **Check process status**: Use `C-c C-k` in the GPT buffer to kill a hung process.

2. **Check network**: Verify you can reach the API endpoints:
   ```bash
   curl -I https://api.openai.com/v1/models
   curl -I https://api.anthropic.com/v1/messages
   ```

3. **Enable debug output**: Check `*Messages*` buffer for error details.

### Buffer-Specific Issues

If GPT commands fail in certain buffers:

1. **Check buffer-file-name**: Some commands require a file-backed buffer.

2. **Check for read-only buffers**: GPT edit commands require writable buffers.

3. **Large buffers**: Very large buffers may exceed token limits. Try selecting a region instead.

## Recent Changes

### 2025-01-09: Pure Elisp Rewrite

- Removed all Python dependencies - gpt.el is now pure Elisp
- Implemented modular backend system using EIEIO classes
- Added streaming support via curl for real-time responses
- Created comprehensive test suite (unit + integration tests)
- Simplified installation - just Emacs 28.1+ and curl required

## License

MIT License - see LICENSE.md for details.
