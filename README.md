# Claudemacs

AI pair programming with [Claude Code](https://docs.anthropic.com/en/docs/claude-code/overview) in Emacs.

## Features

- **Eat terminal integration** - Clean Claude Code interface using eat terminal emulator
- **Workspace-aware sessions** - Automatic buffer naming based on Doom/Perspective workspaces
- **Project-based sessions** - Each project gets its own Claude session
- **Transient interface** - Easy-to-use menu system with `C-c C-v`
- **Session resumption** - Resume previous Claude Code sessions

## Installation

### Prerequisites

1. Install [Claude Code CLI](https://docs.anthropic.com/en/docs/claude-code/overview)
2. Install the `eat` package in Emacs

### Package Installation

#### Doom Emacs

Add to your `packages.el`:

```elisp
(package! claudemacs
  :recipe (:host github :repo "cpoile/claudemacs"))
```

Then in your `config.el`:

```elisp
(use-package! claudemacs
  :config
  (claudemacs-mode 1))
```

#### Manual Installation

Clone this repository and add to your Emacs configuration:

```elisp
;; Add to load path
(add-to-list 'load-path "/path/to/claudemacs")

;; Load the package
(require 'claudemacs)

;; Enable minor mode globally (optional)
(claudemacs-mode 1)
```

## Setup

<!-- TODO: add setup instructions -->

## Usage

### Basic Commands

- `C-c C-v c` - Start Claude Code session
- `C-c C-v r` - Start Claude Code with resume option

### Customization

```elisp
;; Custom Claude Code executable path
(setq claudemacs-program "/usr/local/bin/claude")

;; Add command line switches
(setq claudemacs-program-switches '("--verbose" "--dangerously-skip-permissions"))
```

## Buffer Naming

Claudemacs creates workspace-aware buffer names:
- With workspace: `*claudemacs:workspace-name*`
- Without workspace: `*claudemacs:/path/to/project*`

Currently supports Doom Emacs workspaces and Perspective mode. If you use another workspace package and would like support added, please open an issue - I'm happy to add support for others.

## Requirements

- Emacs 26.1+
- [eat](https://github.com/kephale/emacs-eat) package
- [Claude Code CLI](https://docs.anthropic.com/en/docs/claude-code/overview)

## Credits

Inspired by:
- [Aidermacs](https://github.com/MatthewZMD/aidermacs) by Matthew Zeng
- [claude-code.el](https://github.com/stevemolitor/claude-code.el) by Steve Molitor

## License

MIT License. See LICENSE file for details.
