# ClaudEmacs

AI pair programming with [Claude Code](https://docs.anthropic.com/en/docs/claude-code/overview) in Emacs.

## Features

- **Project-based sessions** - Each project gets its own Claude session rooted at its git root
- **Workspace-aware naming** - Automatic buffer naming based on Doom/Perspective workspaces, or the git root dir
- **Terminal fixes** - Use `u` to unstick input box and reset buffer issues (see [Tips](#tips) section)
- **Transient interface** - Easy-to-use menu system (customizable keybinding; default: `C-c C-e`)
- **Session resumption** - Resume previous Claude Code sessions
- **Fix error at point** - Will send flycheck error to Claude
- **Implement comment at point** - Extracts comment text and sends it to Claude for implementation, with context
- **Execute request with context** - Send a request to Claude, will add file and line (or region) context
- **Add file or current file** - Will add file with Claude's @ symbol convention
- **Option: Swap RET and M-RET** - Swap keys by preference (Claude maps RET to submit, and M-RET to newline)
- **Option: S-RET as newline** - May be more typical (Claude maps S-RET to submit)

## Table of Contents

- [Installation](#installation)
  - [Prerequisites](#prerequisites)
  - [Package Installation](#package-installation)
    - [Doom Emacs](#doom-emacs)
    - [Manual Installation](#manual-installation)
- [Setup](#setup)
- [Usage](#usage)
  - [Basic Commands](#basic-commands)
  - [Customization](#customization)
- [Buffer Naming](#buffer-naming)
- [Tips and Tricks](#tips-and-tricks)
- [Requirements](#requirements)
- [Credits](#credits)
- [License](#license)

## Installation

### Prerequisites

1. Install [Claude Code CLI](https://docs.anthropic.com/en/docs/claude-code/overview)
2. Install the [eat](https://codeberg.org/akib/emacs-eat) package in Emacs

### Package Installation

#### Doom Emacs

Add to your `packages.el`:

```elisp
(package! claudemacs
  :recipe (:host github :repo "cpoile/claudemacs"))
```

Then in your `config.el`:

```elisp
(use-package! claudemacs)
```

#### use-package with :vc (Emacs 29+)

```elisp
(use-package claudemacs
  :vc (:fetcher github :repo "cpoile/claudemacs"))
```

#### straight.el

```elisp
(straight-use-package
 '(claudemacs :type git :host github :repo "cpoile/claudemacs"))
```

#### Manual Installation

Clone this repository and add to your Emacs configuration:

```elisp
;; Add to load path
(add-to-list 'load-path "/path/to/claudemacs")

;; Load the package
(require 'claudemacs)
```

## Setup

Use your preferred keybinding (I use `C-c C-e`). I'd recommend adding to the relevant mode-maps (instead of using a `global-set-key`, since that will override the very useful `C-c C-e` keybind in the `eat-semi-char-mode-map` (see [Using Eat Mode](#using-eat-mode-effectively) section below).

Doom Emacs style:

```elisp
(map! :map prog-mode-map
      "C-c C-e" #'claudemacs-transient-menu)
(map! :map emacs-lisp-mode-map
      "C-c C-e" #'claudemacs-transient-menu)
(map! :map text-mode-map
      "C-c C-e" #'claudemacs-transient-menu)

;; Set a big buffer so we can search our history.
(after! eat
  (setq eat-term-scrollback-size 400000))
```

Regular Emacs style:

```elisp
(require 'claudemacs)
(define-key prog-mode-map (kbd "C-c C-e") #'claudemacs-transient-menu)
(define-key emacs-lisp-mode-map (kbd "C-c C-e") #'claudemacs-transient-menu)
(define-key text-mode-map (kbd "C-c C-e") #'claudemacs-transient-menu)
```

Other useful tweaks:

```elisp
;; If you want it to pop up as a new buffer. Otherwise, it will use "other buffer."
;; Personally, I use the "other buffer" style.
(add-to-list 'display-buffer-alist
             '("^\\*claudemacs"
               (display-buffer-in-side-window)
               (side . right)
               (window-width . 0.40)))

;; Turn on autorevert because Claude modifies and saves buffers. Make it a habit to save
;; before asking Claude anything, because it uses the file on disk as its source of truth.
;; (And you don't want to lose edits after it modifies and saves the files.)
(after! autorevert
  (global-auto-revert-mode t))
```

## Usage

### Basic Commands

ClaudEmacs provides a transient menu accessible via `C-c C-e` (or your own keybinding):

#### Core Commands
- `c` - Start Claude Code session (or switch to existing)
- `r` - Start Claude Code with resume option (or switch to existing)
- `k` - Kill active ClaudEmacs session
- `t` - Toggle ClaudEmacs buffer visibility

#### Action Commands
- `e` - Fix error at point (using flycheck if available)
- `x` - Execute request with file context (current line or region)
- `i` - Implement comment (extracts comment text and asks Claude to implement it)
- `f` - Add file reference (@file) to conversation
- `F` - Add current file reference to conversation

#### Maintenance Commands
- `u` - Unstick Claude input box (reset buffer tracking)

All commands are also available as `M-x claudemacs-<command>` (e.g., `M-x claudemacs-run`).

### Customization

ClaudEmacs provides several customization variables to tailor the experience to your workflow:

#### Basic Configuration

```elisp
;; Custom Claude Code executable path (default: "claude")
(setq claudemacs-program "/usr/local/bin/claude")

;; Add command line switches (default: nil)
(setq claudemacs-program-switches '("--verbose" "--dangerously-skip-permissions"))
```

#### Buffer Behavior

```elisp
;; Whether to switch to ClaudEmacs buffer when creating new session (default: t)
(setq claudemacs-switch-to-buffer-on-create nil)

;; Whether to switch to ClaudEmacs buffer when toggling visibility (default: t)
(setq claudemacs-switch-to-buffer-on-toggle nil)

;; Whether to switch to ClaudEmacs buffer when adding file references (default: nil)
(setq claudemacs-switch-to-buffer-on-file-add t)
```

#### Key Bindings

```elisp
;; Swap RET and M-RET behavior in ClaudEmacs buffers (default: nil)
;; When enabled: RET creates newline, M-RET submits
(setq claudemacs-m-return-is-submit t)

;; Enable Shift-Return to create newlines (default: t) 
;; Provides alternative to M-RET for creating newlines
(setq claudemacs-shift-return-newline t)
```

All variables can also be customized via `M-x customize-group RET claudemacs RET`.

## Buffer Naming

ClaudEmacs creates workspace-aware buffer names:
- With workspace: `*claudemacs:workspace-name*`
- Without workspace: `*claudemacs:/path/to/project*`

Currently supports Doom Emacs workspaces and Perspective mode. If you use another workspace package and would like support added, please open an issue - I'm happy to add support for others.

## Tips and Tricks

### Using eat-mode effectively

When interacting with the eat-mode buffer, you are limited in what you can do in the default semi-char mode.

Press `C-c C-e` to enter emacs mode. A box cursor will appear, which you can use to move around and select and kill text.
Press `C-c C-l` to re-enter semi-char mode and continue typing to Claude.

### Scroll-popping, input box sticking, input box border draw issues

There is a tricky interaction between Eat-mode and Claude Code, probably because Claude Code uses some input libraries that eat has trouble with. It was causing the eat-mode buffer to "scroll-pop" to the top whenever you change the other window's buffer. This is mostly fixed now, but a side effect is sometimes the Claude Clode input box gets stuck halfway up the buffer and won't move.

There are also issues with drawing the input box border after the window resizes, which is expected of terminal programs. 

If you see these issues, press `u` in the ClaudEmacs transient menu to "unstick" the buffer, and everything should get reset.

### Buffer Toggle Edge Case

When toggling the ClaudEmacs buffer visibility, there's an edge case to be aware of: if a window was originally created for ClaudEmacs, but you've since switched to another workspace and back, that window may have shown other buffers in the meantime. In this case, the window is no longer considered "created just for ClaudEmacs" and won't automatically close when you toggle. This is due to Emacs' window management - once a window has been reused for other content, it loses its original "dedicated" status.

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
