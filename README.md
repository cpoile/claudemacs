# Claudemacs

AI pair programming with [Claude Code](https://docs.anthropic.com/en/docs/claude-code/overview) in Emacs.

https://github.com/user-attachments/assets/f7bbd151-7eed-469b-89e8-dad752abb75c

## Features

- Project-based Claude Code, with Workspace-aware sessions (see [Sessions](#workspace-and-project-aware-sessions))
- System notifications: Growl notifications with sound when waiting for input (see [System Notifications](#system-notifications) for setup)
- Terminal fixes: Use `u` to unstick input box and reset buffer issues (see [Tips](#tips-and-tricks) section)
- Session resume: Resume previous Claude Code sessions
- Execute request with context: Send request to Claude, will add file and line/region context
- Fix error at point: Will send flycheck error to Claude, with context
- Implement comment at point: Extracts all comment text and sends it to Claude, with context
- Add file or current file: Will add file with Claude's @ symbol convention
- C-g sends Esc: Old habits die hard
- Option: Swap RET and M-RET - Optionally swap keys (Claude maps RET to submit, and M-RET to newline)
- Option: S-RET as newline - May be more natural (Claude maps S-RET to submit)
- Transient interface: Easy-to-use menu system (customizable keybinding; default: `C-c C-e`)

## Table of Contents

- [Installation](#installation)
  - [Prerequisites](#prerequisites)
  - [Package Installation](#package-installation)
  - [Setup](#setup)
  - [System Notifications](#system-notifications)
  - [Fonts](#fonts)
- [Usage](#usage)
  - [Workspace and Project-aware Sessions](#workspace-and-project-aware-sessions)
  - [Commands](#commands)
  - [Customization](#customization)
    - [Basic Configuration](#basic-configuration)
    - [System Notifications](#system-notifications)
- [Buffer Naming](#buffer-naming)
- [Tips and Tricks](#tips-and-tricks)
  - [Using eat-mode effectively](#using-eat-mode-effectively)
  - [Scroll-popping, input box sticking, input box border draw issues](#scroll-popping-input-box-sticking-input-box-border-draw-issues)
  - [Buffer Toggle Edge Case](#buffer-toggle-edge-case)
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

Use your preferred keybinding (I use `C-c C-e`). I'd recommend adding it to the relevant mode-maps, instead of using a `global-set-key`, since that will override the very useful `C-c C-e` keybind in the `eat-semi-char-mode-map` (see [Using Eat Mode](#using-eat-mode-effectively) section below).

```elisp
(require 'claudemacs)
(define-key prog-mode-map (kbd "C-c C-e") #'claudemacs-transient-menu)
(define-key emacs-lisp-mode-map (kbd "C-c C-e") #'claudemacs-transient-menu)
(define-key text-mode-map (kbd "C-c C-e") #'claudemacs-transient-menu)
(define-key python-base-mode-map (kbd "C-c C-e") #'claudemacs-transient-menu)

;; Set a big buffer so we can search our history.
(with-eval-after-load 'eat
  (setq eat-term-scrollback-size 400000))
```

Other useful tweaks:

```elisp
;; If you want it to pop up as a new buffer. Otherwise, it will use "other buffer."
;; Personally, I use the default "other buffer" style.
(add-to-list 'display-buffer-alist
             '("^\\*claudemacs"
               (display-buffer-in-side-window)
               (side . right)
               (window-width . 0.33)))

;; Turn on autorevert because Claude modifies and saves buffers. Make it a habit to save
;; before asking Claude anything, because it uses the file on disk as its source of truth.
;; (And you don't want to lose edits after it modifies and saves the files.)
(global-auto-revert-mode t)
```

### System Notifications

First, set `claude config set --global preferredNotifChannel terminal_bell`.

#### -- Mac --

For Mac, you need to do some setup to make notifications work.
1. Run the built in `Script Editor` program, start a new script, and run `display notification "Test notification" with title "Test Title" sound name "Frog"`
1. Accept the notification permissions. (Or go into System Settings -> Notifications -> Script Editor and allow notifications there.)

Now you should receive System notifications when Claude Code is waiting for input. It **does not** seem to notify when it's done, unfortunately. Maybe that's how it's designed?

Also, clicking on the notification doesn't bring you to Emacs. Open to ideas on how to fix that.

#### -- Linux / Windows --

I have not tested on linux or windows, so would appreciate any help there (PRs welcome).

### Fonts

Claude Code uses many non-standard unicode characters during its thinking animations, and emojis for its summaries. They look nice, but some of them aren't included in a typical font set (even one patched with Nerd Fonts). So you'll need to add fallbacks.

The fallbacks will differ based on your system.

#### -- Mac --

``` elisp
;;
;; font insanity for Claudemacs
;;
(defun my/setup-custom-font-fallbacks-mac ()
  (interactive)
  "Configure font fallbacks on mac for symbols and emojis.
This will need to be called every time you change your font size,
to load the new symbol and emoji fonts."

  (setq use-default-font-for-symbols nil)

  ;; --- Configure for 'symbol' script ---
  ;; We add fonts one by one. Since we use 'prepend',
  ;; the last one added here will be the first one Emacs tries.
  ;; So, list them in reverse order of your preference.

  ;; Least preferred among this list for symbols (will be at the end of our preferred list)
  (set-fontset-font t 'symbol "Hiragino Sans" nil 'prepend)
  (set-fontset-font t 'symbol "STIX Two Math" nil 'prepend)
  (set-fontset-font t 'symbol "Zapf Dingbats" nil 'prepend)
  (set-fontset-font t 'symbol "Monaco" nil 'prepend)
  (set-fontset-font t 'symbol "Menlo" nil 'prepend)
  ;; Most preferred for symbols -- use your main font here
  (set-fontset-font t 'symbol "JetBrainsMono Nerd Font Mono" nil 'prepend)


  ;; --- Configure for 'emoji' script ---
  ;; Add fonts one by one, in reverse order of preference.

  ;; Least preferred among this list for emojis
  (set-fontset-font t 'emoji "Hiragino Sans" nil 'prepend)
  (set-fontset-font t 'emoji "STIX Two Math" nil 'prepend)
  (set-fontset-font t 'emoji "Zapf Dingbats" nil 'prepend)
  (set-fontset-font t 'emoji "Monaco" nil 'prepend)
  (set-fontset-font t 'emoji "Menlo" nil 'prepend)
  ;; (set-fontset-font t 'emoji "Noto Emoji" nil 'prepend) ;; If you install Noto Emoji
  ;; Most preferred for emojis -- use your main font here
  (set-fontset-font t 'emoji "JetBrainsMono Nerd Font Mono" nil 'prepend))
  
;; to test if you have a font family installed:
;   (find-font (font-spec :family "Menlo"))

;; Then, add the fonts after your setup is complete:
(add-hook 'emacs-startup-hook
          (lambda ()
            (progn
              (when (string-equal system-type "darwin")
                (my/setup-custom-font-fallbacks-mac)))))
```

#### -- Linux / Windows --

I'm not sure of the built in fonts for these systems, or which ones should be used as fallbacks for Claude Code. PRs welcome.


## Usage

### Workspace and Project-aware Sessions

- The Claudemacs session is based on Doom/Perspective workspace, and the Claude Code's cwd is the project's git-root. 
- Why? This allows you to have multiple workspaces in a monorepo, and a separate Claudemacs session per workspace, but each session will be correctly rooted to the project's git root.
- If you don't use workspaces, the decision sequence is: Workspace name -> Perspective name -> project root dir name
- Open to adding other workspace package support, or options for other logic

### Commands

Claudemacs provides a transient menu accessible via `C-c C-e` (or your own keybinding):

Core Commands
- `c` - Start Claude Code session (or switch to existing)
- `r` - Start Claude Code with resume option (or switch to existing)
- `k` - Kill active Claudemacs session
- `t` - Toggle Claudemacs buffer visibility

Action Commands
- `e` - Fix error at point (using flycheck if available)
- `x` - Execute request with file context (current line or region)
- `i` - Implement comment (extracts comment text and asks Claude to implement it)
- `f` - Add file reference (@file) to conversation
- `F` - Add current file reference to conversation

Maintenance Commands
- `u` - Unstick Claude input box (reset buffer tracking)

The following commands are available via `M-x` but not in the transient menu:
- `M-x claudemacs-setup-bell-handler` - Re-setup system notification handler if notifications stop working

### Customization

Claudemacs provides several customization variables to tailor the experience to your workflow:

#### Basic Configuration

```elisp
;; Custom Claude Code executable path (default: "claude")
(setq claudemacs-program "/usr/local/bin/claude")

;; Add command line switches (default: nil)
(setq claudemacs-program-switches '("--verbose" "--dangerously-skip-permissions"))
```

```elisp
;; Whether to switch to Claudemacs buffer when creating new session (default: t)
(setq claudemacs-switch-to-buffer-on-create nil)

;; Whether to switch to Claudemacs buffer when toggling visibility (default: t)
(setq claudemacs-switch-to-buffer-on-toggle nil)

;; Whether to switch to Claudemacs buffer when adding file references (default: nil)
(setq claudemacs-switch-to-buffer-on-file-add t)

;; Whether to switch to Claudemacs buffer when sending error fix requests (default: nil)
(setq claudemacs-switch-to-buffer-on-send-error t)
```

```elisp
;; Swap RET and M-RET behavior in Claudemacs buffers (default: nil)
;; When enabled: RET creates newline, M-RET submits
(setq claudemacs-m-return-is-submit t)

;; Enable Shift-Return to create newlines (default: t) 
;; Provides alternative to M-RET for creating newlines
(setq claudemacs-shift-return-newline t)
```

#### System Notifications

```elisp
;; Whether to show system notifications when Claude is awaiting input (default: t)
(setq claudemacs-notify-on-await t)

;; Sound to use for macOS notifications (default: "Submarine")
;; Available sounds: Basso, Blow, Bottle, Frog, Funk, Glass, Hero, Morse, 
;; Ping, Pop, Purr, Sosumi, Submarine, Tink
(setq claudemacs-notification-sound-mac "Ping")
```

All variables can also be customized via `M-x customize-group RET claudemacs RET`.

## Buffer Naming

Claudemacs creates workspace-aware buffer names:
- With workspace: `*claudemacs:workspace-name*`
- Without workspace: `*claudemacs:/path/to/project*`

Currently supports Doom Emacs workspaces and Perspective mode. If you use another workspace package and would like support added, please open an issue - I'm happy to add support for others.

## Tips and Tricks

### Using eat-mode effectively

When interacting with the eat-mode buffer, you are limited in what you can do in the default semi-char mode.

Press `C-c C-e` to enter emacs mode. A box cursor will appear, which you can use to move around and select and kill text.
Press `C-c C-j` to re-enter semi-char mode and continue typing to Claude.

### Scroll-popping, input box sticking, input box border draw issues

There is a tricky interaction between Eat-mode and Claude Code, probably because Claude Code uses some input libraries that eat has trouble with. It was causing the eat-mode buffer to "scroll-pop" to the top whenever you change the other window's buffer. This is mostly fixed now, but a side effect is sometimes the Claude Clode input box gets stuck halfway up the buffer and won't move.

There are also issues with drawing the input box border after the window resizes, which is expected of terminal programs. 

If you see these issues, press `u` in the Claudemacs transient menu to "unstick" the buffer, and everything should get reset.

### Buffer Toggle Edge Case

Normally, toggling the Claudemacs buffer will close its window, if the window was created for the Claudemacs session. Toggling it again will recreate the window.

But there's an edge case to be aware of: if a window was originally created for Claudemacs, but you've since switched to another workspace and back, that window may have shown other buffers in the meantime. In this case, the window is no longer considered "created just for Claudemacs" and won't automatically close when you toggle. This is due to Emacs' window management - once a window has been reused for other content, it loses its original "dedicated" status.

## Requirements

- Emacs 28.1+
- [eat](https://github.com/kephale/emacs-eat) package
- [Claude Code CLI](https://docs.anthropic.com/en/docs/claude-code/overview)

## Credits

Inspired by:
- [Aidermacs](https://github.com/MatthewZMD/aidermacs) by Matthew Zeng
- [claude-code.el](https://github.com/stevemolitor/claude-code.el) by Steve Molitor

## License

MIT License. See LICENSE file for details.
