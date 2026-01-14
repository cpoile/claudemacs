# Claudemacs

AI pair programming with [Claude Code](https://docs.anthropic.com/en/docs/claude-code/overview) in Emacs.

https://github.com/user-attachments/assets/a7a8348d-471c-4eec-85aa-946c3ef9d364

## What makes this project different? Simplicity
- Let Claude Code shine in the terminal
- No agents, MCP, or IDE integration -- these eat up context

## Features

- **Multi-tool support**: Use Claude, Codex, Gemini, or other AI coding tools via configurable tool registry
- **Multiple instances**: Run multiple sessions of the same tool per workspace (claude, claude-2, etc.)
- **Broadcast to all sessions**: Use `C-u` prefix to send actions to all active sessions
- **Workspace-aware sessions**: Project-based sessions with Doom/Perspective workspace support (see [Sessions](#workspace-and-project-aware-sessions))
- **Session management**: Switch between sessions, switch to "other" session, kill specific sessions
- **System notifications**: OS notifications with sound when awaiting input (see [System Notifications](#system-notifications))
- **Terminal fixes**: Use `u` to unstick input box and reset buffer issues (see [Tips](#tips-and-tricks))
- **Session resume**: Resume previous sessions with tool-specific resume flags
- **Execute request with context**: Send request with file and line/region context
- **Fix error at point**: Send flycheck error to Claude with context
- **Implement comment at point**: Extract comment text and ask Claude to implement it
- **Add file or current file**: Add files with Claude's @ symbol convention
- **C-g sends Esc**: Old habits die hard
- **Option: Swap RET and M-RET**: Optionally swap keys (Claude maps RET to submit, M-RET to newline)
- **Option: S-RET as newline**: May be more natural
- **Option: Shell environment loading**: Load shell rc files for PATH and environment variables
- **Transient interface**: Easy-to-use menu system (default: `C-c C-e`)

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
    - [Tool Registry](#tool-registry)
    - [Basic Configuration](#basic-configuration)
    - [System Notifications](#system-notifications)
- [Buffer Naming](#buffer-naming)
- [Tips and Tricks](#tips-and-tricks)
  - [Using eat-mode effectively](#using-eat-mode-effectively)
  - [Copy file path with line number](#copy-file-path-with-line-number)
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

#### use-package with built-in :vc (Emacs 30+)

```elisp
(use-package claudemacs
  :vc (:url "https://github.com/cpoile/claudemacs"))
```

#### use-package with vc-use-package

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

Now you should receive System notifications when Claude Code is waiting for input, or when done.

Unfortunately, clicking on the notification doesn't bring you to Emacs. Open to ideas on how to fix that.

#### -- Linux --

For Linux systems using `notify-send`, notifications will automatically dismiss by default instead of persisting in the system tray. You can control this behavior with:

```elisp
;; Auto-dismiss notifications (default: t)
(setq claudemacs-notification-auto-dismiss-linux t)

;; Keep notifications in system tray
(setq claudemacs-notification-auto-dismiss-linux nil)

;; Play sound with notifications (requires canberra-gtk-play)
;; Common sound IDs: "message-new-instant", "bell", "dialog-error", "dialog-warning"
(setq claudemacs-notification-sound-linux "message-new-instant")

;; Disable sound
(setq claudemacs-notification-sound-linux "")
```

#### -- Windows --

I have not tested on windows, so would appreciate any help there (PRs welcome).

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

#### -- Linux --

``` elisp
(defun my/setup-custom-font-fallbacks-linux ()
  (interactive)
  "Configure font fallbacks on linux for symbols and emojis.
This will need to be called every time you change your font size,
to load the new symbol and emoji fonts."

  (setq use-default-font-for-symbols nil)

  ;; --- Configure for 'symbol' script ---
  ;; We add fonts one by one. Since we use 'prepend',
  ;; the last one added here will be the first one Emacs tries.
  ;; So, list them in reverse order of your preference.

  ;; Least preferred among this list for symbols (will be at the end of our preferred list)
  ;; (set-fontset-font t 'symbol "FreeSerif" nil 'prepend)
  ;; (set-fontset-font t 'symbol "NotoSansSymbols2" nil 'prepend)
  ;; (set-fontset-font t 'symbol "NotoSansCJKJP" nil 'prepend)
  ;; (set-fontset-font t 'symbol "unifont" nil 'prepend)
  (set-fontset-font t 'symbol "DejaVu Sans Mono" nil 'prepend)
  ;; Most preferred for symbols -- use your main font here
  (set-fontset-font t 'symbol "JetBrainsMono Nerd Font Mono" nil 'prepend)


  ;; --- Configure for 'emoji' script ---
  ;; Add fonts one by one, in reverse order of preference.

  ;; Least preferred among this list for emojis
  ;; (set-fontset-font t 'emoji "FreeSerif" nil 'prepend)
  ;; (set-fontset-font t 'emoji "NotoSansSymbols2" nil 'prepend)
  ;; (set-fontset-font t 'emoji "NotoSansCJKJP" nil 'prepend)
  ;; (set-fontset-font t 'emoji "unifont" nil 'prepend)
  (set-fontset-font t 'emoji "DejaVuSans" nil 'prepend)
  ;; (set-fontset-font t 'emoji "Noto Emoji" nil 'prepend) ;; If you install Noto Emoji
  ;; Most preferred for emojis -- use your main font here
  (set-fontset-font t 'emoji "JetBrainsMono Nerd Font Mono" nil 'prepend)
  )

;; to test if you have a font family installed:
;;   (find-font (font-spec :family "DejaVu Sans Mono"))

;; Then, add the fonts after your setup is complete:
(add-hook 'emacs-startup-hook
          (lambda ()
            (progn
              (when (string-equal system-type "gnu/linux")
                  (my/setup-custom-font-fallbacks-linux)))))

```

#### -- Windows --

I'm not sure of the built in fonts for Windows, or which ones should be used as fallbacks for Claude Code. PRs welcome.

## Usage

### Workspace and Project-aware Sessions

--- Session Names ---

- The Claudemacs session is based on Doom/Perspective workspace, and the Claude Code's `cwd` is the project's git-root (by default -- see below). 
- Why?
  - This allows you to have multiple workspaces in a monorepo, and a separate Claudemacs session per workspace, but each session will be correctly rooted to the project's git root.
- If you don't use workspaces, the decision sequence is: Doom workspace -> Perspective name -> project root dir name
- Supports: Doom Emacs workspaces, perspective.el (vanilla), and fallback to project root

--- Claude Code CWD ---

- You can make Claude Code use your projectile root as it's `cwd` by setting:

``` elisp
(setq claudemacs-prefer-projectile-root t)
```

- Why?
  - Claude Code is forbidden to auto-edit or auto-read files outside its `cwd`. This is annoying if you have the following repo structure:
  
```
monorepo/
├── backend/
│   ├── .git/
│   └── api/
│       └── server.py
└── frontend/
    ├── .git/
    └── src/
        └── app.tsx
```

By putting a `.projectile` file in the parent and using `(setq claudemacs-prefer-projectile-root t)`, Claude Code will be able to read and edit all files, like so:

```
monorepo/
├── .projectile
├── backend/
│   ├── .git/
│   └── api/
│       └── server.py
└── frontend/
    ├── .git/
    └── src/
        └── app.tsx
```

🎉 NOTE: Since implementing this feature there was a new Claude Code improvement that let's you manually add directories to a project's safe list. You could add them like:

``` elisp
(setq claudemacs-program-switches '("--add-dir ../apps ../libs"))
```

You could also add them to a project's `.dir-locals.el` and have it customized per project.


### Commands

Claudemacs provides a transient menu accessible via `C-c C-e` (or your own keybinding):

**Core Commands**
- `s` - Switch to session (or select from multiple)
- `S` - Start Session submenu (select tool, with switches)
- `o` - Switch to other session (second most recent)
- `r` - Resume Session submenu (select tool to resume)
- `k` - Kill session (select from active sessions)
- `t` - Toggle buffer visibility

**Start/Resume Submenus** (`S` and `r`)

These open a submenu where you can select which tool to start/resume:
- `1` - First tool in registry (default: claude)
- `2` - Second tool (default: codex)
- `3` - Third tool (default: gemini)
- `RET` - Start/resume default tool

Switches available in submenus:
- `-d` - Skip permissions on start (`--dangerously-skip-permissions` or equivalent)
- `-p` - Prompt for project root directory
- `-f` - Add custom flag (prompts for input)

**Action Commands** (use `C-u` prefix to send to all sessions)
- `e` - Fix error at point (using flycheck if available)
- `x` - eXecute request with file context (current line or region)
- `X` - eXecute request with no context
- `i` - Implement comment (extracts comment text and asks Claude to implement it)
- `f` - Add file reference (@file) to conversation
- `F` - Add current file reference to conversation
- `a` - Add context (sends file:line or file:line-range without newline)

**Quick Responses**
- `y` - Send Yes (RET)
- `n` - Send No (ESC)

**Maintenance**
- `u` - Unstick Claude input box (reset buffer tracking)

**Additional M-x commands:**
- `M-x claudemacs-setup` - Re-run setup (hooks/advice)
- `M-x claudemacs-setup-bell-handler` - Re-setup notification handler

### Customization

Claudemacs provides several customization variables to tailor the experience to your workflow:

#### Tool Registry

Configure which AI coding tools are available:

```elisp
;; Default registry includes Claude, Codex, and Gemini
(setq claudemacs-tool-registry
  '((claude :program "claude" :switches nil)
    (codex :program "codex" :switches nil)
    (gemini :program "gemini-cli" :switches nil)))

;; Add a custom tool or modify switches
(setq claudemacs-tool-registry
  '((claude :program "claude" :switches ("--verbose"))
    (codex :program "codex" :switches nil)
    (aider :program "aider" :switches ("--no-auto-commits"))))

;; Set the default tool (default: 'claude)
(setq claudemacs-default-tool 'claude)
```

#### Basic Configuration

```elisp
;; Fallback executable path if not in tool registry (default: "claude")
(setq claudemacs-program "/usr/local/bin/claude")

;; Fallback switches if not in tool registry (default: nil)
(setq claudemacs-program-switches '("--verbose"))
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

;; Whether to switch to Claudemacs buffer when adding context (default: t)
(setq claudemacs-switch-to-buffer-on-add-context nil)

;; Whether to prefer projectile root over git root when available (default: nil)
(setq claudemacs-prefer-projectile-root t)
```

```elisp
;; Swap RET and M-RET behavior in Claudemacs buffers (default: nil)
;; When enabled: RET creates newline, M-RET submits
(setq claudemacs-m-return-is-submit t)

;; Enable Shift-Return to create newlines (default: t)
;; Provides alternative to M-RET for creating newlines
(setq claudemacs-shift-return-newline t)
```

```elisp
;; Run Claude through interactive shell to load shell environment (default: nil)
;; When enabled, Claude is invoked through your shell (e.g., zsh -i -c "claude ...")
;; which sources rc files like .zshrc or .bashrc, making shell-configured PATH
;; and environment variables available to Claude.
;; Useful if Claude can't find commands that are in your shell's PATH.
;; NOTE: Changes only apply to new sessions - kill and restart to take effect.
(setq claudemacs-use-shell-env t)
```

#### System Notifications

```elisp
;; Whether to show system notifications when Claude is awaiting input (default: t)
(setq claudemacs-notify-on-await t)

;; Sound to use for macOS notifications (default: "Submarine")
;; Available sounds: Basso, Blow, Bottle, Frog, Funk, Glass, Hero, Morse, 
;; Ping, Pop, Purr, Sosumi, Submarine, Tink
(setq claudemacs-notification-sound-mac "Ping")

;; Auto-dismiss Linux notifications instead of persisting to system tray (default: t)
(setq claudemacs-notification-auto-dismiss-linux nil)

;; Sound for Linux notifications using canberra-gtk-play (default: "bell")
;; Common sound IDs: "message-new-instant", "bell", "dialog-error", "dialog-warning"
(setq claudemacs-notification-sound-linux "message-new-instant")
```

All variables can also be customized via `M-x customize-group RET claudemacs RET`.

#### Startup Hook

Claudemacs provides a startup hook that runs after a session has finished initializing. Hook functions execute with the claudemacs buffer as the current buffer.

```elisp
;; Example: Custom initialization based on project type
(add-hook 'claudemacs-startup-hook
          (lambda ()
            (when (file-exists-p (expand-file-name "package.json" claudemacs--cwd))
              (message "Node.js project detected in %s. Do stuff." claudemacs--cwd))))

```

## Buffer Naming

Claudemacs creates workspace-aware buffer names that include the tool name:
- With workspace: `*claudemacs:claude:workspace-name*`
- Without workspace: `*claudemacs:claude:/path/to/project*`
- Multiple instances: `*claudemacs:claude-2:workspace-name*`

The format is `*claudemacs:TOOL(-N):SESSION-ID*` where:
- `TOOL` is the tool name (claude, codex, gemini, etc.)
- `-N` is the instance number (omitted for first instance)
- `SESSION-ID` is the workspace name or project path

Currently supports Doom Emacs workspaces and Perspective mode. Open an issue if you use another workspace package.

## Tips and Tricks

### Using eat-mode effectively

When interacting with the eat-mode buffer, you are limited in what you can do in the default semi-char mode.

Press `C-c C-e` to enter emacs mode. A box cursor will appear, which you can use to move around and select and kill text.
Press `C-c C-j` to re-enter semi-char mode and continue typing to Claude.

Press `C-v` to paste an image from the clipboard.

### Copy file path with line number

Useful helper to copy the current file path with line number (or range) for pasting into Claude:

```elisp
(defun copy-file-path-with-line ()
  "Copy the current file path with line number (or range) to clipboard.
If a region is active, uses the range of lines."
  (interactive)
  (let* ((file-path (buffer-file-name))
         (line-start (line-number-at-pos (if (use-region-p) (region-beginning) (point))))
         (line-end (when (use-region-p) (line-number-at-pos (region-end))))
         (result (if line-end
                     (format "%s:%d-%d" file-path line-start line-end)
                   (format "%s:%d" file-path line-start))))
    (if file-path
        (progn
          (kill-new result)
          (message "Copied: %s" result))
      (message "Buffer is not visiting a file"))))
```

Bind it to a key and use it to quickly copy file references to paste into Claude.

### Scroll-popping, input box sticking, input box border draw issues

There is a tricky interaction between Eat-mode and Claude Code, probably because Claude Code uses some input libraries that eat has trouble with. It was causing the eat-mode buffer to "scroll-pop" to the top whenever you change the other window's buffer. This is mostly fixed now, but a side effect is sometimes the Claude Clode input box gets stuck halfway up the buffer and won't move.

There are also issues with drawing the input box border after the window resizes, which is expected of terminal programs. 

If you see these issues, press `u` in the Claudemacs transient menu to "unstick" the buffer, and everything should get reset.

### Buffer Toggle Edge Case

Normally, toggling the Claudemacs buffer will close its window, if the window was created for the Claudemacs session. Toggling it again will recreate the window.

But there's an edge case to be aware of: if a window was originally created for Claudemacs, but you've since switched to another workspace and back, that window may have shown other buffers in the meantime. In this case, the window is no longer considered "created just for Claudemacs" and won't automatically close when you toggle. This is due to Emacs' window management - once a window has been reused for other content, it loses its original "dedicated" status.

## Requirements

- Emacs 28.1+
- [eat](https://codeberg.org/akib/emacs-eat) package
- [transient](https://github.com/magit/transient) (built-in since Emacs 28)
- [Claude Code CLI](https://docs.anthropic.com/en/docs/claude-code/overview) (or other supported tool)

## Credits

Inspired by:
- [Aidermacs](https://github.com/MatthewZMD/aidermacs) by Matthew Zeng
- [claude-code.el](https://github.com/stevemolitor/claude-code.el) by Steve Molitor

## License

MIT License. See LICENSE file for details.
