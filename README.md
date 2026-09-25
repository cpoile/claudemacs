# Claudemacs

AI pair programming with [Claude Code](https://docs.anthropic.com/en/docs/claude-code/overview) in Emacs.

https://github.com/user-attachments/assets/a7a8348d-471c-4eec-85aa-946c3ef9d364

## What makes this project different? Simplicity
- Let your LLM cli shine in the terminal
- No agents, MCP, or IDE integration -- these eat up context

## Features

- **Multi-tool support**: Use Claude, Codex, Gemini, or other AI coding tools via configurable tool registry
- **Tool profiles**: Run one CLI under several registry entries — separate configs, switches, or accounts — each labeled in the menu (see [Tool Registry](#tool-registry))
- **Per-tool environment**: Scope environment variables like `CODEX_HOME` to a single entry's sessions
- **Codex background server**: Connect Codex 0.157.0 sessions to its shared local server while switching models in the start menu (see [Codex 0.157.0 background server](#codex-01570-background-server))
- **Selectable terminal backends**: Use Ghostel when available, with Eat as the fallback
- **Multiple instances**: Run multiple sessions of the same tool per workspace (claude, claude-2, etc.)
- **Broadcast to all sessions**: Use `C-u` prefix to send actions to all active sessions
- **Workspace-aware sessions**: Project-based sessions with Doom/Perspective workspace support (see [Sessions](#workspace-and-project-aware-sessions))
- **Session management**: Switch between sessions, switch to "other" session, kill specific sessions
- **Session list**: Inspect live workspaces, tool instances, projects, and authoritative session IDs in one table
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
  - [Terminal Backends](#terminal-backends)
  - [System Notifications](#system-notifications)
  - [Fonts](#fonts)
- [Usage](#usage)
  - [Workspace and Project-aware Sessions](#workspace-and-project-aware-sessions)
  - [Commands](#commands)
  - [Customization](#customization)
    - [Tool Registry](#tool-registry)
      - [Which CLI an entry runs (`:tool`)](#which-cli-an-entry-runs-tool)
      - [Per-tool environment variables (`:env`)](#per-tool-environment-variables-env)
      - [Codex 0.157.0 background server](#codex-01570-background-server)
    - [Basic Configuration](#basic-configuration)
    - [Process Environment](#process-environment)
    - [Notification Settings](#notification-settings)
- [Buffer Naming](#buffer-naming)
- [Tips and Tricks](#tips-and-tricks)
  - [Using semi-char mode effectively](#using-semi-char-mode-effectively)
  - [Copy file path with line number](#copy-file-path-with-line-number)
  - [Scroll-popping, input box sticking, input box border draw issues](#scroll-popping-input-box-sticking-input-box-border-draw-issues)
  - [Buffer Toggle Edge Case](#buffer-toggle-edge-case)
- [Requirements](#requirements)
- [Credits](#credits)
- [License](#license)

## Installation

### Prerequisites

1. Install [Claude Code CLI](https://docs.anthropic.com/en/docs/claude-code/overview)
2. Install at least one supported terminal backend in Emacs:
   - [Eat](https://codeberg.org/akib/emacs-eat) (the fallback backend)
   - [Ghostel](https://github.com/dakra/ghostel), including its required native module

Claudemacs only loads the selected backend.  You do not need both packages,
but the selected backend must be installed and loadable before starting a
session.

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

Use your preferred keybinding (I use `C-c C-e`). I'd recommend adding it to
the relevant mode maps instead of using a `global-set-key`, since a global
binding can override useful keybindings in the selected terminal's mode map
(see [Using the terminal backend](#using-the-terminal-backend) below).

```elisp
(require 'claudemacs)
(define-key prog-mode-map (kbd "C-c C-e") #'claudemacs-transient-menu)
(define-key emacs-lisp-mode-map (kbd "C-c C-e") #'claudemacs-transient-menu)
(define-key text-mode-map (kbd "C-c C-e") #'claudemacs-transient-menu)
(define-key python-base-mode-map (kbd "C-c C-e") #'claudemacs-transient-menu)

;; Set a big buffer so we can search our history.
;; This option applies when using the Eat backend.
(with-eval-after-load 'eat
  (setq eat-term-scrollback-size 400000))
```

### Terminal Backends

Claudemacs presents the same session and action commands regardless of the
terminal implementation.  Ghostel is selected automatically when its package
is available on `load-path`; otherwise Claudemacs falls back to Eat.  To choose
explicitly, set the backend before creating a session:

```elisp
;; Force Eat:
(setq claudemacs-terminal-backend 'eat)

;; Force Ghostel (requires the Ghostel package and native module):
(setq claudemacs-terminal-backend 'ghostel)

;; Ghostel Claudemacs sessions confirm before killing a live process (default: t)
;; Other values are `auto' (only while a command is running) and nil (never).
(setq claudemacs-ghostel-query-before-killing t)

;; Increase this if programmatically sent prompts occasionally fail to submit.
(setq claudemacs-ghostel-submit-delay 0.15)
```

The selector is checked when a session starts and affects new sessions only.
Kill and restart existing sessions after changing it.  Claudemacs does not
automatically fall back to another backend: an invalid, unavailable, or
unloadable selection reports an error so the configuration can be corrected.

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

Claude Code's `iterm2` and `ghostty` channels work too, and they carry Claude's
own notification text rather than just the event — see [tool messages in
notifications](#tool-messages-in-notifications) below.

#### Tool messages in notifications

A terminal bell announces only that something happened.  Tools can instead
announce a completion as an OSC 9 or OSC 777 escape sequence, which carries the
notification text along with it, and Claudemacs shows that text as the body of
the system notification, e.g.:

> **Codex**
> Renamed the helper and ran the tests; all 81 pass.

By default, Claudemacs sets Codex's TUI notification path to OSC 9 and makes it
notify even while the session is focused.  For Codex 0.157.0 background server
sessions, put those settings in `config.toml` instead; see
[the setup below](#codex-01570-background-server).  Codex sends response text
verbatim, which Claudemacs truncates to fit the notification.  This is separate
from Codex's top-level `notify` hook, which runs an external command.

For Claude Code, set `preferredNotifChannel` to `iterm2` or `ghostty` instead of
`terminal_bell`.  `iterm2_with_bell` also works — it announces the same event
twice, and Claudemacs drops the duplicate.  The `kitty` channel uses OSC 99,
which Claudemacs does not read.

Set `claudemacs-notify-with-tool-message` to nil to always use the generic
"awaiting your input" text.  Tools that only ring the bell have no message to
send, so their notifications keep the generic text either way.

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

Windows requires a registered application identity before an unpackaged app can send Notification Center toasts.  Claudemacs installs this per-user identity automatically the first time it needs to send a notification; no manual setup is required.

If the identity ever needs to be repaired or reinstalled, run:

```text
M-x claudemacs-setup-windows-notifications
```

Windows notifications expire after five seconds by default.  To change that:

```elisp
(setq claudemacs-notification-timeout-windows 8)
```

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
- `l` - List live sessions
- `k` - Kill session (select from active sessions)
- `t` - Toggle buffer visibility

**Start/Resume Submenus** (`S` and `r`)

These open a submenu where you can select which tool to start/resume:
- `1` - First tool in registry (default: claude)
- `2` - Second tool (default: codex)
- `3` - Third tool (default: gemini)
- `RET` - Start/resume default tool

Historical sessions in the resume picker are ordered by most recent use, with
the newest session at the top.

Switches available in submenus:
- `-d` - Skip permissions on start (`--dangerously-skip-permissions` or equivalent)
- `-p` - Prompt for project root directory
- `-f` - Add custom command-line arguments (prompts for input)
- `-u` - In the resume submenu, resume a Codex session with a UUID (prompts for the UUID)

**Action Commands** (use `C-u` prefix to send to all sessions)
- `e` - Fix error at point (using flycheck if available)
- `x` - eXecute request with file context (current line or region)
- `X` - eXecute request with no context
- `i` - Implement comment (extracts comment text and asks Claude to implement it)
- `f` - Add file reference (@file) to conversation
- `F` - Add current file reference to conversation
- `a` - Add context (sends file:line or file:line-range without newline)

When `x` or `X` is used from an active Ediff buffer, set `claudemacs-open-new-frame-for-ediff-requests` to non-nil to preserve the Ediff window layout.  Claudemacs shows the session in another frame; if the session is already displayed in a window, that window is reused.  The default is nil, so requests otherwise retain the normal behavior.

**Quick Responses**
- `y` - Send Yes (RET)
- `n` - Send No (ESC)

**Maintenance**
- `u` - Unstick Claude input box (reset buffer tracking)

**Additional M-x commands:**
- `M-x claudemacs-setup` - Re-run setup (hooks/advice)
- `M-x claudemacs-setup-bell-handler` - Re-setup notification handler

### Session list

Run `M-x claudemacs-session-list` (or press `l` in the Claudemacs transient) to view currently live Claudemacs buffers in one table. Each row represents a running terminal-backed session and shows three columns: workspace, tool instance, and project directory. Session IDs remain internal row keys for visiting the live session.

Press `RET` to visit the selected live session and `g` to refresh the list. Sessions whose terminal process has exited disappear on refresh; this command does not display CLI history or historical rows. If Claudemacs cannot determine a session ID safely, it keeps that row's identity as `unknown` rather than guessing from recency or the project directory. New or forked Codex sessions remain `unknown` internally; explicit Codex resumes retain the selected authoritative ID.


### Customization

Claudemacs provides several customization variables to tailor the experience to your workflow:

#### Tool Registry

Configure which AI coding tools are available:

```elisp
;; Default registry includes Claude, Codex, and Gemini
(setq claudemacs-tool-registry
  '((claude :label "Claude" :program "claude" :switches nil
            :model-types (("opus-high" :model "opus" :effort "high")
                          ("sonnet-high" :model "sonnet" :effort "high")))
    (codex :label "Codex" :program "codex" :switches nil
           :model-types (("luna-max" :model "gpt-5.6-luna" :effort "max")
                         ("sol-high" :model "gpt-5.6-sol" :effort "high")))
    (gemini :label "Gemini" :program "gemini-cli" :switches nil)))

;; Set the default tool (default: 'claude)
(setq claudemacs-default-tool 'claude)
```

The optional `:label` is the display name shown before the session name in the
start and resume menus, so a tool reads as `1. Claude - claude-2 (opus-high)
(default)`.  Omit `:label` to show only the session name (`1. claude-2 ...`).

##### Which CLI an entry runs (`:tool`)

Each registry key is one menu entry and one session identity, so **keys must be
unique**.  The optional `:tool` says which CLI the entry actually runs —
`claude`, `codex`, `gemini`, etc. — and that family, not the key, selects every
CLI-specific behavior: notification switches, resume and fork arguments, `-d`
translation, and session-identity tracking.

When `:tool` is omitted it is inferred, first from `:program` and then from the
key.  Inference handles paths and conventional names (`/usr/bin/codex`,
`codex.exe`, `codex-personal` are all Codex), so most entries never need it.
Set `:tool` when `:program` is an alias or wrapper whose name reveals nothing
(`cdx`, `~/bin/work-cli`).  An entry matching nothing is a generic tool: it
starts and sends text, without CLI-specific handling.

Duplicate keys warn when the menu is built, since only the first entry for a
key is ever used.

##### Per-tool environment variables (`:env`)

The optional `:env` is a list of `"VAR=VALUE"` strings applied to that tool's
sessions only.  They take precedence over `claudemacs-process-environment`
(which applies to every tool) and over the environment Emacs itself was started
with, and they are not exported into Emacs — only the session's process sees
them.

Claudemacs applies an entry's `:env` to its own lookups too, not just to the
session process: the configured model shown in the menu, and the session
history offered when resuming or branching, both resolve `CODEX_HOME` /
`CLAUDE_CONFIG_DIR` from `:env` first.  So a profile shows its own model and
lists its own past sessions, never the default installation's.

For Codex profiles, setting `CODEX_HOME` also scopes `CODEX_SQLITE_HOME` to
that directory unless the profile sets `CODEX_SQLITE_HOME` explicitly.  This
prevents an ambient SQLite override from mixing histories between profiles.

Values are used **literally**: `:program` and `:env` never go through a shell,
so `"$HOME"` and `"~"` are not expanded.  Build paths with `expand-file-name`
and a backquoted registry:

```elisp
;; Two Codex registry entries, each with its own CODEX_HOME
(setq claudemacs-tool-registry
  `((codex-work :label "Codex work" :program "codex"
                :switches ("--approve-for-me"))
    (codex-pers :label "Codex pers" :program "codex"
                :switches ("--approve-for-me")
                :env (,(concat "CODEX_HOME=" (expand-file-name "~/.codex-personal"))))
    (claude :label "Claude work" :program "claude"
            :switches ("--verbose"))))
```

That produces:

```
1. Codex work - codex-work (default)
2. Codex pers - codex-pers
3. Claude work - claude
```

A malformed entry (not a `"VAR=VALUE"` string) signals an error when the
session starts rather than launching the tool with an unintended environment.

The configured model is shown beside each tool in the start-session menu by
default, along with the `m` (`Toggle model type`) menu item.  Disable it with:

```elisp
(setq claudemacs-show-model-in-menu nil)
```

When enabled, Claudemacs reads Codex's default `model` and
`model_reasoning_effort` from `config.toml` under `CODEX_HOME` (`~/.codex` by
default).  It reads Claude Code's optional `model` and `effortLevel` from
`settings.json` under `CLAUDE_CONFIG_DIR` (`~/.claude` by default), falling back
to the current `sonnet` alias when no model is configured.  Both directories
come from the entry's `:env` when it sets them.  The model text is
shown in comment-face.  Press `m` in the start menu to cycle the configured
model types from each tool's configuration; the selection lasts only for the
current menu invocation and applies only to newly started sessions.

Model types are `(NAME PLIST)` entries in each tool's registry plist.  The
standard `:model` and `:effort` keys generate Claude Code and Codex command
line switches.  An explicit `:switches` list replaces those generated switches
for that choice.  Use explicit Codex `--profile` switches for the background
server setup below.

##### Codex 0.157.0 background server

[Codex CLI 0.157.0](https://learn.chatgpt.com/docs/changelog) added automatic
background server startup for eligible interactive sessions.  In 0.157.0,
launching with a custom `--profile` or Claudemacs' default Codex notification
`--config` switches can instead show **"Running without the shared background
server"**.  Selecting a Codex profile alone does not guarantee a shared server.

For a local Unix socket on macOS or Linux, use an explicit
[`--remote unix://`](https://learn.chatgpt.com/docs/developer-commands)
connection.  With this setting, Claudemacs runs `codex app-server daemon start`
for that entry's `CODEX_HOME` before opening the terminal; an already running
managed server is reused.  It also passes the project directory with `--cd`,
so a new session opens the selected project.  Separate `CODEX_HOME` values use
separate servers.  This setup requires Codex CLI 0.157.0 or later.

1. Put the TUI notification settings in `$CODEX_HOME/config.toml` (normally
   `~/.codex/config.toml`), under its existing `[tui]` table if it has one:

   ```toml
   [tui]
   notification_method = "osc9"
   notification_condition = "always"
   ```

2. Create a [Codex profile file](https://learn.chatgpt.com/docs/config-file/config-advanced)
   for each model choice next to `config.toml`.  For example,
   `~/.codex/sol-high.config.toml` contains:

   ```toml
   model = "gpt-6-sol"
   model_reasoning_effort = "high"
   ```

   And `~/.codex/luna-max.config.toml` contains:

   ```toml
   model = "gpt-6-luna"
   model_reasoning_effort = "max"
   ```

3. Add this to your Emacs config:

   ```elisp
   (setq claudemacs-codex-notification-switches nil
         claudemacs-tool-registry
         '((codex :label "Codex" :program "codex"
                  :switches ("--remote" "unix://")
                  :model-types (("sol-high" :model "gpt-6-sol" :effort "high"
                                 :switches ("--profile" "sol-high"))
                                ("luna-max" :model "gpt-6-luna" :effort "max"
                                 :switches ("--profile" "luna-max"))))))
   ```

   If an entry sets `CODEX_HOME` in `:env`, put its base config and profile files
   there instead.  Repeat steps 1 and 2 for each Codex home you use.

The `:model` and `:effort` fields keep Claudemacs' model menu readable; the
`--profile` switches select the actual Codex settings.  Press `m` in the start
menu to choose a model for a **new** session.  Existing sessions keep their
current connection and model.  Users who leave the registry and notification
settings at their defaults retain the usual Codex launch behavior; this local
server setup is opt-in.

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

;; Show x/X request sessions in a separate frame when invoked from Ediff (default: nil)
(setq claudemacs-open-new-frame-for-ediff-requests t)

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

#### Process Environment

Customize environment variables passed to Claude processes:

```elisp
;; Default enables 24-bit truecolor for Claude Code's syntax highlighting
(setq claudemacs-process-environment
  '("TERM=xterm-256color" "COLORTERM=truecolor"))

;; Add additional environment variables as needed
(add-to-list 'claudemacs-process-environment "CLAUDE_CODE_SYNTAX_HIGHLIGHT=off")
```

#### Notification Settings

```elisp
;; Whether to show system notifications when Claude is awaiting input (default: t)
(setq claudemacs-notify-on-await t)

;; Codex notification switches are enabled by default so notifications reach
;; the selected terminal backend.  For the Codex 0.157.0 background server
;; setup above, set this to nil and put the TUI notification settings in
;; $CODEX_HOME/config.toml instead.
;; (setq claudemacs-codex-notification-switches nil)

;; Whether a notification shows the message the tool sent with it -- for Codex,
;; normally the last thing the agent said (default: t)
;; Set to nil for the generic "awaiting your input" text.
(setq claudemacs-notify-with-tool-message t)

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
- `TOOL` is the registry key (claude, codex, gemini, or a profile key like `codex-pers`)
- `-N` is the instance number (omitted for first instance)
- `SESSION-ID` is the workspace name or project path

Only a trailing number is read as an instance, so a hyphenated profile key stays
intact: `*claudemacs:codex-pers-2:workspace-name*` is instance 2 of `codex-pers`.

Currently supports Doom Emacs workspaces and Perspective mode. Open an issue if you use another workspace package.

## Tips and Tricks

### Using the terminal backend

The terminal backend provides a semi-character mode for typing directly to
the AI tool and an Emacs mode for editing terminal contents.  The keybindings
below apply to both Eat and Ghostel.

#### Using semi-char mode effectively

When interacting with the terminal buffer, you are limited in what you can do
in the default semi-char mode.

Press `C-c C-e` to enter emacs mode. A box cursor will appear, which you can use to move around and select and kill text.
Press `C-c C-j` to re-enter semi-char mode and continue typing to Claude.

Press `C-v` in semi-char mode to send it to Claude Code, which uses it to paste an image from the clipboard.

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

### Sending keycodes to Claude Code

Claude Code uses some keybindings that conflict with Emacs defaults. For example, `Ctrl-g` is used by Claude Code to edit your plan in your editor, but in Emacs `C-g` is the universal quit key (which claudemacs maps to send ESC to the terminal). In semi-char mode, both Eat and Ghostel provide `C-q` as a quoted-input escape hatch:

Use it to send the actual `Ctrl-g` character (or any other control character) to the terminal:

- `C-q C-g` - Send Ctrl-g to Claude Code (e.g., to edit your plan)
- `C-q C-c` - Send Ctrl-c to Claude Code
- `C-q <any-key>` - Send that key literally to the terminal

### Scroll-popping, input box sticking, input box border draw issues

There can be a tricky interaction between a terminal emulator and Claude Code,
because Claude Code uses input libraries that depend on terminal behavior.  In
Eat-mode this can cause the buffer to "scroll-pop" to the top whenever you
change the other window's buffer.  This is mostly fixed now, but a side effect
is sometimes the Claude Code input box gets stuck halfway up the buffer and
won't move.

There are also issues with drawing the input box border after the window
resizes, which is expected of terminal programs.

If you see these issues, press `u` in the Claudemacs transient menu to
"unstick" the buffer, and everything should get reset.  The same command is
available for both supported backends.

### Buffer Toggle Edge Case

Normally, toggling the Claudemacs buffer will close its window, if the window was created for the Claudemacs session. Toggling it again will recreate the window.

But there's an edge case to be aware of: if a window was originally created for Claudemacs, but you've since switched to another workspace and back, that window may have shown other buffers in the meantime. In this case, the window is no longer considered "created just for Claudemacs" and won't automatically close when you toggle. This is due to Emacs' window management - once a window has been reused for other content, it loses its original "dedicated" status.

## Requirements

- Emacs 28.1+
- [Eat](https://codeberg.org/akib/emacs-eat) package, or [Ghostel](https://github.com/dakra/ghostel) with its native module
- [transient](https://github.com/magit/transient) (built-in since Emacs 28)
- [Claude Code CLI](https://docs.anthropic.com/en/docs/claude-code/overview) (or other supported tool)

## Credits

Inspired by:
- [Aidermacs](https://github.com/MatthewZMD/aidermacs) by Matthew Zeng
- [claude-code.el](https://github.com/stevemolitor/claude-code.el) by Steve Molitor

## License

MIT License. See LICENSE file for details.
