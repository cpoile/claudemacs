;;; claudemacs.el --- AI pair programming with Claude Code -*- lexical-binding: t; -*-
;; Author: Christopher Poile <cpoile@gmail.com>
;; Version: 0.5.1
;; Package-Requires: ((emacs "28.1") (transient "0.4.0"))
;; Keywords: claudecode ai emacs llm ai-pair-programming tools
;; URL: https://github.com/cpoile/claudemacs
;; SPDX-License-Identifier: MIT

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Claudemacs integrates with Claude Code (https://docs.anthropic.com/en/docs/claude-code/overview)
;; for AI-assisted programming in Emacs using a selectable terminal backend.
;;
;; Inspired by Aidermacs: https://github.com/MatthewZMD/aidermacs and
;; claude-code.el: https://github.com/stevemolitor/claude-code.el

;;; Changelog:

;; Version 0.5.1 (2026-09-25)
;; - Codex 0.157.0 shared background server support: registry entries using
;;   `--remote unix://' start or reuse the managed server for their `CODEX_HOME'
;;   and pass the selected project with `--cd'.  Named `--profile' model choices
;;   can use that connection; the README documents the required setup.

;; Version 0.5.0 (2026-09-25)
;; - Selectable Eat and Ghostel terminal backends, with backend ownership
;;   captured per session so both can coexist; Ghostel is preferred by default
;;   when its package is available
;; - Windows is fully supported (thanks to Ghostel)
;; - Windows completion alerts use registered, non-modal Notification Center
;;   toasts with a one-time setup command
;; - Start-session menus show configured model/effort and let `m' cycle through
;;   per-tool model types for newly started sessions
;; - Tool profiles: one CLI can run under several registry entries with their
;;   own switches, models, and configuration directories.  Registry entries
;;   accept three new keys:
;;   - `:label' names the entry in the start and resume menus, shown before
;;     the session name as "Codex work - codex-work"
;;   - `:tool' states which CLI the entry runs (`claude', `codex', `gemini',
;;     etc.).  Every CLI-specific behavior now follows it instead of the
;;     registry key.  It is inferred from `:program' and then from the key,
;;     so existing configurations are unchanged and only an alias or wrapper
;;     `:program' needs to state it
;;   - `:env' sets environment variables for that entry's sessions only, such
;;     as a per-profile `CODEX_HOME'.  Claudemacs's own lookups honor it as
;;     well: the model shown in the menu and the history offered when
;;     resuming or branching both resolve `CODEX_HOME' and
;;     `CLAUDE_CONFIG_DIR' from `:env', so a profile reports its own
;;     configuration and lists its own sessions
;; - Duplicate `claudemacs-tool-registry' keys now warn; entries are looked up
;;   by key, so only the first entry for a repeated key was ever used
;; - Fixed session lookup for registry keys containing a hyphen, which
;;   previously failed to parse out of the session buffer name
;; - Start and resume menus accept custom command-line arguments via `-f';
;;   Codex sessions can also be resumed directly by UUID with `-u'
;; - System notifications show the message the tool sent with them -- for
;;   Codex, normally the last thing the agent said -- instead of generic
;;   "awaiting your input" text.  Codex is configured for this automatically;
;;   Claude Code needs `preferredNotifChannel' set to `iterm2' or `ghostty'.
;;   See `claudemacs-notify-with-tool-message'
;; - Codex waiting-for-input notifications notify even while the session is
;;   focused
;; - Resume session lists sessions by most recently used
;; - Live session list with authoritative Claude/Codex identities
;; - Optional separate frame for `x' and `X' requests made while using Ediff
;;   (which means: ask questions about a review without leaving Ediff
;;    or ruining your window layout)

;; Version 0.4.0 (2026-07-10)
;; - New `claudemacs-branch-session' command for forking the current Claude or
;;   Codex session, with session-specific UUID tracking and selection
;; - No more Eat-mode expensive blinking-cursor redraws for Codex sessions
;; - Optional Consult integration adds live previews when switching sessions
;; - Aborted request minibuffers now save their contents for recall with `M-p'
;; - Global `claudemacs-program-switches' are now combined with per-tool switches
;; - Fixed `C-b' so it moves backward in Claudemacs terminals without affecting
;;   other Eat buffers
;; - Terminals now resize to their displayed window when sessions start
;; - Documented using `C-q' to send literal control keys to Claude Code

;; Version 0.3.0 (2025-01-14)
;; - Default opens "start a new session" when pressing 's' without a cur session
;; - Files outside the session's working directory now use absolute paths
;;   (less confusing for LLMs)
;; - Region selection now sends exact lines selected (not line with cursor)
;; - New `claudemacs-process-environment' custom variable for customizing
;;   environment variables passed to LLM processes
;; - Default enables 24-bit truecolor support for Claude Code's syntax
;;   highlighting (TERM=xterm-256color, COLORTERM=truecolor)

;; Version 0.2.0 (2025-12-14)
;; - Multi-tool support: Added `claudemacs-tool-registry' to support multiple
;;   AI coding tools (Claude, Codex, Gemini, etc.) with per-tool configuration
;; - Multiple instances: Run multiple sessions of the same tool per workspace
;;   (claude, claude-2, claude-3, etc.)
;; - New session management system with workspace-aware session tracking
;; - New commands: `claudemacs-switch-to-session', `claudemacs-switch-other',
;;   `claudemacs-kill-specific-session'
;; - New transient menus: `claudemacs-start-menu' and `claudemacs-resume-menu'
;;   with dynamic tool selection and custom flags (-d, -f, -p)
;; - Action system: Send commands to all sessions with C-u prefix
;; - Improved workspace detection supporting Doom, perspective.el, and
;;   vanilla Emacs
;; - Improved projectile support with `.projectile' marker file detection
;; - Error handling: Validate program exists before starting, catch startup
;;   failures and clean up orphaned buffers
;; - New `claudemacs-setup' function for explicit initialization
;; - Added `claudemacs-unload-function' for proper cleanup on unload
;; - Explicitly declared `transient' dependency in Package-Requires
;; - New `claudemacs-tool-name-face' for menu highlighting
;; - Buffer naming: `*claudemacs:TOOL:SESSION-ID*' format
;; - Fixed unreachable code in `claudemacs-toggle-buffer'

;; Version 0.1.0 (2024-11-01)
;; - Initial release

;;; Code:

;;;; Dependencies
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'project)
(require 'vc-git)
(require 'claudemacs-terminal)
(require 'claudemacs-comment)
(require 'claudemacs-session-list)

;; A live Emacs can reload this file over a pre-remediation version.  Remove
;; the old shutdown callback/state in that case so reloading cannot leave
;; prompt-bearing persistence behind.  This performs no file I/O and is a
;; one-time compatibility cleanup, not a new lifecycle hook.
(dolist (function '(claudemacs--save-session-snapshot-on-exit
                    claudemacs--write-session-snapshot
                    claudemacs--read-session-snapshot
                    claudemacs--snapshot-session-data))
  (when (fboundp function)
    (remove-hook 'kill-emacs-hook function)
    (fmakunbound function)))
(when (boundp 'claudemacs-session-snapshot-file)
  (makunbound 'claudemacs-session-snapshot-file))

;; Keep the pre-0.5 command name working for users who have it in a keymap or
;; transient configuration.  The implementation and autoloaded command live
;; in the dedicated session-list module.
(unless (fboundp 'claudemacs-session-overview)
  (defalias 'claudemacs-session-overview #'claudemacs-session-list))

;; Declare functions from optional packages
(declare-function safe-persp-name "perspective")
(declare-function get-current-persp "perspective")
(declare-function flycheck-error-message "flycheck")
(declare-function flycheck-overlay-errors-in "flycheck")
(declare-function projectile-project-root "projectile")
;; consult: optional, enables live buffer preview in session switching
(declare-function consult--read "consult")
(declare-function consult--original-window "consult")
(declare-function w32-notification-notify "w32fns.c" (&rest params))

;;;; Customization
(defgroup claudemacs nil
  "AI pair programming with Claude Code."
  :group 'tools)

(defcustom claudemacs-program "claude"
  "The name or path of the claude-code program."
  :type 'string
  :group 'claudemacs)

(defcustom claudemacs-terminal-backend
  (if (locate-library "ghostel") 'ghostel 'eat)
  "Terminal backend used for newly created Claudemacs sessions.
Ghostel is the default when its package is available on `load-path'; otherwise
Eat is used.  A value set through Customize or `setq' takes precedence over
this detected default.  The selected package is loaded lazily when a session
starts.  Existing sessions retain the backend with which they were created,
so sessions using different backends may coexist."
  :type '(choice (const :tag "Eat" eat)
                 (const :tag "Ghostel" ghostel))
  :group 'claudemacs)

(defcustom claudemacs-ghostel-query-before-killing t
  "Whether Claudemacs Ghostel sessions confirm before killing a live process.
This value is applied buffer-locally to Claudemacs sessions and does not
change the setting for unrelated Ghostel buffers.  Values match
`ghostel-query-before-killing': `t' always confirms, `nil' never confirms,
and `auto' confirms only while a shell command is running."
  :type '(choice (const :tag "Always" t)
                 (const :tag "Never" nil)
                 (const :tag "While a command is running" auto))
  :group 'claudemacs)

(defcustom claudemacs-ghostel-submit-delay 0.15
  "Seconds to wait before submitting programmatically inserted Ghostel input.
Interactive terminal applications can classify a prompt and an immediately
following Return as one paste burst, causing Return to insert a newline instead
of submitting.  This delay makes Return arrive as a separate key event.  Set
this higher if submission is unreliable on a heavily loaded system."
  :type 'number
  :group 'claudemacs)

(defcustom claudemacs-program-switches nil
  "List of command line switches to pass to the Claude program.
These are passed to the selected terminal backend when starting the program.
E.g, `\'(\"--verbose\" \"--dangerously-skip-permissions\")'"
  :type '(repeat string)
  :group 'claudemacs)

(defcustom claudemacs-tool-registry
  '((claude :label "Claude" :program "claude" :switches nil
            :model-types (("opus-high" :model "opus" :effort "high")
                          ("sonnet-high" :model "sonnet" :effort "high")))
    (codex :label "Codex" :program "codex" :switches nil
           :model-types (("luna-max" :model "gpt-5.6-luna" :effort "max")
                         ("sol-high" :model "gpt-5.6-sol" :effort "high")))
    (gemini :label "Gemini" :program "gemini-cli" :switches nil))
  "Registry of AI coding tools available for use with claudemacs.
Each entry is a list of the form (TOOL-NAME PLIST) where PLIST contains:
  :label    - Optional display name shown before the session name in the
              start and resume menus, e.g. \"Claude - claude-2\".  When
              omitted, only the session name is shown.
  :tool     - Which CLI this entry runs: `claude', `codex', `gemini', etc.
              It selects every CLI-specific behavior (notification switches,
              resume and fork arguments, `-d' translation, and session
              identity tracking), so several keys can run the same CLI with
              different settings.  When omitted it is inferred from
              `:program' and then from the entry's key, which is why a
              conventionally named program needs no `:tool' and a `:program'
              that is a path or an alias does.
  :program  - The name or path of the tool's executable
  :switches - List of command line switches to pass to the program
  :env      - Optional list of \"VAR=VALUE\" strings set in the environment of
              this tool's sessions only.  They take precedence over
              `claudemacs-process-environment'.  Values are used literally:
              no shell expansion happens, so build paths with
              `expand-file-name' rather than writing \"$HOME\" or \"~\".
  :model-types - Optional list of (NAME PLIST) model choices.  The model
                plist may contain :model, :effort, and/or :switches.

When `claudemacs-show-model-in-menu' is non-nil, model types are available
from the start-session menu.  `:model' and `:effort' are translated to the
appropriate command-line switches for Claude Code and Codex.  Use `:switches'
when a tool needs custom model-selection arguments.

Example, with two Codex profiles under distinct keys:
  ((claude :label \"Claude\" :program \"claude\" :switches nil
           :model-types ((\"opus-max\" :model \"opus\" :effort \"max\")))
   (codex-work :label \"Codex work\" :program \"codex\"
               :model-types ((\"fast\" :switches (\"--model\" \"gpt-4\"))))
   (codex-personal :label \"Codex personal\" :tool codex :program \"cdx\"
                   :env (\"CODEX_HOME=/home/me/.codex-personal\"))
   (gemini :program \"gemini\" :switches nil)
   (aider :program \"aider\" :switches '(\"--no-auto-commits\")))"
  :type '(alist :key-type symbol
                :value-type (plist :key-type symbol :value-type sexp))
  :group 'claudemacs)

(defcustom claudemacs-show-model-in-menu 't
  "Whether to show model information in the start-session menu.
When non-nil, each tool in `claudemacs-start-menu' shows the model it will
use in `font-lock-comment-face'.  Codex reads its default model and reasoning
effort from `~/.codex/config.toml'; Claude Code reads its model and effort
from `~/.claude/settings.json' when those values are present.

The setting also adds a \"Toggle model type\" menu item.  That item cycles
through the `:model-types' choices in `claudemacs-tool-registry'.  The
selected choice is applied only to newly started sessions."
  :type 'boolean
  :group 'claudemacs)

(defvar claudemacs--model-type-offset nil
  "Menu-local number of model-type steps from each tool's configured default.
Nil means that each tool's configured default should be used.  A numeric
value is reset whenever the start menu is opened and advances each tool's
configured model through its own `:model-types' list.")

(defcustom claudemacs-default-tool 'claude
  "The default AI coding tool to use when starting a new session.
Must be a symbol corresponding to a key in `claudemacs-tool-registry'.
When using the smart session start (key 's'), this tool will be used
if no other sessions exist in the current workspace."
  :type 'symbol
  :group 'claudemacs)

(defcustom claudemacs-prefer-projectile-root nil
  "Whether to prefer projectile root over git root when available.
If non-nil and projectile is loaded, use `projectile-project-root' to
determine the project root instead of `vc-git-root'. If projectile is
not available or fails to find a project root, falls back to git root
detection. This option has no effect if projectile is not installed."
  :type 'boolean
  :group 'claudemacs)

(defcustom claudemacs-switch-to-buffer-on-create t
  "Whether to switch to the Claudemacs buffer when creating a new session.
If non-nil, automatically switch to the Claude buffer after starting.
If nil, create the session but don't switch focus to it."
  :type 'boolean
  :group 'claudemacs)

(defcustom claudemacs-switch-to-buffer-on-toggle t
  "Whether to switch to the Claudemacs buffer when toggling to show it.
If non-nil, switch to the Claude buffer when toggling from hidden to visible.
If nil, show the buffer but don't switch focus to it."
  :type 'boolean
  :group 'claudemacs)

(defcustom claudemacs-m-return-is-submit nil
  "Swap the behavior of RET and M-RET in claudemacs buffers.
If nil (default): RET submits input, M-RET creates new line (standard behavior).
If non-nil: M-RET submits input, RET creates new line (swapped behavior).

This setting only affects Claudemacs terminal buffers."
  :type 'boolean
  :group 'claudemacs)

(defcustom claudemacs-shift-return-newline t
  "Whether Shift-Return creates a newline in claudemacs buffers.
If non-nil: S-RET acts like M-RET (creates a newline).
If nil (default): S-RET has default behavior.

This provides an alternative way to create newlines without using M-RET."
  :type 'boolean
  :group 'claudemacs)

(defcustom claudemacs-switch-to-buffer-on-file-add nil
  "Whether to switch to the Claudemacs buffer when adding file references.
If non-nil, automatically switch to the Claude buffer after adding files.
If nil, add the file reference but don't switch focus to it."
  :type 'boolean
  :group 'claudemacs)

(defcustom claudemacs-use-shell-env nil
  "Whether to run Claude through an interactive shell to load shell environment.
If non-nil, Claude is invoked through the user's interactive shell (e.g., zsh -i -c)
which sources rc files like .zshrc or .bashrc, making shell-configured PATH and
environment variables available to Claude.
If nil (default), Claude is invoked directly without shell environment loading.
This preserves backward compatibility for users whose existing setup works correctly."
  :type 'boolean
  :group 'claudemacs)

(defcustom claudemacs-process-environment
  '("TERM=xterm-256color" "COLORTERM=truecolor")
  "Additional environment variables for Claude processes.
Each entry should be a string in \"VAR=value\" format.
These are prepended to `process-environment' when starting sessions.

The defaults enable 24-bit truecolor support:
- TERM=xterm-256color: Standard terminal type recognized by most CLI tools
- COLORTERM=truecolor: Signals that 24-bit color is supported"
  :type '(repeat string)
  :group 'claudemacs)

(defcustom claudemacs-switch-to-buffer-on-send-error nil
  "Whether to switch to the Claudemacs buffer when sending error fix requests.
If non-nil, automatically switch to the Claude buffer after sending
error fix requests. If nil, send the error fix request but don't switch
focus to it."
  :type 'boolean
  :group 'claudemacs)

(defcustom claudemacs-switch-to-buffer-on-add-context t
  "Whether to switch to the Claudemacs buffer when adding context.
If non-nil, automatically switch to the Claude buffer after adding context.
If nil, add the context but don't switch focus to it."
  :type 'boolean
  :group 'claudemacs)

(defcustom claudemacs-open-new-frame-for-ediff-requests nil
  "Whether `x' and `X' requests from Ediff use a separate frame.
When non-nil, sending either request from an active Ediff buffer preserves the
Ediff window layout by showing the current Claudemacs session in another frame.
If that session is already displayed in any frame, reuse its existing window
instead of creating another frame."
  :type 'boolean
  :group 'claudemacs)

(defcustom claudemacs-notify-on-await t
  "Whether to show a system notification when an AI tool awaits the user.
When non-nil, display an OS notification popup when an AI tool completes a task.
When nil, no notification is shown (silent operation)."
  :type 'boolean
  :group 'claudemacs)

(defconst claudemacs--legacy-codex-notification-switches
  '("--config" "tui.notification_method=\"bel\""
    "--config" "tui.notification_condition=\"always\"")
  "Codex notification default used before message-bearing notifications.")

(defconst claudemacs--default-codex-notification-switches
  '("--config" "tui.notification_method=\"osc9\""
    "--config" "tui.notification_condition=\"always\"")
  "Default switches for message-bearing Codex notifications.")

(defcustom claudemacs-codex-notification-switches
  (copy-tree claudemacs--default-codex-notification-switches)
  "Command-line switches used to route Codex notifications through the terminal.

Codex can emit TUI notifications as OSC 9 or BEL, and by default only emits
them when it believes its terminal is unfocused.  Both forms reach
Claudemacs' system notification handler, and these switches make Codex
notify regardless of its focus state.

The OSC 9 form carries Codex's own notification text — normally the last
thing the agent said — which Claudemacs shows as the body of the system
notification.  BEL carries nothing but the event, so a session configured
with `tui.notification_method=\"bel\"' falls back to the generic
\"awaiting your input\" text.  See `claudemacs-notify-with-tool-message'.

Set this to nil to use Codex's own notification settings."
  :type '(repeat string)
  :group 'claudemacs)

(defcustom claudemacs-notify-with-tool-message t
  "Whether system notifications show the message the AI tool sent with them.

Tools raise a desktop notification by writing an OSC 9 or OSC 777 escape
sequence that carries the notification text — for Codex, normally the last
thing the agent said.  When non-nil, Claudemacs uses that text as the body
of the system notification instead of the generic \"awaiting your input\"
message.

Tools that only ring the terminal bell have no message to show, so those
notifications keep the generic text either way."
  :type 'boolean
  :group 'claudemacs)

(defcustom claudemacs-notification-sound-mac "Submarine"
  "The sound to use when displaying system notifications on macOS.

System sounds include: `Basso', `Blow', `Bottle', `Frog', `Funk',
`Glass', `Hero', `Morse', `Ping', `Pop', `Purr', `Sosumi', `Submarine',
`Tink'. Or put more sounds in the `/Library/Sound' folder and use those."
  :type 'string
  :group 'claudemacs)

(defcustom claudemacs-notification-auto-dismiss-linux t
  "Whether to auto-dismiss notifications on Linux (don't persist to system tray).
When non-nil, notifications will automatically disappear and not stay in the tray.
When nil, notifications will persist in the system tray according to system defaults.
This setting only affects Linux systems using notify-send."
  :type 'boolean
  :group 'claudemacs)

(defcustom claudemacs-notification-sound-linux "bell"
  "The sound to use when displaying system notifications on Linux.
Uses canberra-gtk-play if available.  Common sound IDs include:
`message-new-instant', `bell', `dialog-error', `dialog-warning'.
When empty string, no sound is played."
  :type 'string
  :group 'claudemacs)

(defcustom claudemacs-notification-timeout-windows 5
  "Seconds before a Windows completion notification expires."
  :type 'integer
  :group 'claudemacs)

(defcustom claudemacs-startup-hook nil
  "Hook run after a claudemacs session has finished starting up.
This hook is called after the terminal is initialized, keymaps
are set up, and bell handlers are configured. The hook functions
are executed with the claudemacs buffer as the current buffer."
  :type 'hook
  :group 'claudemacs)

(defface claudemacs-repl-face
  nil
  "Face for Claude REPL."
  :group 'claudemacs)

(defface claudemacs-tool-name-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for highlighting tool names in transient menu descriptions."
  :group 'claudemacs)

;;;; Buffer-local Variables
(defvar-local claudemacs--cwd nil
  "Buffer-local variable storing the current working directory for this Claude session.")

(defvar-local claudemacs--tool nil
  "Buffer-local variable storing the AI tool name (symbol) for this session.")

(defvar-local claudemacs--tool-instance nil
  "Buffer-local instance number for this session.")

(defvar-local claudemacs--session-tool-kind nil
  "CLI family captured when this session started.")

(defvar-local claudemacs--session-tool-kind-set-p nil
  "Non-nil when `claudemacs--session-tool-kind' is an authoritative snapshot.")

(defvar-local claudemacs--claude-session-uuid nil
  "Buffer-local variable storing the Claude Code session UUID.
Set when starting a new Claude session with --session-id.
Used for branching with --resume <uuid> --fork-session.

This variable is retained for compatibility.  New code should use
`claudemacs--session-id' and `claudemacs--session-id-provenance'.")

;; Declare compatibility aliases before their buffer-local referents so Emacs
;; propagates local bindings through the aliases correctly.
(defvaralias 'claudemacs--authoritative-session-id 'claudemacs--session-id)
(defvaralias 'claudemacs--session-identity 'claudemacs--session-id-provenance)

(defvar-local claudemacs--session-id nil
  "Authoritative tool-neutral session ID for the current Claudemacs buffer.

The value is nil until Claudemacs passes an authoritative ID to the tool.
It must never be populated from buffer recency, a matching working directory,
or an ordering heuristic.")

(defvar-local claudemacs--session-id-provenance 'unknown
  "Provenance of `claudemacs--session-id'.

The value is one of `exact', `discovered', or `unknown'.  `exact' means that
Claudemacs generated, explicitly selected, or explicitly passed the ID.
`discovered' is retained only for compatibility with buffers created by an
older version; new sessions never use it.  An unknown ID is intentionally not
associated with any history row.")

(defconst claudemacs--session-id-provenances '(exact discovered unknown)
  "Accepted values for `claudemacs--session-id-provenance'.")

;; Names used by early session-list revisions remain aliases rather than a
;; second mutable source of identity.  This also lets the read-only module
;; consume the live state without knowing which lifecycle spelling introduced
;; it.
(defvar-local claudemacs--ghostel-escape-map-active nil
  "Non-nil when Claudemacs' Ghostel overrides are active in this buffer.")

(defvar-local claudemacs--ghostel-escape-map-alist nil
  "Buffer-local emulation map alist used for Ghostel key overrides.")

;;;;
;;;; Utility Functions
;;;;

(defun claudemacs--generate-uuid ()
  "Generate a UUID v4 string."
  (format "%08x-%04x-4%03x-%04x-%012x"
          (random (expt 16 8))
          (random (expt 16 4))
          (random (expt 16 3))
          (logior #x8000 (random #x3fff))
          (random (expt 16 12))))

(defun claudemacs--find-projectile-root (dir)
  "Find project root by looking for .projectile marker file from DIR.
Returns the directory containing .projectile, or nil if not found."
  (when-let* ((root (locate-dominating-file dir ".projectile")))
    (file-name-as-directory root)))

(defun claudemacs--project-root (&optional dir)
  "Get the project root for starting the CLI.
If DIR is given, use it as the starting location.
When `claudemacs-prefer-projectile-root' is enabled, tries in order:
1. `projectile-project-root' (if projectile is installed)
2. .projectile marker file (works without projectile)
Then falls back to `vc-git-root', then to the directory itself."
  (let ((loc (or dir
                 (when (buffer-file-name)
                   (file-name-directory (buffer-file-name)))
                 default-directory)))
    (or
     ;; Try projectile if enabled
     (when claudemacs-prefer-projectile-root
       (or
        ;; First try projectile-project-root if available
        (when (fboundp 'projectile-project-root)
          (condition-case nil
              ;; Projectile consults `default-directory' rather than taking a
              ;; location argument.  Bind it so an explicit DIRECTORY does
              ;; not accidentally resolve the caller's current project.
              (let ((default-directory loc))
                (let ((proj-root (projectile-project-root)))
                  (when (and proj-root (file-directory-p proj-root))
                    proj-root)))
            (error nil)))
        ;; Fall back to .projectile marker file
        (claudemacs--find-projectile-root loc)))
     ;; Fallback to vc-git-root (built-in via vc-git)
     (vc-git-root loc)
     ;; Final fallback to location itself
     loc)))

(defun claudemacs--get-tool-config (tool)
  "Get the configuration plist for TOOL from `claudemacs-tool-registry'.
Returns nil if the tool is not found in the registry."
  (cdr (assq tool claudemacs-tool-registry)))

(defun claudemacs--get-tool-label (tool)
  "Return the display label for TOOL, or nil when none is configured.
The label comes from the `:label' entry in `claudemacs-tool-registry'.  An
empty or non-string value is treated as no label."
  (let ((label (plist-get (claudemacs--get-tool-config tool) :label)))
    (when (and (stringp label) (not (string-empty-p label)))
      label)))

(defconst claudemacs--tool-kinds '(claude codex gemini)
  "CLI families Claudemacs can infer from a program name or registry key.
An entry's `:tool' may name any family, including one absent from this list;
the list only drives inference when `:tool' is omitted.")

(defun claudemacs--tool-kind-from-name (name)
  "Return the CLI family implied by NAME, or nil when none matches.
NAME is a program name or a registry key.  Directory components and an
executable extension are ignored, and a family name followed by a separator
matches too, so \"/usr/bin/codex\", \"codex.exe\", and \"codex-personal\" are
all Codex.  Inference is a convenience for conventionally named programs; an
alias such as \"cdx\" is why entries can state `:tool' outright."
  (when (stringp name)
    (let ((base (downcase (file-name-sans-extension
                           (file-name-nondirectory name)))))
      (seq-find
       (lambda (kind)
         (let ((kind-name (symbol-name kind)))
           (or (string= base kind-name)
               (string-match-p (format "\\`%s[-_.]" (regexp-quote kind-name))
                               base))))
       claudemacs--tool-kinds))))

(defun claudemacs--tool-kind (tool)
  "Return the CLI family for TOOL, or nil when no family applies.
TOOL is a key in `claudemacs-tool-registry'.  The family decides every
CLI-specific behavior: notification switches, resume and fork arguments,
`-d' translation, and session-identity tracking.  It is resolved in order
from the entry's `:tool', then its `:program', then the registry key, so
several keys can run the same CLI and an entry whose program is a path or an
alias can state its family explicitly."
  (when (symbolp tool)
    (let* ((config (claudemacs--get-tool-config tool))
           (declared (plist-get config :tool))
           (program (and config
                         (or (plist-get config :program) claudemacs-program))))
      (cond
       ((and declared (symbolp declared)) declared)
       ((stringp declared) (intern declared))
       (t (or (claudemacs--tool-kind-from-name program)
              (claudemacs--tool-kind-from-name (symbol-name tool))))))))

(defun claudemacs--tool-kind-p (tool kind)
  "Return non-nil when TOOL belongs to the CLI family KIND."
  (eq (claudemacs--tool-kind tool) kind))

(defun claudemacs--tool-kind-for-buffer (buffer tool)
  "Return TOOL's CLI family as owned by session BUFFER.
Existing sessions lazily capture the current family for reload compatibility;
new sessions capture it at launch."
  (if (not (buffer-live-p buffer))
      (claudemacs--tool-kind tool)
    (with-current-buffer buffer
      (unless claudemacs--session-tool-kind-set-p
        (setq-local claudemacs--session-tool-kind (claudemacs--tool-kind tool)
                    claudemacs--session-tool-kind-set-p t))
      claudemacs--session-tool-kind)))

(defun claudemacs--get-tool-env (tool)
  "Return TOOL's `:env' entries from `claudemacs-tool-registry'.
The value is a list of \"VAR=VALUE\" strings that are prepended to
`process-environment' when a session for TOOL starts, so they take precedence
over `claudemacs-process-environment' and the Emacs environment.  Signal an
error when an entry is not a \"VAR=VALUE\" string, because silently dropping it
would start the tool with an environment the user did not ask for."
  (let ((env (plist-get (claudemacs--get-tool-config tool) :env)))
    (when env
      (unless (listp env)
        (error "Invalid :env for tool `%s': %S is not a list" tool env))
      (dolist (entry env)
        (unless (and (stringp entry)
                     (string-match-p "\\`[^=]+=" entry))
          (error "Invalid :env entry for tool `%s': %S is not \"VAR=VALUE\""
                 tool entry)))
      env)))

(defun claudemacs--tool-env-value (environment variable)
  "Return VARIABLE's value from ENVIRONMENT, or nil when it is absent."
  (let ((prefix (concat variable "=")))
    (seq-some (lambda (entry)
                (when (string-prefix-p prefix entry)
                  (substring entry (length prefix))))
              environment)))

(defun claudemacs--effective-tool-env (tool)
  "Return TOOL's environment with profile isolation defaults applied.
An explicit Codex `CODEX_HOME' also owns its SQLite state unless the profile
states `CODEX_SQLITE_HOME' separately."
  (let* ((environment (claudemacs--get-tool-env tool))
         (codex-home (claudemacs--tool-env-value environment "CODEX_HOME")))
    (if (and (claudemacs--tool-kind-p tool 'codex)
             codex-home
             (not (string-empty-p codex-home))
             (null (claudemacs--tool-env-value environment "CODEX_SQLITE_HOME")))
        (cons (concat "CODEX_SQLITE_HOME=" codex-home) environment)
      environment)))

(defun claudemacs--tool-getenv (tool variable)
  "Return VARIABLE for TOOL, preferring TOOL's `:env' over Emacs's environment.
This keeps a setting such as `CODEX_HOME' consistent between the session
Claudemacs starts and the configuration it reads to describe that session."
  (let ((prefix (concat variable "=")))
    (or (seq-some (lambda (entry)
                    (when (string-prefix-p prefix entry)
                      (substring entry (length prefix))))
                  (ignore-errors (claudemacs--effective-tool-env tool)))
        (getenv variable))))

(defun claudemacs--call-with-tool-env (tool function)
  "Call FUNCTION with TOOL's `:env' applied to `process-environment'.
History and configuration lookups resolve their storage through the
environment, so reading a profile's sessions has to use the same environment
that profile's sessions are started with.  The binding is undone when
FUNCTION returns, and a malformed `:env' is ignored here: a lookup should
degrade to the default environment rather than fail, and starting a session
reports that error already."
  (let ((process-environment
         (append (ignore-errors (claudemacs--effective-tool-env tool))
                 process-environment)))
    (funcall function)))

(defun claudemacs--tool-config-file (tool file)
  "Return TOOL's configuration FILE, honoring TOOL's configuration directory.
Codex reads `CODEX_HOME' and Claude Code reads `CLAUDE_CONFIG_DIR', each from
TOOL's `:env' first, so a second profile reads its own configuration."
  (let* ((kind (claudemacs--tool-kind tool))
         (directory
          (pcase kind
            ('codex (or (claudemacs--tool-getenv tool "CODEX_HOME") "~/.codex"))
            ('claude (or (claudemacs--tool-getenv tool "CLAUDE_CONFIG_DIR")
                         "~/.claude"))
            (_ nil))))
    (when directory
      (expand-file-name file (expand-file-name directory)))))

(defun claudemacs--read-setting-from-file (file regexp)
  "Read the first captured value matching REGEXP from FILE.
Return nil when FILE cannot be read or REGEXP does not match.  This small
reader is intentionally limited to the scalar settings needed for the menu;
it avoids adding a TOML dependency just to inspect Codex's default model."
  (condition-case nil
      (when (file-readable-p file)
        (with-temp-buffer
          (insert-file-contents-literally file)
          (goto-char (point-min))
          (when (re-search-forward regexp nil t)
            (match-string-no-properties 1))))
    (error nil)))

(defun claudemacs--get-tool-configured-model (tool)
  "Return TOOL's configured model and effort as a plist.
Codex stores these values in `config.toml' under `CODEX_HOME' (`~/.codex' by
default).  Claude Code stores an optional model and effort level in
`settings.json' under `CLAUDE_CONFIG_DIR' (`~/.claude' by default); its
documented CLI default is the latest Sonnet alias when no model is configured.
Both directories are resolved through TOOL's `:env' first, so each profile
reports its own configuration."
  (let* ((tool-config (claudemacs--get-tool-config tool))
         (configured-model (plist-get tool-config :model))
         (configured-effort (or (plist-get tool-config :effort)
                                (plist-get tool-config :reasoning-effort))))
    (cond
     ((or configured-model configured-effort)
      (list :model configured-model :effort configured-effort))
     ((claudemacs--tool-kind-p tool 'codex)
      (let ((config-file (claudemacs--tool-config-file tool "config.toml")))
        (list :model
              (claudemacs--read-setting-from-file
               config-file
               "^[[:space:]]*model[[:space:]]*=[[:space:]]*[\"']\\([^\"']+\\)[\"']")
              :effort
              (claudemacs--read-setting-from-file
               config-file
               "^[[:space:]]*model_reasoning_effort[[:space:]]*=[[:space:]]*[\"']\\([^\"']+\\)[\"']"))))
     ((claudemacs--tool-kind-p tool 'claude)
      (let ((settings-file (claudemacs--tool-config-file tool "settings.json")))
        (list :model
              (or (claudemacs--tool-getenv tool "ANTHROPIC_MODEL")
                  (claudemacs--read-setting-from-file
                   settings-file
                   "\"model\"[[:space:]]*:[[:space:]]*\"\\([^\"]+\\)\"")
                  "sonnet")
              :effort
              (or (claudemacs--read-setting-from-file
                   settings-file
                   "\"effortLevel\"[[:space:]]*:[[:space:]]*\"\\([^\"]+\\)\"")
                  (claudemacs--read-setting-from-file
                   settings-file
                   "\"effort\"[[:space:]]*:[[:space:]]*\"\\([^\"]+\\)\"")))))
     (t nil))))

(defun claudemacs--get-tool-model-types (tool)
  "Return the model types configured for TOOL, or nil."
  (let ((model-types (plist-get (claudemacs--get-tool-config tool)
                                :model-types)))
    (and (listp model-types) model-types)))

(defun claudemacs--model-type-plist (model-type)
  "Return the options plist from MODEL-TYPE.
The preferred form is (NAME :model MODEL :effort EFFORT).  A plist beginning
with a keyword and a bare string model name are also accepted for convenient
custom tool configurations."
  (cond
   ((and (listp model-type) (keywordp (car model-type))) model-type)
   ((consp model-type) (cdr model-type))
   (t nil)))

(defun claudemacs--model-type-name (model-type)
  "Return the display name for MODEL-TYPE."
  (let* ((plist (claudemacs--model-type-plist model-type))
         (name (or (plist-get plist :name)
                   (and (consp model-type) (car model-type))
                   (plist-get plist :model))))
    (cond
     ((stringp model-type) model-type)
     ((symbolp name) (symbol-name name))
     ((stringp name) name)
     (name (format "%s" name))
     (t "default"))))

(defun claudemacs--model-type-model (model-type)
  "Return the model identifier represented by MODEL-TYPE."
  (let* ((plist (claudemacs--model-type-plist model-type))
         (model (or (plist-get plist :model)
                    (and (stringp model-type) model-type)
                    (and (consp model-type)
                         (or (stringp (car model-type))
                             (symbolp (car model-type)))
                         (car model-type)))))
    (cond
     ((stringp model) model)
     ((symbolp model) (symbol-name model))
     (model (format "%s" model))
     (t nil))))

(defun claudemacs--model-type-effort (model-type)
  "Return the reasoning effort represented by MODEL-TYPE, if any."
  (let ((plist (claudemacs--model-type-plist model-type)))
    (or (plist-get plist :effort)
        (plist-get plist :reasoning-effort))))

(defun claudemacs--model-type-switches (tool model-type)
  "Return command-line switches for MODEL-TYPE of TOOL.
An explicit `:switches' value is used verbatim.  Otherwise the standard
Claude Code and Codex model/effort switches are generated."
  (let* ((plist (claudemacs--model-type-plist model-type))
         (explicit-switches (plist-get plist :switches))
         (model (claudemacs--model-type-model model-type))
         (effort (claudemacs--model-type-effort model-type)))
    (cond
     (explicit-switches
      (if (listp explicit-switches)
          (copy-sequence explicit-switches)
        (list explicit-switches)))
     ((null model) nil)
     ((claudemacs--tool-kind-p tool 'codex)
      (append (list "--model" model)
              (when effort
                (list "--config"
                      (format "model_reasoning_effort=%S" effort)))))
     ((claudemacs--tool-kind-p tool 'claude)
      (append (list "--model" model)
              (when effort (list "--effort" (format "%s" effort)))))
     (t (list "--model" model)))))

(defun claudemacs--model-type-for-tool-at-offset (tool offset)
  "Return TOOL's model type OFFSET steps after its configured model.
Return nil when OFFSET is nil, meaning that TOOL's configuration should be
used without a command-line override.  If the configured model is not one of
TOOL's model types, the cycle is the configured default followed by each
configured type."
  (let* ((model-types (claudemacs--get-tool-model-types tool))
         (configured-index (and model-types
                                (claudemacs--model-type-index-for-config tool))))
    (when model-types
      (let* ((cycle (cons nil
                          (if (numberp configured-index)
                              (append
                               (nthcdr (1+ configured-index) model-types)
                               (cl-subseq model-types 0 configured-index))
                            model-types)))
             (position (and (numberp offset)
                            (mod offset (length cycle)))))
        (and position (nth position cycle))))))

(defun claudemacs--model-type-for-tool (tool)
  "Return TOOL's currently selected model type, or nil.
The current selection is local to the active start-menu invocation; nil means
that TOOL's own configuration should be used."
  (claudemacs--model-type-for-tool-at-offset
   tool claudemacs--model-type-offset))

(defun claudemacs--model-type-index-for-config (tool)
  "Return the model-type index matching TOOL's configured model and effort.
Return nil when either value differs from every configured model type."
  (let ((configured (claudemacs--get-tool-configured-model tool)))
    (when configured
      (cl-loop with configured-model = (plist-get configured :model)
               with configured-effort = (plist-get configured :effort)
               for model-type in (claudemacs--get-tool-model-types tool)
               for index from 0
               for model = (claudemacs--model-type-model model-type)
               for effort = (claudemacs--model-type-effort model-type)
               when (and configured-model
                         (equal configured-model model)
                         (equal configured-effort effort))
               return index))))

(defun claudemacs--model-type-for-config (tool)
  "Return TOOL's configured model type, if it matches one exactly."
  (let* ((model-types (claudemacs--get-tool-model-types tool))
         (index (and model-types
                     (claudemacs--model-type-index-for-config tool))))
    (and (numberp index)
         (nth index model-types))))

(defun claudemacs--model-type-cycle-length (tool)
  "Return the number of toggle positions for TOOL."
  (let ((model-types (claudemacs--get-tool-model-types tool)))
    (when model-types
      (+ (length model-types)
         (if (numberp (claudemacs--model-type-index-for-config tool)) 0 1)))))

(defun claudemacs--model-type-count ()
  "Return the largest model-type cycle across the tool registry."
  (let ((counts (delq nil
                      (mapcar (lambda (entry)
                                (claudemacs--model-type-cycle-length (car entry)))
                              claudemacs-tool-registry))))
    (if counts (apply #'max counts) 0)))

(defun claudemacs--model-type-reference-tool ()
  "Return a tool to use for model-type toggle labels and cycling."
  (or (and (claudemacs--get-tool-model-types claudemacs-default-tool)
           claudemacs-default-tool)
      (cl-loop for entry in claudemacs-tool-registry
               when (claudemacs--get-tool-model-types (car entry))
               return (car entry))))

(defun claudemacs--model-type-toggle-available-p ()
  "Return non-nil when at least two model types are configured."
  (> (claudemacs--model-type-count) 1))

(defun claudemacs--model-type-toggle-visible-p ()
  "Return non-nil when the model-type toggle belongs in the start menu."
  (and claudemacs-show-model-in-menu
       (claudemacs--model-type-toggle-available-p)))

(defun claudemacs--format-model-type-display (model-type)
  "Return MODEL-TYPE's preset name for display."
  (claudemacs--model-type-name model-type))

(defun claudemacs--get-tool-configured-model-short-name (tool)
  "Return a preset-style short name for TOOL's configured model, if possible.
Use an exact configured preset name when one exists.  Otherwise derive the
same MODEL-EFFORT form used by the model-type names when TOOL has model types."
  (let* ((configured (claudemacs--get-tool-configured-model tool))
         (model-type (claudemacs--model-type-for-config tool))
         (model (plist-get configured :model))
         (effort (plist-get configured :effort)))
    (or (and model-type
             (claudemacs--model-type-name model-type))
        (and (claudemacs--get-tool-model-types tool)
             model
             (cond
              (effort (format "%s-%s" model effort))
              ((and (stringp model)
                    (string-match-p "\\`[^/]+/[^/]+\\'" model))
               (replace-regexp-in-string "/" "-" model)))))))

(defun claudemacs--get-tool-configured-model-display (tool)
  "Return TOOL's actual configured model and effort for display."
  (or (claudemacs--get-tool-configured-model-short-name tool)
      (let ((configured (claudemacs--get-tool-configured-model tool)))
        (if (plist-get configured :model)
            (if (plist-get configured :effort)
                (format "%s/%s"
                        (plist-get configured :model)
                        (plist-get configured :effort))
              (plist-get configured :model))
          "default"))))

(defun claudemacs--get-tool-model-display (tool)
  "Return the model text to show for TOOL in the start menu."
  (let ((model-type (claudemacs--model-type-for-tool tool)))
    (if model-type
        (claudemacs--format-model-type-display model-type)
      (claudemacs--get-tool-configured-model-display tool))))

(defun claudemacs--get-toggle-model-type-description ()
  "Return the dynamic description for the model-type toggle suffix."
  (let* ((tool (claudemacs--model-type-reference-tool))
         (model-types (and tool (claudemacs--get-tool-model-types tool)))
         (count (claudemacs--model-type-count))
         (current-offset (or claudemacs--model-type-offset 0))
         (next-offset (and (> count 1) (1+ current-offset)))
         (next (and tool next-offset
                    (claudemacs--model-type-for-tool-at-offset
                     tool next-offset))))
    (if next
        (format "Toggle model type (→ %s)"
                (propertize (claudemacs--model-type-name next)
                            'face 'font-lock-comment-face))
      (if model-types
          (format "Toggle model type (→ %s)"
                  (propertize (claudemacs--get-tool-configured-model-display tool)
                              'face 'font-lock-comment-face))
        "Toggle model type"))))

(defun claudemacs--reset-model-type-state ()
  "Reset the model-type selection for the next start-menu invocation."
  (setq claudemacs--model-type-offset nil))

(defun claudemacs-toggle-model-type ()
  "Advance every tool to its next model type for this menu invocation.
Each tool advances from the model read from its own configuration.  The
selection is reset when the start menu is opened again."
  (interactive)
  (let* ((tool (claudemacs--model-type-reference-tool))
         (model-types (and tool (claudemacs--get-tool-model-types tool)))
         (count (claudemacs--model-type-count)))
    (if (or (null model-types) (<= count 1))
        (user-error "No alternative model types are configured")
      (let ((next-offset (1+ (or claudemacs--model-type-offset 0))))
        (setq claudemacs--model-type-offset next-offset)
        (when (and (boundp 'transient--prefix)
                   transient--prefix
                   (fboundp 'transient--refresh-transient))
          (transient--refresh-transient))))))

(defun claudemacs--get-tool-notification-switches (tool)
  "Get notification-related command-line switches for TOOL.
Codex notifications are routed through Eat's BEL handler; other tools use
their own notification mechanisms."
  (when (claudemacs--tool-kind-p tool 'codex)
    claudemacs-codex-notification-switches))

(defun claudemacs--get-resume-flag (tool)
  "Return TOOL's resume token for compatibility with older callers.

This token is not a complete launch command; new lifecycle code must use
`claudemacs--get-resume-args' so an authoritative ID is always supplied."
  (pcase (claudemacs--tool-kind tool)
    ('claude "--resume")
    ('codex "resume")
    (_ "--resume")))

(defconst claudemacs--safe-session-id-regexp
  "\\`[A-Za-z0-9_.][A-Za-z0-9_.:-]*\\'"
  "Regexp for session IDs safe to display and pass as one CLI argument.")

(defun claudemacs--safe-session-id-p (value)
  "Return non-nil when VALUE is a safe, non-empty session ID token.

The allowlist intentionally accepts canonical UUIDs, Codex thread IDs, and
the other ASCII identifiers emitted by the history providers while rejecting
leading hyphens, whitespace, control characters, and display punctuation.
Keeping this check centralized prevents an untrusted history row or manual
selection from becoming an option-like or misleading CLI argument."
  (and (stringp value)
       (string-match-p claudemacs--safe-session-id-regexp value)))

(defun claudemacs--validate-session-id (value &optional tool)
  "Return VALUE when it is safe for an explicit session operation.

Signal `user-error' for an empty or unsafe ID.  TOOL is included only in the
diagnostic; it does not change validation rules."
  (unless (claudemacs--safe-session-id-p value)
    (user-error "Invalid%s session ID; expected a non-empty safe token"
                (if tool (format " %s" (capitalize (symbol-name tool))) "")))
  value)

(defun claudemacs--get-resume-args (tool session-id)
  "Return explicit resume arguments for TOOL and SESSION-ID.

The caller must supply an authoritative ID.  In particular, this helper never
returns Codex's bare `resume' selector or Claude's interactive resume picker."
  (unless session-id
    (user-error "No authoritative %s session ID is available"
                (capitalize (symbol-name tool))))
  (setq session-id (claudemacs--validate-session-id session-id tool))
  (pcase (claudemacs--tool-kind tool)
    ('claude (list "--resume" session-id))
    ('codex (list "resume" session-id))
    (_ (list "--resume" session-id))))

(defun claudemacs--get-branch-args (tool &optional source-id destination-id)
  "Return explicit branch/fork arguments for TOOL.

SOURCE-ID must identify the authoritative source conversation.  Claude also
accepts an optional DESTINATION-ID, which Claudemacs generates before launch
when the installed CLI supports `--session-id' with `--fork-session'.  Codex
does not expose a destination-ID option, so the new destination remains
unknown.  Nil is returned when no authoritative source is available; callers
must not replace it with a bare picker or `--last' selector."
  (when source-id
    (setq source-id (claudemacs--validate-session-id source-id tool))
    (when destination-id
      (setq destination-id
            (claudemacs--validate-session-id destination-id tool)))
    (pcase (claudemacs--tool-kind tool)
      ('claude (append (list "--resume" source-id "--fork-session")
                       (when destination-id
                         (list "--session-id" destination-id))))
      ('codex (list "fork" source-id))
      ('gemini (list "--resume" source-id))
      (_ (list "--resume" source-id)))))

(declare-function claudemacs--session-list-history-rows
                  "claudemacs-session-list" (&optional tool cwd))

(declare-function claudemacs--session-list-claude-history
                  "claudemacs-session-list" (&optional cwd))
(declare-function claudemacs--session-list-codex-history
                  "claudemacs-session-list" (&optional cwd))
(declare-function claudemacs--session-list--newer-first-p
                  "claudemacs-session-list" (left right))
(declare-function claudemacs--session-list--same-path-p
                  "claudemacs-session-list" (left right))

(defun claudemacs--call-history-provider (tool cwd)
  "Call TOOL's session-list history provider for CWD, or return nil.

The unified provider name is retained as a small forward-compatible contract;
the current module also exposes the two focused provider functions directly.
The provider runs with TOOL's `:env' applied, so a profile that sets
`CODEX_HOME' or `CLAUDE_CONFIG_DIR' lists its own sessions rather than the
default installation's.  Provider exceptions are returned as `(:error
MESSAGE)' so callers that need a trustworthy snapshot can distinguish them
from an empty history."
  (let ((function
         (cond
          ((fboundp 'claudemacs--session-list-history-rows)
           (lambda () (claudemacs--session-list-history-rows tool cwd)))
          ((and (claudemacs--tool-kind-p tool 'claude)
                (fboundp 'claudemacs--session-list-claude-history))
           (lambda () (claudemacs--session-list-claude-history cwd)))
          ((and (claudemacs--tool-kind-p tool 'codex)
                (fboundp 'claudemacs--session-list-codex-history))
           (lambda () (claudemacs--session-list-codex-history cwd))))))
    (when function
      (condition-case error-data
          (claudemacs--call-with-tool-env tool function)
        ;; Preserve provider failures as an explicit wrapper.  A nil result is
        ;; a valid empty history for the focused providers, so collapsing an
        ;; exception to nil would make an unavailable provider indistinguishable
        ;; from an empty explicit-selection source.
        (error (list :error (error-message-string error-data)))))))

(defun claudemacs--history-rows-for-tool (tool cwd)
  "Return authoritative history rows for TOOL and CWD.

The session-list module owns storage resolution and schema validation.  This
small lifecycle adapter intentionally returns nil when the provider is not
available or reports an error; callers then fail closed rather than inventing
an ID.  A row may use either `:session-id' (the normalized contract) or `:id'
for compatibility with provider implementations."
  (let ((rows (claudemacs--call-history-provider tool cwd)))
    (cond
     ;; A provider result is a wrapper only when it explicitly carries the
     ;; `:rows' or `:error' contract.  Do not mistake a single normalized row
     ;; plist for that wrapper.
     ((and (listp rows)
           (or (memq :rows rows) (memq :error rows)))
      (unless (plist-get rows :error)
        (or (plist-get rows :rows) '())))
     ((listp rows) rows))))

(defun claudemacs--history-row-session-id (row)
  "Return a safe authoritative ID from normalized history ROW, or nil.

Malformed provider IDs are excluded before they can enter a completion choice
or become a command-line argument."
  (let ((id (or (plist-get row :session-id)
                (plist-get row :id))))
    (when (claudemacs--safe-session-id-p id)
      id)))

(defun claudemacs--history-row-cwd (row)
  "Return normalized CWD from history ROW, or nil."
  (let ((cwd (plist-get row :cwd)))
    (when (stringp cwd)
      (condition-case nil
          (file-truename cwd)
        (error cwd)))))

(defun claudemacs--same-cwd-p (left right)
  "Return non-nil when LEFT and RIGHT identify the same directory."
  (and (stringp left) (stringp right)
       (claudemacs--session-list--same-path-p left right)))

(defun claudemacs--history-rows-for-cwd (tool cwd)
  "Return TOOL history rows for CWD, ordered from newest to oldest."
  (sort
   (seq-filter
    (lambda (row)
      (claudemacs--same-cwd-p cwd (claudemacs--history-row-cwd row)))
    (or (claudemacs--history-rows-for-tool tool cwd) nil))
   (lambda (left right)
     (claudemacs--session-list--newer-first-p
      (plist-get left :updated-at)
      (plist-get right :updated-at)))))

(defun claudemacs--select-history-session-id (tool cwd)
  "Prompt for an authoritative history ID for TOOL in CWD.

Rows are ordered by the history provider's last-used time, newest first.
If a provider is unavailable, an explicit ID may still be entered manually;
an invalid or empty answer is rejected before launch."
  (let* ((rows (claudemacs--history-rows-for-cwd tool cwd))
         (choices
          (mapcar
           (lambda (row)
             (let* ((id (claudemacs--history-row-session-id row))
                    (description (or (plist-get row :description)
                                      (plist-get row :title)
                                      ""))
                    (display-description
                     (if (and (stringp description)
                              (> (length description) 80))
                         (concat (substring description 0 77) "...")
                       description)))
               (cons (if (string-empty-p display-description)
                         id
                       (format "%s — %s" id display-description))
                     id)))
           (seq-filter #'claudemacs--history-row-session-id rows))))
    (if choices
        (let ((completion-extra-properties
               '(:display-sort-function identity
                 :cycle-sort-function identity)))
          (cdr (assoc (completing-read
                       (format "Select %s session: "
                               (capitalize (symbol-name tool)))
                       choices nil t)
                      choices)))
      (let ((id (read-string
                 (format "%s session ID (history unavailable): "
                         (capitalize (symbol-name tool))))))
        (claudemacs--validate-session-id id tool)))))

(defun claudemacs--get-current-tool-name ()
  "Get the display name (capitalized) of the current session's tool.
Returns the tool name as a string (e.g., \"Claude\", \"Codex\").
Returns \"Claude\" as fallback if no session is found."
  (let* ((session-buffer (claudemacs--get-current-session-buffer))
         (tool (when session-buffer
                 (with-current-buffer session-buffer
                   claudemacs--tool))))
    (capitalize (symbol-name (or tool claudemacs-default-tool)))))

(defun claudemacs--get-workspace-name ()
  "Return the current workspace name if available, or nil.
Checks workspace systems in priority order:
1. Doom Emacs (+workspace-current-name)
2. Doom's persp wrapper (safe-persp-name + get-current-persp)
3. Vanilla perspective.el (persp-current-name)"
  (cl-flet ((valid-ws-p (ws) (and ws (stringp ws) (not (string-empty-p ws)))))
    (cond
     ;; Doom Emacs workspace
     ((fboundp '+workspace-current-name)
      (let ((ws (+workspace-current-name)))
        (when (valid-ws-p ws) ws)))
     ;; Doom's perspective wrapper
     ((and (fboundp 'safe-persp-name) (fboundp 'get-current-persp))
      (let ((ws (safe-persp-name (get-current-persp))))
        (when (valid-ws-p ws) ws)))
     ;; Vanilla perspective.el
     ((fboundp 'persp-current-name)
      (let ((ws (persp-current-name)))
        (when (valid-ws-p ws) ws))))))

(defun claudemacs--session-id (&optional directory)
  "Return an identifier for the current Claudemacs session.
If a workspace is active (checking various workspace packages), use its name;
otherwise fall back to the project root containing DIRECTORY.  DIRECTORY is
used only for the project-root fallback so workspace-name precedence remains
  unchanged."
  (or (claudemacs--get-workspace-name)
      (file-truename (if directory
                         (claudemacs--project-root directory)
                       (claudemacs--project-root)))))

(defun claudemacs--get-instance-numbers-for-tool (tool &optional directory)
  "Get instance numbers currently in use for TOOL in the session for DIRECTORY.
Return a sorted list of integers, such as (1 2 3) when claude, claude-2, and
claude-3 exist."
  (let ((session-id (if directory
                       (claudemacs--session-id directory)
                     (claudemacs--session-id)))
        (tool-str (symbol-name tool))
        (numbers '()))
    (dolist (buf (buffer-list))
      (when (claudemacs--is-claudemacs-buffer-p buf)
        (let ((buf-name (buffer-name buf)))
          ;; Match both "tool:session" and "tool-N:session" patterns
          (when (string-match (format "^\\*claudemacs:%s\\(-\\([0-9]+\\)\\)?:%s\\*$"
                                      (regexp-quote tool-str)
                                      (regexp-quote session-id))
                              buf-name)
            (let ((num-str (match-string 2 buf-name)))
              (push (if num-str (string-to-number num-str) 1) numbers))))))
    (sort numbers #'<)))

(defun claudemacs--get-next-instance-number (tool &optional directory)
  "Get the next available instance number for TOOL in the session for DIRECTORY.
Returns 1 if no instances exist, or the next sequential number."
  (let ((used (if directory
                  (claudemacs--get-instance-numbers-for-tool tool directory)
                (claudemacs--get-instance-numbers-for-tool tool))))
    (if (null used)
        1
      ;; Find first gap or use max+1
      (let ((n 1))
        (while (or (member n used)
                   (and (> n 1)
                        (assq (intern (format "%s-%d" tool n))
                              claudemacs-tool-registry)))
          (setq n (1+ n)))
        n))))

(defun claudemacs--format-tool-instance-name (tool &optional instance-num)
  "Format TOOL with INSTANCE-NUM into display name.
Returns 'tool' for instance 1, 'tool-N' for N > 1."
  (if (or (null instance-num) (= instance-num 1))
      (symbol-name tool)
    (format "%s-%d" tool instance-num)))

(defun claudemacs--split-tool-instance-name (instance-name)
  "Split INSTANCE-NAME into a (TOOL . INSTANCE) pair.
INSTANCE-NAME is the display name produced by
`claudemacs--format-tool-instance-name', so \"codex\" is instance 1 and
\"codex-2\" is instance 2.  Only a trailing number is an instance, which
keeps a hyphenated registry key such as `codex-personal' intact.
The caller's match data is preserved, because callers parse this name out of
a buffer name and go on to read their own match groups."
  (save-match-data
    (let ((exact-tool (intern instance-name)))
      (if (assq exact-tool claudemacs-tool-registry)
          (cons exact-tool 1)
        (if (string-match "\\`\\(.+\\)-\\([0-9]+\\)\\'" instance-name)
            (cons (intern (match-string 1 instance-name))
                  (string-to-number (match-string 2 instance-name)))
          (cons exact-tool 1))))))

(defun claudemacs--get-buffer-name-for-instance (tool instance-num &optional directory)
  "Generate buffer name for TOOL at INSTANCE-NUM.
Format: *claudemacs:TOOL:SESSION-ID* or *claudemacs:TOOL-N:SESSION-ID*.
When DIRECTORY is supplied, use it for the project-root fallback in the
session ID."
  (let ((instance-name (claudemacs--format-tool-instance-name tool instance-num)))
    (format "*claudemacs:%s:%s*" instance-name
            (if directory
                (claudemacs--session-id directory)
              (claudemacs--session-id)))))

(defun claudemacs--get-buffer-name (&optional tool directory)
  "Generate the claudemacs buffer name based on TOOL and workspace session ID.
TOOL defaults to `claudemacs-default-tool' if not specified.
Format: *claudemacs:TOOL:SESSION-ID*
Note: This returns the name for the first instance. Use
`claudemacs--get-buffer-name-for-instance' for specific instances.
When DIRECTORY is supplied, use it for the project-root fallback in the
session ID."
  (let ((tool-name (or tool claudemacs-default-tool)))
    (format "*claudemacs:%s:%s*" tool-name
            (if directory
                (claudemacs--session-id directory)
              (claudemacs--session-id)))))

(defun claudemacs--get-buffer (&optional tool directory)
  "Return existing claudemacs buffer for current session and TOOL.
TOOL defaults to `claudemacs-default-tool' if not specified.  DIRECTORY is
used for the project-root fallback in the session ID."
  (get-buffer (if directory
                  (claudemacs--get-buffer-name tool directory)
                (claudemacs--get-buffer-name tool))))

(defun claudemacs--get-current-session-buffer ()
  "Return the most relevant claudemacs buffer for the current context.
Priority:
1. Current buffer if it's a claudemacs buffer
2. Most recently used session in current workspace
3. Session for default tool in current workspace
4. nil if no sessions exist"
  (cond
   ;; Already in a claudemacs buffer
   ((claudemacs--is-claudemacs-buffer-p (current-buffer))
    (current-buffer))
   ;; Get most recent session in workspace
   (t
    (let ((sessions (claudemacs--list-sessions-for-workspace)))
      (if sessions
          (plist-get (car sessions) :buffer)
        ;; Try default tool as fallback
        (claudemacs--get-buffer claudemacs-default-tool))))))

(defun claudemacs--is-claudemacs-buffer-p (&optional buffer)
  "Return t if BUFFER (or current buffer) is a claudemacs buffer."
  (let ((buf (or buffer (current-buffer))))
    (and (buffer-live-p buf)
         (string-match-p "^\\*claudemacs:" (buffer-name buf)))))

(defun claudemacs--switch-to-buffer (&optional tool)
  "Switch to the claudemacs buffer for current session and TOOL.
TOOL defaults to `claudemacs-default-tool' if not specified.
Returns t if switched successfully, nil if no buffer exists."
  (if-let* ((buffer (claudemacs--get-buffer tool)))
      (progn
        (with-current-buffer buffer
          (unless (and claudemacs--terminal-backend
                       (claudemacs--terminal-ready-p))
            (error "Claudemacs session exists but its terminal is not initialized. Please kill the session buffer and restart"))
          (unless (claudemacs--terminal-live-p)
            (error "Claudemacs session exists but its process is not running. Please kill the session buffer and restart")))
        (display-buffer buffer)
        (select-window (get-buffer-window buffer))
        t)
    nil))

;;;;
;;;; Session Management
;;;;

(defun claudemacs--list-all-sessions ()
  "Return a list of all active claudemacs session buffers.
Each element is a buffer object."
  (seq-filter #'claudemacs--is-claudemacs-buffer-p (buffer-list)))

(defun claudemacs--buffer-session-identity (&optional buffer)
  "Return the authoritative identity plist for BUFFER.

The result contains `:id' and `:provenance'.  The deprecated Claude UUID
variable is accepted as an exact identity so that buffers created by an older
loaded version remain usable after `cp/claudemacs-reload'.  No history lookup
or CWD/recency inference is performed here."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((id (or claudemacs--session-id
                      claudemacs--claude-session-uuid))
              (provenance (if (memq claudemacs--session-id-provenance
                                    claudemacs--session-id-provenances)
                              claudemacs--session-id-provenance
                            'unknown)))
          (list :id id
                :provenance (if (and id
                                     (null claudemacs--session-id)
                                     claudemacs--claude-session-uuid)
                                'exact
                              provenance)))))))

(defun claudemacs--set-session-identity (id provenance &optional force)
  "Set the current buffer's live identity to ID with PROVENANCE.

PROVENANCE must be `exact', `discovered', or `unknown'.  Once a non-unknown
identity has been established it is immutable unless FORCE is non-nil.  This
  prevents a later refresh or a second database observation from remapping a
  live buffer to another conversation.  FORCE is used only when a buffer is
  being deliberately reused for a new tool process."
  ;; Keep every non-nil identity on the same allowlist used by explicit
  ;; resume/branch selection.  Invalid exact/discovered IDs are downgraded to
  ;; unknown rather than being exposed to the live-session view.  A nil ID is
  ;; valid when deliberately resetting a buffer to unknown.
  (setq id (and (claudemacs--safe-session-id-p id) id))
  (unless (memq provenance claudemacs--session-id-provenances)
    (setq provenance 'unknown))
  (unless id
    (setq provenance 'unknown))
  (let ((current (claudemacs--buffer-session-identity)))
    (when (or force
              (eq (plist-get current :provenance) 'unknown)
              (and (equal id (plist-get current :id))
                   (eq provenance (plist-get current :provenance))))
      (setq-local claudemacs--session-id (and (stringp id) id)
                  claudemacs--session-id-provenance provenance)
      ;; Keep the old Claude-only slot synchronized for callers that still
      ;; inspect it.  A discovered Codex ID must never appear there.
      (setq-local claudemacs--claude-session-uuid
                  (when (and (eq (claudemacs--tool-kind-for-buffer
                                  (current-buffer) claudemacs--tool)
                                 'claude)
                             (stringp id)
                             (eq provenance 'exact))
                    id))))
  (claudemacs--buffer-session-identity))

(defun claudemacs--get-session-info (buffer)
  "Extract session information from BUFFER.
Returns a plist with :tool, :tool-kind, :instance, :session-id, :buffer,
:buffer-name, and :claude-uuid (the Claude Code session UUID, if tracked), plus
:authoritative-session-id and :identity-provenance.
The :tool is the registry key and :tool-kind is its captured CLI family.
The :instance is the instance number (1 for claude, 2 for claude-2, etc.).
Returns nil if the buffer is not a claudemacs buffer."
  (when (claudemacs--is-claudemacs-buffer-p buffer)
    (let ((buf-name (buffer-name buffer)))
      ;; Buffer name format: *claudemacs:TOOL:SESSION-ID* or *claudemacs:TOOL-N:SESSION-ID*
      (when (string-match "^\\*claudemacs:\\([^:]+\\):\\(.+\\)\\*$" buf-name)
        (let* ((name-parts (claudemacs--split-tool-instance-name
                            (match-string 1 buf-name)))
               (tool (or (and (local-variable-p 'claudemacs--tool buffer)
                              (buffer-local-value 'claudemacs--tool buffer))
                         (car name-parts)))
               (instance
                (or (and (local-variable-p 'claudemacs--tool-instance buffer)
                         (buffer-local-value 'claudemacs--tool-instance buffer))
                    (cdr name-parts)))
               (tool-kind (claudemacs--tool-kind-for-buffer buffer tool))
               (session-id (match-string 2 buf-name))
               (identity (claudemacs--buffer-session-identity buffer))
               (claude-uuid (and (eq (plist-get identity :provenance) 'exact)
                                 (eq tool-kind 'claude)
                                 (plist-get identity :id))))
          (list :tool tool
                :tool-kind tool-kind
                :instance instance
                :session-id session-id
                :workspace session-id
                :buffer buffer
                :buffer-name buf-name
                :claude-uuid claude-uuid
                :authoritative-session-id (plist-get identity :id)
                :identity-provenance (plist-get identity :provenance)))))))

(defun claudemacs--list-sessions-for-workspace ()
  "Return a list of all active sessions in the current workspace.
Returns a list of plists with :tool, :session-id, :buffer, :buffer-name.
Only includes sessions that match the current workspace's session-id.
Sessions are sorted by most recently accessed (using buffer-display-time)."
  (let* ((current-session-id (claudemacs--session-id))
         (filtered-sessions
          (seq-filter (lambda (info)
                        (and info  ; Filter out nil entries first
                             (string= (plist-get info :session-id) current-session-id)))
                      (mapcar #'claudemacs--get-session-info
                              (claudemacs--list-all-sessions)))))
    ;; Sort by most recently accessed (buffer-display-time)
    (seq-sort-by (lambda (info)
                   (let ((buf (plist-get info :buffer)))
                     (or (condition-case nil
                             (when (and buf (buffer-live-p buf))
                               (with-current-buffer buf
                                 (when (and (boundp 'buffer-display-time)
                                            buffer-display-time)
                                   ;; Convert time value to float for comparison
                                   (float-time buffer-display-time))))
                           (error nil))
                         0.0)))
                 #'>
                 filtered-sessions)))

(defun claudemacs--list-available-tools ()
  "Return a list of tools from registry not yet started in current workspace.
Returns a list of symbols (tool names)."
  (let* ((active-tools (mapcar (lambda (info) (plist-get info :tool))
                               (claudemacs--list-sessions-for-workspace)))
         (all-tools (mapcar #'car claudemacs-tool-registry)))
    (seq-difference all-tools active-tools)))

;;;;
;;;; Session/Tool Selection
;;;;

(defun claudemacs--format-session-choice (info &optional is-current)
  "Format a session INFO plist as a choice string for selection.
If IS-CURRENT is non-nil, mark it as the current session.
Format: 'TOOL(-N):SESSION-ID (current)' or 'TOOL(-N):SESSION-ID'."
  (let* ((tool (plist-get info :tool))
         (instance (plist-get info :instance))
         (session-id (plist-get info :session-id))
         (instance-name (claudemacs--format-tool-instance-name tool instance)))
    (if is-current
        (format "%s:%s (current)" instance-name session-id)
      (format "%s:%s" instance-name session-id))))

(defun claudemacs--find-session-window ()
  "Find a window currently displaying a claudemacs buffer, or nil."
  (seq-find (lambda (win)
              (claudemacs--is-claudemacs-buffer-p (window-buffer win)))
            (window-list)))

(defun claudemacs--session-preview-state (choices)
  "Create a consult state function for previewing session buffers.
CHOICES is an alist of (display-string . session-info-plist).
Previews in a window showing a claudemacs buffer if one exists,
otherwise falls back to the window that invoked the minibuffer."
  (let ((lookup (make-hash-table :test 'equal))
        (target-win (or (claudemacs--find-session-window)
                        (consult--original-window)))
        orig-buf)
    (setq orig-buf (window-buffer target-win))
    (dolist (choice choices)
      (puthash (car choice) (plist-get (cdr choice) :buffer) lookup))
    (lambda (action cand)
      (pcase action
        ('preview
         (when-let* ((buf (and cand (gethash cand lookup))))
           (when (buffer-live-p buf)
             (when (window-live-p target-win)
               (with-selected-window target-win
                 (switch-to-buffer buf 'norecord))))))
        ((or 'exit 'return)
         (when (and (window-live-p target-win) (buffer-live-p orig-buf))
           (with-selected-window target-win
             (switch-to-buffer orig-buf 'norecord))))))))

(defun claudemacs--switch-to-session ()
  "Switch to a session in current workspace.
If one session exists, switch directly to it.
If multiple sessions exist, prompt for selection.
If no sessions exist, open the Start Session menu."
  (let ((active-sessions (claudemacs--list-sessions-for-workspace)))
    (cond
     ;; No sessions - open start menu
     ((null active-sessions)
      (claudemacs-start-menu))
     ;; Exactly one session - switch directly
     ((= (length active-sessions) 1)
      (let ((buffer (plist-get (car active-sessions) :buffer)))
        (display-buffer buffer)
        (select-window (get-buffer-window buffer))))
     ;; Multiple sessions - prompt for selection with preview
     (t
      (let* ((choices (mapcar (lambda (info)
                                (cons (claudemacs--format-session-choice info)
                                      info))
                              active-sessions))
             (candidates (mapcar #'car choices))
             (selected-name
              (if (fboundp 'consult--read)
                  (consult--read candidates
                                 :prompt "Switch to session: "
                                 :require-match t
                                 :sort nil
                                 :state (claudemacs--session-preview-state choices))
                (completing-read "Switch to session: " candidates nil t)))
             (info (cdr (assoc selected-name choices))))
        (when info
          (let ((buffer (plist-get info :buffer)))
            (display-buffer buffer)
            (select-window (get-buffer-window buffer)))))))))

(defun claudemacs--get-other-session ()
  "Get the second most recent session (the 'other' session).
Returns the session info plist, or nil if there aren't enough sessions."
  (let ((active-sessions (claudemacs--list-sessions-for-workspace)))
    (when (>= (length active-sessions) 2)
      (cadr active-sessions))))

(defun claudemacs--get-flycheck-errors-on-line ()
  "Get all flycheck errors on the current line."
  (when (and (bound-and-true-p flycheck-mode)
             (fboundp 'flycheck-overlay-errors-in))
    (let ((line-start (line-beginning-position))
          (line-end (line-end-position)))
      (flycheck-overlay-errors-in line-start line-end))))

(defun claudemacs--format-flycheck-errors (errors)
  "Format flycheck ERRORS for display to Claude."
  (cond
   ((null errors) "")
   ((= 1 (length errors))
    (flycheck-error-message (car errors)))
   ((<= (length errors) 3)
    (format "(%d errors: %s)"
            (length errors)
            (mapconcat (lambda (err) (flycheck-error-message err))
                      errors "; ")))
   (t
    (format "(%d errors including: %s; ...)"
            (length errors)
            (mapconcat (lambda (err) (flycheck-error-message err))
                      (seq-take errors 2) "; ")))))

;;;; Terminal Integration

;;;; Bell Handling
(defconst claudemacs--notification-message-width 180
  "Width at which a tool-supplied notification message is truncated.")

(defun claudemacs--notification-tool-name ()
  "Return the display name of the AI tool owning the current buffer."
  (capitalize (symbol-name (or claudemacs--tool claudemacs-default-tool))))

(defun claudemacs--notification-message-text (text)
  "Return TEXT as a one-line notification body, or nil when it has no content.
Control characters are collapsed to spaces so a multi-line agent message
stays readable in a system notification."
  (when (stringp text)
    (let ((clean (string-trim
                  (replace-regexp-in-string
                   "[ \t]+" " "
                   (replace-regexp-in-string "[[:cntrl:]]+" " " text)))))
      (unless (string-empty-p clean)
        (truncate-string-to-width
         clean claudemacs--notification-message-width nil nil t)))))

(defvar-local claudemacs--last-tool-notification-time nil
  "When this session last handled an OSC notification from the tool.")

(defconst claudemacs--tool-notification-bell-window 2.0
  "Seconds after a tool notification during which a BEL is a duplicate.
A tool can be configured to announce the same event twice, as Claude Code
does with its `iterm2_with_bell' channel: once with its message and once as
a bare bell.  The second one has nothing to add, so it is dropped.")

(defconst claudemacs--bell-notification-delay 0.01
  "Seconds to wait for a preceding OSC notification before handling BEL.")

(defun claudemacs--duplicate-bell-p ()
  "Return non-nil when a BEL just repeats a notification already shown."
  (and claudemacs--last-tool-notification-time
       (< (float-time (time-subtract nil claudemacs--last-tool-notification-time))
          claudemacs--tool-notification-bell-window)))

(defun claudemacs--show-generic-notification (&optional title)
  "Show the generic completion notification with optional TITLE."
  (let ((tool-name (claudemacs--notification-tool-name)))
    (claudemacs--system-notification
     (format "%s finished and is awaiting your input" tool-name)
     title)))

(defun claudemacs--deliver-bell-notification (buffer)
  "Handle a deferred bell notification belonging to BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and claudemacs-notify-on-await
                 (not (claudemacs--duplicate-bell-p)))
        (claudemacs--show-generic-notification)))))

(defun claudemacs--bell-handler (&rest _arguments)
  "Queue a bell event from the current AI tool.
The short delay lets a message-bearing OSC event suppress its duplicate BEL."
  (when (and claudemacs-notify-on-await
             (buffer-live-p (current-buffer)))
    (run-at-time claudemacs--bell-notification-delay nil
                 #'claudemacs--deliver-bell-notification (current-buffer))))

(defun claudemacs--notification-handler (body &optional title)
  "Handle a desktop notification from the current AI tool.

BODY is the message the tool sent with its OSC 9 or OSC 777 notification and
TITLE is the title it supplied, if any.  Tools send these instead of a
bell character, so this is the same completion event `claudemacs--bell-handler'
reports — just one that arrives with the tool's own text attached.  Fall back
to that generic handler when there is no usable message."
  (when claudemacs-notify-on-await
    (let ((message (and claudemacs-notify-with-tool-message
                        (claudemacs--notification-message-text body))))
      (setq-local claudemacs--last-tool-notification-time (current-time))
      (if message
          (claudemacs--system-notification
           message
           (or (claudemacs--notification-message-text title)
               (claudemacs--notification-tool-name)))
        (claudemacs--show-generic-notification
         (claudemacs--notification-message-text title))))))


(defun claudemacs--windows-notification-shortcut ()
  "Return the installed Windows notification shortcut, or nil."
  (when-let* ((appdata (getenv "APPDATA"))
              (shortcut
               (expand-file-name
                "Microsoft/Windows/Start Menu/Programs/Claudemacs.lnk"
                appdata))
              ((file-exists-p shortcut)))
    shortcut))

(defvar claudemacs--windows-notification-identity-ready nil
  "Whether this Emacs process refreshed the Windows notification identity.")

(defun claudemacs--windows-notification-script ()
  "Return the installed Windows notification helper script, or nil."
  (when-let* ((library (or (symbol-file 'claudemacs--system-notification
                                        'defun)
                           (locate-library "claudemacs")))
              (script (expand-file-name "claudemacs-toast.ps1"
                                        (file-name-directory library)))
              ((file-readable-p script)))
    script))

(defun claudemacs--install-windows-notification-shortcut ()
  "Install and return the per-user Windows notification shortcut.
Signal an error if installation fails."
  (let* ((script (claudemacs--windows-notification-script))
         (powershell (executable-find "powershell"))
         (emacs-executable
          (expand-file-name invocation-name invocation-directory)))
    (unless (and script (file-readable-p script))
      (error "Cannot find claudemacs-toast.ps1"))
    (unless powershell
      (error "Cannot find Windows PowerShell"))
    (with-temp-buffer
      (let ((status (call-process
                     powershell nil t nil
                     "-NoProfile" "-ExecutionPolicy" "Bypass"
                     "-File" script "-Install" "-TargetPath"
                     emacs-executable)))
        (unless (zerop status)
          (error "Windows notification setup failed (status %s): %s"
                 status (string-trim (buffer-string))))))
    (or (claudemacs--windows-notification-shortcut)
        (error "Windows notification shortcut was not created"))))

(defun claudemacs--launch-windows-notification (message title)
  "Launch the Windows toast helper directly with MESSAGE and TITLE."
  (let ((script (claudemacs--windows-notification-script))
        (powershell (executable-find "powershell")))
    (unless script
      (error "Cannot find claudemacs-toast.ps1"))
    (unless powershell
      (error "Cannot find Windows PowerShell"))
    (make-process
     :name "claudemacs-toast"
     :buffer nil
     :command (list powershell
                    "-NoProfile" "-WindowStyle" "Hidden"
                    "-ExecutionPolicy" "Bypass"
                    "-File" script
                    "-Title" title
                    "-Message" message
                    "-TimeoutSeconds"
                    (number-to-string
                     claudemacs-notification-timeout-windows))
     :connection-type 'pipe
     :noquery t)))

(defun claudemacs--fallback-windows-notification (message title)
  "Show a best-effort Windows notification with MESSAGE and TITLE."
  (if (fboundp 'w32-notification-notify)
      (w32-notification-notify :level 'info :title title :body message)
    (message "%s: %s" title message)))

(defun claudemacs--windows-notification (message title)
  "Show a non-modal Windows notification with MESSAGE and TITLE."
  (condition-case error-data
      (progn
        ;; Refresh once per Emacs process.  This repairs shortcuts left behind
        ;; by package upgrades or moves without adding work to every toast.
        (unless claudemacs--windows-notification-identity-ready
          (claudemacs--install-windows-notification-shortcut)
          (setq claudemacs--windows-notification-identity-ready t))
        (claudemacs--launch-windows-notification message title))
    (error
     (display-warning 'claudemacs (error-message-string error-data))
     (claudemacs--fallback-windows-notification message title))))

;;;###autoload
(defun claudemacs-setup-windows-notifications ()
  "Reinstall the per-user identity for Windows toast notifications.
Claudemacs normally installs this automatically when first needed."
  (interactive)
  (unless (eq system-type 'windows-nt)
    (user-error "This setup command is only needed on Windows"))
  (condition-case error-data
      (progn
        (claudemacs--install-windows-notification-shortcut)
        (setq claudemacs--windows-notification-identity-ready t)
        (message "Claudemacs Windows notifications installed"))
    (error (user-error "%s" (error-message-string error-data)))))

(defun claudemacs--escape-applescript-string (string)
  "Escape STRING for use inside an AppleScript double-quoted literal.
Notification text can come from the AI tool itself, so it may contain the
quotes and backslashes that would otherwise end the literal."
  (replace-regexp-in-string "[\\\"]" "\\\\\\&" string))

(defun claudemacs--system-notification (message &optional title)
  "Show a system notification with MESSAGE and optional TITLE.
This works across macOS, Linux, and Windows platforms."
  (let ((title (or title "Claudemacs"))
        (message (or message "Claudemacs is finished and awaiting your input")))
    (cond
     ;; macOS
     ((eq system-type 'darwin)
      (call-process "osascript" nil nil nil
                    "-e" (format "display notification \"%s\" with title \"%s\" sound name \"%s\""
                                (claudemacs--escape-applescript-string message)
                                (claudemacs--escape-applescript-string title)
                                (claudemacs--escape-applescript-string
                                 claudemacs-notification-sound-mac))))
     ;; Linux with notify-send and canberra-gtk-play
     ((and (eq system-type 'gnu/linux)
           (executable-find "notify-send"))
      (let ((args (if claudemacs-notification-auto-dismiss-linux
                      (list "--hint=int:transient:1" title message)
                    (list title message))))
        (apply #'call-process "notify-send" nil nil nil args))
      (when (and (not (string-empty-p claudemacs-notification-sound-linux))
                 (executable-find "canberra-gtk-play"))
        (call-process "canberra-gtk-play" nil nil nil
                      "--id" claudemacs-notification-sound-linux)))
     ;; Linux with kdialog (KDE)
     ((and (eq system-type 'gnu/linux)
           (executable-find "kdialog"))
      (call-process "kdialog" nil nil nil "--passivepopup"
                    (format "%s: %s" title message) "3"))
     ;; Windows taskbar/Notification Center notification.  Unlike a Forms
     ;; message box, this is non-modal and dismisses itself.
     ((eq system-type 'windows-nt)
      (claudemacs--windows-notification message title))
     ;; Fallback: show in Emacs message area
     (t (message "%s: %s" title message)))))

(defun claudemacs--setup-terminal-integration (buffer &optional retry-count)
  "Set up terminal integration for BUFFER.
Retries using RETRY-COUNT up to 10 times if the backend is not ready yet."
  (let ((retry-count (or retry-count 0)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (cond
         ((and claudemacs--terminal-backend
               (claudemacs--terminal-ready-p))
          (message "Claudemacs terminal is ready, setting up integrations")
          (claudemacs--terminal-setup-faces)
          (claudemacs--setup-buffer-keymap)
          (claudemacs-setup-bell-handler)
          (run-hooks 'claudemacs-startup-hook))
         ((< retry-count 10)
          (message "Claudemacs terminal not ready; retrying in 0.5s (attempt %d/10)"
                   (1+ retry-count))
          (run-with-timer 0.5 nil
                          (lambda ()
                            (claudemacs--setup-terminal-integration
                             buffer (1+ retry-count))))))))))

;;;###autoload
(defun claudemacs-setup-bell-handler ()
  "Set up or re-setup the completion notification handler.
Use this if system notifications aren't working after starting a session."
  (interactive)
  (if-let* ((buffer (claudemacs--get-current-session-buffer)))
      (with-current-buffer buffer
        (claudemacs--terminal-setup-buffer #'claudemacs--bell-handler)
        (claudemacs--terminal-setup-notifications #'claudemacs--notification-handler)
        (message "Bell handler configured for Claudemacs session"))
    (user-error "No Claudemacs session is active")))

(defun claudemacs--ret-key ()
  "Send a return key event to the current terminal."
  (interactive)
  (claudemacs--terminal-send-key 'return))

(defun claudemacs--meta-ret-key ()
  "Send meta-return to the current terminal."
  (interactive)
  (claudemacs--terminal-send-key 'meta-return))

(defun claudemacs--send-escape ()
  "Send ESC to the current terminal."
  (interactive)
  ;; Ghostel sets `quit-flag' before dispatching C-g because it keeps
  ;; `inhibit-quit' non-nil while routing terminal input.  Clear it so the
  ;; escape key reaches the selected backend instead of aborting the command.
  (setq quit-flag nil)
  (claudemacs--terminal-send-key 'escape))

(defun claudemacs--setup-ghostel-escape-map ()
  "Keep Claudemacs' Ghostel key overrides ahead of Ghostel's mode maps.

Ghostel replaces its local map whenever it switches input modes.  An
emulation map remains active across those replacements while staying local to
this Claudemacs session buffer.  Rebuild the map on every setup so changing
the return-key options cannot leave stale bindings behind."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g") #'claudemacs--send-escape)

    ;; Ghostel accepts both the control-character and named-event forms of
    ;; return keys.  Bind both forms because the event Emacs reports depends
    ;; on whether input came from a terminal or a graphical session.
    (when claudemacs-m-return-is-submit
      (dolist (key '("RET" "<return>"))
        (define-key map (kbd key) #'claudemacs--meta-ret-key))
      (dolist (key '("M-RET" "<M-return>"))
        (define-key map (kbd key) #'claudemacs--ret-key)))
    (when claudemacs-shift-return-newline
      (dolist (key '("S-RET" "<S-return>"))
        (define-key map (kbd key) #'claudemacs--meta-ret-key)))

    (setq-local claudemacs--ghostel-escape-map-active t)
    (setq-local claudemacs--ghostel-escape-map-alist
                `((claudemacs--ghostel-escape-map-active . ,map)))
    (setq-local emulation-mode-map-alists
                (cons 'claudemacs--ghostel-escape-map-alist
                      (delq 'claudemacs--ghostel-escape-map-alist
                            (copy-sequence emulation-mode-map-alists))))))

;;;###autoload
(defun claudemacs-send-yes ()
  "Send yes (RET) to the active Claudemacs session."
  (interactive)
  (claudemacs--validate-process)
  (let ((buffer (claudemacs--get-current-session-buffer)))
    (with-current-buffer buffer
      (claudemacs--send-return-for-tool buffer))))

;;;###autoload
(defun claudemacs-send-no ()
  "Send no (ESC) to the active Claudemacs session."
  (interactive)
  (claudemacs--validate-process)
  (let ((buffer (claudemacs--get-current-session-buffer)))
    (with-current-buffer buffer
      (claudemacs--terminal-send-key 'escape))))

(defun claudemacs--setup-buffer-keymap ()
  "Set up truly buffer-local keymap for claudemacs buffers with custom key bindings."
  (when (claudemacs--is-claudemacs-buffer-p)
    (message "Setting up buffer-local keymap for claudemacs buffer: %s" (buffer-name))

    ;; Inherit the selected terminal mode's local map.
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map (current-local-map))

      ;; Override specific keys for claudemacs functionality
      (define-key map (kbd "C-g") #'claudemacs--send-escape)
      (message "Defined C-g -> claudemacs--send-escape")

      ;; Ghostel replaces its local map when switching input modes, so keep
      ;; this override in a buffer-local emulation map as well.
      (when (eq claudemacs--terminal-backend 'ghostel)
        (claudemacs--setup-ghostel-escape-map))

      ;; Handle return key swapping if enabled
      (when claudemacs-m-return-is-submit
        (define-key map (kbd "<return>") #'claudemacs--meta-ret-key)
        (define-key map (kbd "<M-return>") #'claudemacs--ret-key)
        (message "Swapped RET and M-RET"))
      
      ;; Handle shift-return newline if enabled
      (when claudemacs-shift-return-newline
        (define-key map (kbd "<S-return>") #'claudemacs--meta-ret-key)
        (message "Defined S-RET -> newline"))

      ;; Apply the keymap as truly buffer-local
      (use-local-map map)
      (message "Applied buffer-local keymap successfully"))))

(defun claudemacs--get-shell-name ()
  "Get the path to the user's shell (e.g., '/bin/zsh', '/bin/bash').
Falls back to '/bin/sh' if SHELL environment variable is not set."
  (or (getenv "SHELL") "/bin/sh"))

(defun claudemacs--configure-terminal-process-environment (tool backend)
  "Apply terminal-specific environment settings for TOOL and BACKEND.

Claude Code's normal prompt renderer draws its own cursor indicator.  Ghostel
provides the terminal cursor to Emacs, so use Claude Code's accessibility
cursor path for Ghostel sessions to avoid leaving the renderer's indicator at
the prompt's original position while retaining the real Emacs cursor."
  (when (and (claudemacs--tool-kind-p tool 'claude)
             (eq backend 'ghostel))
    (setenv "CLAUDE_CODE_ACCESSIBILITY" "1")))

(defun claudemacs--codex-local-daemon-requested-p (tool switches)
  "Return non-nil when TOOL's SWITCHES select Codex's local server."
  (and (claudemacs--tool-kind-p tool 'codex)
       (equal (cadr (member "--remote" switches)) "unix://")))

(defun claudemacs--codex-remote-switches (tool switches work-dir)
  "Add WORK-DIR to local Codex remote SWITCHES when needed.
The local server otherwise uses its current project instead of the project
Claudemacs is opening."
  (if (and (claudemacs--codex-local-daemon-requested-p tool switches)
           (not (or (member "--cd" switches)
                    (member "-C" switches))))
      (append switches (list "--cd" work-dir))
    switches))

(defun claudemacs--ensure-codex-local-daemon (program use-shell-env)
  "Start PROGRAM's managed local server if it is not already running.
The caller binds `process-environment' to the selected tool profile, so a
profile with its own `CODEX_HOME' starts the server for that home.
USE-SHELL-ENV runs PROGRAM through the same shell as the terminal launch."
  (with-temp-buffer
    (let* ((arguments '("app-server" "daemon" "start"))
           (status
            (if use-shell-env
                (call-process
                 (claudemacs--get-shell-name) nil t nil "-c"
                 (mapconcat #'shell-quote-argument
                            (cons program arguments) " "))
              (apply #'call-process program nil t nil arguments))))
      (unless (and (integerp status) (zerop status))
        (error "Codex background server could not start: %s"
               (string-trim (buffer-string)))))))

(defun claudemacs--argument-value (args flag)
  "Return the string value following FLAG in ARGS, or nil.

The value is intentionally returned before validation so callers can reject
empty, option-like, or otherwise unsafe IDs rather than silently treating
them as absent."
  (when-let* ((tail (member flag args)))
    (let ((value (cadr tail)))
      (when (stringp value)
        value))))

(defun claudemacs--identity-plan-for-start (tool args session-uuid)
  "Return the identity plan implied by TOOL, ARGS, and SESSION-UUID.

The result is a plist with `:provenance' and optional `:id'.  Codex fork/new
sessions intentionally return `:unknown': the CLI does not expose a
destination-ID option and no post-start history association is attempted.
An explicit Codex resume remains exact."
  (let ((explicit-session-id (claudemacs--argument-value args "--session-id"))
        (resume-id (claudemacs--argument-value args "--resume"))
        (codex-resume-id (claudemacs--argument-value args "resume"))
        (codex-fork-source-id (claudemacs--argument-value args "fork")))
    ;; Validate values even when they are empty or option-like.  This keeps a
    ;; malformed explicit flag from reaching the process argv as an implicit
    ;; picker or another option.
    (when (member "--session-id" args)
      (setq explicit-session-id
            (claudemacs--validate-session-id explicit-session-id tool)))
    (when (member "--resume" args)
      (setq resume-id
            (claudemacs--validate-session-id resume-id tool)))
    (when (and (claudemacs--tool-kind-p tool 'codex) (member "fork" args))
      (setq codex-fork-source-id
            (claudemacs--validate-session-id codex-fork-source-id tool)))
    (pcase (claudemacs--tool-kind tool)
      ('claude
       (cond
        ((member "--session-id" args)
         (list :provenance 'exact :id explicit-session-id))
        ;; A fork without an explicit destination creates a new conversation;
        ;; the source ID must not be mistaken for that destination.
        ((and (member "--fork-session" args) (null session-uuid))
         (list :provenance 'unknown))
        (session-uuid (list :provenance 'exact :id session-uuid))
        (resume-id
         (list :provenance 'exact
               :id (claudemacs--validate-session-id resume-id tool)))
        (t (list :provenance 'unknown))))
      ('codex
       (cond
        ((member "resume" args)
         (list :provenance 'exact
               :id (claudemacs--validate-session-id
                    codex-resume-id tool)))
        (t (list :provenance 'unknown))))
      (_ (list :provenance 'unknown)))))

(defun claudemacs--start (work-dir &optional tool instance-num &rest args)
  "Start AI coding tool TOOL in WORK-DIR with ARGS.
TOOL defaults to `claudemacs-default-tool' if not specified.
INSTANCE-NUM specifies which instance number to use (1, 2, 3, etc.).
If INSTANCE-NUM is nil, the next available instance number is used.
The tool configuration is looked up in `claudemacs-tool-registry'."
  (let* ((tool-name (or tool claudemacs-default-tool))
         (terminal-backend claudemacs-terminal-backend)
         (tool-config (claudemacs--get-tool-config tool-name))
         ;; Validate the tool's environment before a session buffer exists, so
         ;; a malformed `:env' never leaves an empty buffer behind.
         (tool-env (claudemacs--effective-tool-env tool-name))
         (instance (or instance-num
                       (claudemacs--get-next-instance-number tool-name work-dir)))
         (default-directory work-dir)
         ;; Generate UUID and validate explicit identity arguments before
         ;; allocating the session buffer.  Invalid launch IDs must fail
         ;; without leaving an empty live-session buffer behind.
         (session-uuid (when (and (claudemacs--tool-kind-p tool-name 'claude)
                                  (not (member "--session-id" args))
                                  (not (member "--resume" args))
                                  (not (member "--continue" args)))
                         (claudemacs--generate-uuid)))
         (identity-plan
          (claudemacs--identity-plan-for-start
           tool-name args session-uuid))
         (buffer-name (claudemacs--get-buffer-name-for-instance
                       tool-name instance work-dir))
         (_reserved-name-check
          (when (and (> instance 1)
                     (assq (intern (format "%s-%d" tool-name instance))
                           claudemacs-tool-registry))
            (user-error "Instance name `%s-%d' is reserved by a tool profile"
                        tool-name instance)))
         (existing-buffer (get-buffer buffer-name))
         (_buffer-owner-check
          (when (and existing-buffer
                     (local-variable-p 'claudemacs--tool existing-buffer)
                     (buffer-local-value 'claudemacs--tool existing-buffer)
                     (not (eq (buffer-local-value 'claudemacs--tool existing-buffer)
                              tool-name)))
            (user-error "Buffer name `%s' is already owned by tool `%s'"
                        buffer-name
                        (buffer-local-value 'claudemacs--tool existing-buffer))))
         (buffer (get-buffer-create buffer-name))
         ;; Capture buffer-local and tool-specific values before switching buffers
         (program (or (plist-get tool-config :program) claudemacs-program))
         (program-switches
          (append (claudemacs--get-tool-notification-switches tool-name)
                  claudemacs-program-switches
                  (plist-get tool-config :switches)))
         (use-shell-env claudemacs-use-shell-env)
         (process-environment
          (append tool-env claudemacs-process-environment process-environment)))
    (claudemacs--configure-terminal-process-environment
     tool-name terminal-backend)
    ;; Verify program exists before attempting to start
    (unless (or use-shell-env (executable-find program))
      (kill-buffer buffer)
      (error "Program '%s' not found in PATH" program))

    (condition-case error-data
        (progn
          ;; Load before displaying the new buffer so a missing optional package
          ;; fails without leaving an empty session window behind.
          (claudemacs--terminal-ensure-backend terminal-backend)
          (let* ((window (display-buffer buffer))
                 (process-adaptive-read-buffering nil)
                 (uuid-args (when session-uuid
                              (list "--session-id" session-uuid)))
                 (switches (claudemacs--codex-remote-switches
                            tool-name
                            (remove nil
                                    (append uuid-args args program-switches))
                            work-dir))
                 (start-program program)
                 (start-switches switches))
            (when (claudemacs--codex-local-daemon-requested-p
                   tool-name switches)
              (claudemacs--ensure-codex-local-daemon program use-shell-env))
            (when use-shell-env
              (setq start-program (claudemacs--get-shell-name)
                    start-switches
                    (list "-c"
                          (mapconcat #'shell-quote-argument
                                     (cons program switches) " "))))
            (with-current-buffer buffer
              (cd work-dir)
              (claudemacs--terminal-start
               buffer terminal-backend start-program start-switches)

              ;; Set session state after the backend establishes its major mode.
              (setq-local claudemacs--cwd work-dir)
              (setq-local claudemacs--tool tool-name
                          claudemacs--tool-instance instance
                          claudemacs--session-tool-kind
                          (claudemacs--tool-kind tool-name)
                          claudemacs--session-tool-kind-set-p t)
              ;; A reused buffer can still hold the identity of an older
              ;; process.  Reset it before establishing this process's
              ;; identity; ordinary library reloads never call this path and
              ;; therefore preserve their buffer-local identity.
              (claudemacs--set-session-identity nil 'unknown t)
              (let ((provenance (plist-get identity-plan :provenance))
                    (identity-id (plist-get identity-plan :id)))
                (when (eq provenance 'exact)
                  (claudemacs--set-session-identity identity-id 'exact t)))
              (claudemacs--terminal-post-display buffer)
              (run-with-timer
               0.1 nil
               (lambda ()
                 (claudemacs--setup-terminal-integration buffer))))
            (when claudemacs-switch-to-buffer-on-create
              (select-window window))
            buffer))
      (error
       (when (buffer-live-p buffer)
         (kill-buffer buffer))
       (error "Failed to start %s with terminal backend `%s': %s"
              program terminal-backend (error-message-string error-data))))))

(defun claudemacs--translate-args-for-tool (tool args)
  "Translate generic ARGS to tool-specific arguments for TOOL.
Converts generic switches like '--dangerous-skip-permissions' to
tool-specific equivalents."
  (mapcar (lambda (arg)
            (cond
             ;; Translate dangerous skip permissions
             ((string= arg "--dangerous-skip-permissions")
              (cond
               ((claudemacs--tool-kind-p tool 'claude)
                "--dangerously-skip-permissions")
               ((claudemacs--tool-kind-p tool 'codex)
                "--dangerously-bypass-approvals-and-sandbox")
               (t arg)))  ; Unknown tool, pass through
             ;; All other args pass through unchanged
             (t arg)))
          args))

(defun claudemacs--expand-transient-args (args)
  "Expand transient command strings in ARGS into individual arguments.

Transient's custom `-f' and UUID `-u' options read one string, while process
commands need one list element per command-line argument.  Use Emacs-style
quoting so a value containing spaces can still be kept as one argument."
  (apply #'append
         (mapcar (lambda (arg)
                   (if (stringp arg)
                       (split-string-and-unquote arg)
                     (list arg)))
                 args)))

(defun claudemacs--explicit-resume-command-p (tool args)
  "Return non-nil when a raw transient argument starts TOOL's resume command.

This is used by the resume menu to distinguish an explicit `-u' or `-f'
command such as `resume SESSION-ID' from ordinary extra options such as
`--model MODEL'.
Inspect the raw values so a model or other option whose value happens to be
`resume' is not mistaken for an explicit resume command."
  (let ((resume-flag (claudemacs--get-resume-flag tool)))
    (seq-some (lambda (arg)
                (and (stringp arg)
                     (equal (car (split-string-and-unquote arg)) resume-flag)))
              args)))

(defun claudemacs--run-with-args (tool &optional arg &rest args)
  "Start a new instance of AI coding tool TOOL with ARGS.
TOOL should be a symbol from `claudemacs-tool-registry'.
With prefix ARG, prompt for the project directory.  A directory string in ARG
is used directly; this lets an explicit history selection preserve its
authoritative working directory without prompting a second time.
ARGS are translated to tool-specific arguments.
Always creates a new instance (claude, claude-2, etc.)."
  (let* ((explicit-dir (cond
                        ((stringp arg) arg)
                        (arg (read-directory-name "Project directory: "))))
         (work-dir (or explicit-dir (claudemacs--project-root)))
         (translated-args (claudemacs--translate-args-for-tool tool args)))
    (apply #'claudemacs--start work-dir tool nil translated-args)))

;;;; Interactive Commands
;;;###autoload
(defun claudemacs-switch-to-session ()
  "Switch to the most recent session in current workspace.
Errors if no sessions exist (use Start submenu to create one)."
  (interactive)
  (claudemacs--switch-to-session))

;;;###autoload
(defun claudemacs-switch-other ()
  "Switch to the other (second most recent) session.
Useful when you have 2+ sessions and want to toggle between them."
  (interactive)
  (if-let* ((other-session (claudemacs--get-other-session)))
      (let ((buffer (plist-get other-session :buffer)))
        (display-buffer buffer)
        (select-window (get-buffer-window buffer)))
    (error "Need at least 2 sessions to switch to 'other' session")))

(defun claudemacs--get-next-available-tool ()
  "Get the next tool that isn't currently running.
Returns the first available tool from registry that doesn't have a session,
or nil if all tools are running."
  (car (claudemacs--list-available-tools)))

;;;###autoload
(defun claudemacs-kill ()
  "Kill Claudemacs process and close its window.
Works with the most relevant session (current buffer, or most recent)."
  (interactive)
  (if-let* ((claudemacs-buffer (claudemacs--get-current-session-buffer)))
      (progn
        (let ((tool (buffer-local-value 'claudemacs--tool claudemacs-buffer)))
          (with-current-buffer claudemacs-buffer
            (claudemacs--terminal-kill))
          (when (buffer-live-p claudemacs-buffer)
            (kill-buffer claudemacs-buffer))
          (message "Claudemacs session (%s) killed" tool)))
    (error "There is no Claudemacs session in this workspace or project")))

(defun claudemacs-kill-specific-session ()
  "Select and kill a specific Claudemacs session.
Presents a list of all active sessions in the workspace for selection."
  (interactive)
  (let ((sessions (claudemacs--list-sessions-for-workspace)))
    (if (not sessions)
        (error "No Claudemacs sessions active in this workspace")
      (let* ((choices (mapcar (lambda (info)
                                (cons (claudemacs--format-session-choice info)
                                      info))
                              sessions))
             (selected-name (completing-read "Kill session: " choices nil t))
             (info (cdr (assoc selected-name choices))))
        (when info
          (let* ((buffer (plist-get info :buffer))
                 (tool (plist-get info :tool)))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (claudemacs--terminal-kill))
              (when (buffer-live-p buffer)
                (kill-buffer buffer))
              (message "Claudemacs session (%s) killed" tool))))))))

;;;###autoload
(defun claudemacs-branch-session ()
  "Branch from an explicitly selected authoritative session.

Claude receives an explicit source and generated destination UUID.  Codex
receives an explicit source ID, but its new destination remains unknown
because the installed CLI has no destination-ID option.  A bare `--continue',
`resume', `--last', or picker fallback is never used."
  (interactive)
  (let* ((sessions (claudemacs--list-sessions-for-workspace))
         (session (car sessions)))
    (unless session
      (user-error "No live Claudemacs session is available to branch"))
    (let* ((tool (plist-get session :tool))
           (tool-kind (plist-get session :tool-kind))
           (session-buffer (plist-get session :buffer))
           (_family-check
            (unless (eq tool-kind (claudemacs--tool-kind tool))
              (user-error
               "Tool profile `%s' changed family; restart it before branching"
               tool)))
           (work-dir (and (buffer-live-p session-buffer)
                          (buffer-local-value 'claudemacs--cwd session-buffer)))
           (work-dir (or work-dir (claudemacs--project-root)))
           (live-identities
            (seq-filter
             (lambda (info)
               (memq (plist-get info :identity-provenance)
                     '(exact discovered)))
             (seq-filter (lambda (info) (eq (plist-get info :tool) tool))
                         sessions)))
           (source-id
            (cond
             ;; A single tracked identity is already authoritative.  When
             ;; there are multiple sessions, let the history provider choose
             ;; explicitly rather than using display order.
             ((and (= (length live-identities) 1)
                   (= (length (seq-filter
                               (lambda (info) (eq (plist-get info :tool) tool))
                               sessions))
                      1))
              (plist-get (car live-identities) :authoritative-session-id))
             (t (claudemacs--select-history-session-id tool work-dir))))
           (destination-id (when (eq tool-kind 'claude)
                             (claudemacs--generate-uuid)))
           (branch-args (or (claudemacs--get-branch-args
                            tool source-id destination-id)
                            (user-error
                             "No authoritative %s session ID is available"
                             (capitalize (symbol-name tool))))))
      (apply #'claudemacs--start work-dir tool nil branch-args))))

(defun claudemacs--validate-process ()
  "Validate that the Claudemacs process is alive and running.
Works with the most relevant session (current buffer, or most recent)."
  (let ((buffer (claudemacs--get-current-session-buffer)))
    (unless buffer
      (error "No Claudemacs session is active"))
    (with-current-buffer buffer
      (unless (and claudemacs--terminal-backend
                   (claudemacs--terminal-ready-p))
        (error "Claudemacs session exists but terminal is not initialized. Please kill buffer and restart"))
      (unless (claudemacs--terminal-live-p)
        (error "Claudemacs session exists but process is not running. Please kill buffer and restart"))))
  t)

(defun claudemacs--validate-file-and-session ()
  "Validate that we have a file, project, and active Claudemacs session."
  ;; Buffer must be visiting a file because all calling functions use claudemacs--get-file-context
  ;; which depends on buffer-file-name for relative path calculation and Claude context
  (unless (buffer-file-name)
    (error "Buffer is not visiting a file - save the buffer first or switch to a file buffer"))
  (unless (claudemacs--project-root)
    (error "Not in a project"))
  (claudemacs--validate-process))

(defun claudemacs--get-session-cwd ()
  "Get the stored cwd from the current session."
  (if-let* ((buffer (claudemacs--get-current-session-buffer)))
      (with-current-buffer buffer
        claudemacs--cwd)))

(defun claudemacs--get-file-context ()
  "Get file context information for the current buffer.
Returns a plist with :file-path, :project-cwd, :relative-path,
:absolute-path, and :outside-cwd."
  (let* ((file-path (buffer-file-name))
         (cwd (claudemacs--get-session-cwd))
         (relative-path (file-relative-name file-path cwd))
         (outside-cwd (not (file-in-directory-p file-path cwd))))
    (list :file-path file-path
          :project-cwd cwd
          :relative-path relative-path
          :absolute-path file-path
          :outside-cwd outside-cwd)))

(defun claudemacs--send-return-for-tool (_buffer &optional _tool)
  "Send a return key event through the current terminal backend."
  (claudemacs--terminal-send-key 'return))

(defun claudemacs--send-to-buffer (buffer message &optional no-return tool)
  "Send MESSAGE to BUFFER's terminal.
If NO-RETURN is non-nil, don't send a return/newline."
  (with-current-buffer buffer
    (let* ((resolved-tool (or tool
                              claudemacs--tool
                              (when (string-match "^\\*claudemacs:\\([^:]+\\):" (buffer-name buffer))
                                (intern (match-string 1 (buffer-name buffer))))
                              'claude))
           (plain-message (substring-no-properties message)))
      (if (eq (claudemacs--tool-kind-for-buffer buffer resolved-tool) 'codex)
          (claudemacs--terminal-paste-string plain-message)
        (claudemacs--terminal-send-string plain-message))
      (unless no-return
        ;; Ghostel can otherwise classify adjacent text and Return writes as
        ;; one paste burst.  Delay only this text-plus-submit path; standalone
        ;; Return commands should remain immediate.
        (when (and (eq claudemacs--terminal-backend 'ghostel)
                   (> claudemacs-ghostel-submit-delay 0))
          (sleep-for claudemacs-ghostel-submit-delay))
        (claudemacs--send-return-for-tool buffer resolved-tool)))))

(defun claudemacs--build-prompt (base-prompt)
  "Build a dynamic prompt based on whether C-u was pressed and which tool(s) are active.
BASE-PROMPT is the action description (e.g., 'request', 'question')."
  (if current-prefix-arg
      ;; C-u was pressed - sending to all sessions
      (let* ((sessions (claudemacs--list-sessions-for-workspace))
             (tools (delete-dups (mapcar (lambda (s) (plist-get s :tool)) sessions)))
             (tool-names (mapconcat #'symbol-name tools ", ")))
        (format "Ask all sessions (%s) - %s: " tool-names base-prompt))
    ;; Single session - get the tool name
    (let* ((buffer (claudemacs--get-current-session-buffer))
           (tool (when buffer
                   (buffer-local-value 'claudemacs--tool buffer))))
      (if tool
          (format "Ask %s - %s: " (capitalize (symbol-name tool)) base-prompt)
        (format "Ask Claude - %s: " base-prompt)))))

(defun claudemacs--read-multiline-string (prompt)
  "Read a string from the minibuffer with multi-line support.
PROMPT is the prompt to display.
Shift+Return inserts a newline, Return submits the input.
If the user aborts with C-g, the minibuffer contents are saved
to history so they can be recalled with M-p next time."
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "S-<return>") #'newline)
    (define-key map (kbd "S-RET") #'newline)
    (define-key map (kbd "C-g")
      (lambda ()
        (interactive)
        (let ((text (minibuffer-contents)))
          (when (not (string-empty-p (string-trim text)))
            (add-to-history 'minibuffer-history text)))
        (abort-recursive-edit)))
    (read-from-minibuffer prompt nil map)))

(defun claudemacs--send-message-to-claude (message &optional no-return no-switch)
  "Send MESSAGE to the active Claudemacs session.
If NO-RETURN is non-nil, don't send a return/newline.
If NO-SWITCH is non-nil, don't switch to the Claude buffer."
  (claudemacs--validate-process)
  (let ((claude-buffer (claudemacs--get-current-session-buffer)))
    (claudemacs--send-to-buffer claude-buffer message no-return)
    (unless no-switch
      (display-buffer claude-buffer)
      (select-window (get-buffer-window claude-buffer)))))

(defun claudemacs--active-ediff-buffer-p (&optional buffer)
  "Return non-nil when BUFFER is part of an active Ediff session.
BUFFER defaults to the current buffer.  This recognizes both Ediff control
buffers and the file buffers participating in a live comparison."
  (with-current-buffer (or buffer (current-buffer))
    (or (eq major-mode 'ediff-mode)
        (and (boundp 'ediff-this-buffer-ediff-sessions)
             (seq-some #'buffer-live-p ediff-this-buffer-ediff-sessions)))))

(defun claudemacs--display-session-in-separate-frame (buffer)
  "Show Claudemacs session BUFFER in another frame and select its window.
Reuse a window already displaying BUFFER on any frame; otherwise create one
frame for it."
  (let ((window
         (display-buffer
          buffer
          '((display-buffer-reuse-window display-buffer-pop-up-frame)
            (reusable-frames . t)))))
    (select-frame-set-input-focus (window-frame window))
    (select-window window)
    (with-current-buffer buffer
      (claudemacs--terminal-post-display buffer))))

(defun claudemacs--region-end-line ()
  "Return line number of last line with actual selected content.
If region ends at column 0, returns the previous line since no
content from that line is actually selected."
  (save-excursion
    (goto-char (region-end))
    (if (bolp)
        (1- (line-number-at-pos))
      (line-number-at-pos))))

(defun claudemacs--format-context-line-range (relative-path start-line end-line)
  "Format context for a line range in RELATIVE-PATH from START-LINE to END-LINE."
  (if (= start-line end-line)
      (format "File context: %s:%d\n" relative-path start-line)
    (format "File context: %s:%d-%d\n" relative-path start-line end-line)))

;;;;
;;;; Action Processing System
;;;;

(defun claudemacs--send-action-to-sessions (action-function send-to-all)
  "Call ACTION-FUNCTION to get message, then send to session(s).
ACTION-FUNCTION should return a plist with keys:
  :message - the text to send to Claude
  :no-return - if non-nil, don't send newline
  :no-switch - if non-nil, don't switch to buffer
  :new-frame-from-ediff - if non-nil, honor the Ediff request-frame setting
  :user-message - message to show user after sending

If SEND-TO-ALL is non-nil, send to all active sessions in current workspace.
Otherwise, send to current/active session only."
  (let* ((action-result (funcall action-function))
         (message-text (plist-get action-result :message))
         (no-return (plist-get action-result :no-return))
         (no-switch (plist-get action-result :no-switch))
         (new-frame-from-ediff (plist-get action-result :new-frame-from-ediff))
         (user-message (plist-get action-result :user-message)))

    (if send-to-all
        ;; Send to all sessions in current workspace
        (let ((sessions (claudemacs--list-sessions-for-workspace)))
          (if (null sessions)
              (error "No active Claudemacs sessions found in current workspace")
            (dolist (session-info sessions)
              (let* ((session-buffer (plist-get session-info :buffer))
                     (session-tool (plist-get session-info :tool))
                     ;; Copy to avoid potential destructive mutations by terminal input handlers.
                     (session-message (copy-sequence message-text)))
                (claudemacs--send-to-buffer session-buffer session-message no-return session-tool)
                ;; Give the terminal process a chance to consume each message.
                (sit-for 0.05)))
            (message "%s (sent to %d session%s in current workspace)"
                    user-message
                    (length sessions)
                    (if (= (length sessions) 1) "" "s"))))

      ;; Send to active session only
      (if (and new-frame-from-ediff
               claudemacs-open-new-frame-for-ediff-requests
               (claudemacs--active-ediff-buffer-p))
          (let ((session-buffer (claudemacs--get-current-session-buffer)))
            (claudemacs--send-message-to-claude message-text no-return t)
            (claudemacs--display-session-in-separate-frame session-buffer))
        (claudemacs--send-message-to-claude message-text no-return no-switch))
      (message "%s" user-message))))

(defun claudemacs--fix-error-at-point-action ()
  "Generate fix error action message. Returns plist for action processor."
  (claudemacs--validate-file-and-session)
  (let* ((context (claudemacs--get-file-context))
         (path (if (plist-get context :outside-cwd)
                   (plist-get context :absolute-path)
                 (plist-get context :relative-path)))
         (line-number (line-number-at-pos))
         (errors (claudemacs--get-flycheck-errors-on-line))
         (error-message (claudemacs--format-flycheck-errors errors))
         (message-text (if (string-empty-p error-message)
                          (format "Please fix any issues at %s:%d"
                                  path line-number)
                        (format "Please fix the error at %s:%d, error message: %s"
                                path line-number error-message))))
    (list :message message-text
          :no-return nil
          :no-switch (not claudemacs-switch-to-buffer-on-send-error)
          :user-message (format "Sent error fix request to %s" (claudemacs--get-current-tool-name)))))

;;;###autoload
(defun claudemacs-fix-error-at-point (&optional send-to-all)
  "Send a request to Claude to fix the error at point using flycheck.
With prefix argument (C-u), send to all active sessions."
  (interactive "P")
  (claudemacs--send-action-to-sessions #'claudemacs--fix-error-at-point-action send-to-all))

(defun claudemacs--execute-request-action ()
  "Generate execute request action message. Returns plist for action processor."
  (claudemacs--validate-file-and-session)
  (let* ((context (claudemacs--get-file-context))
         (path (if (plist-get context :outside-cwd)
                   (plist-get context :absolute-path)
                 (plist-get context :relative-path)))
         (has-region (use-region-p))
         (start-line (if has-region
                         (line-number-at-pos (region-beginning))
                       (line-number-at-pos)))
         (end-line (if has-region
                       (claudemacs--region-end-line)
                     (line-number-at-pos)))
         (context-text (claudemacs--format-context-line-range path start-line end-line))
         (request (claudemacs--read-multiline-string (claudemacs--build-prompt "request (with context)")))
         (message-text (concat context-text request)))
    (when (string-empty-p (string-trim request))
      (error "Request cannot be empty"))
    (list :message message-text
          :no-return nil
          :no-switch nil
          :new-frame-from-ediff t
          :user-message (format "Sent request to %s with context" (claudemacs--get-current-tool-name)))))

;;;###autoload
(defun claudemacs-execute-request (&optional send-to-all)
  "Execute a Claude request with file context.
If a region is selected, use it as context with line range.
Otherwise, use current line as context.
With prefix argument (C-u), send to all active sessions."
  (interactive "P")
  (claudemacs--send-action-to-sessions #'claudemacs--execute-request-action send-to-all))

(defun claudemacs--ask-without-context-action ()
  "Generate ask without context action message. Returns plist for action processor."
  (claudemacs--validate-process)
  (let ((request (claudemacs--read-multiline-string (claudemacs--build-prompt "question (without context)"))))
    (when (string-empty-p (string-trim request))
      (error "Request cannot be empty"))
    (list :message request
          :no-return nil
          :no-switch nil
          :new-frame-from-ediff t
          :user-message (format "Sent question to %s" (claudemacs--get-current-tool-name)))))

;;;###autoload
(defun claudemacs-ask-without-context (&optional send-to-all)
  "Ask Claude a question without file or line context.
Prompts for a question and sends it directly to Claude without any
file location or context information.
With prefix argument (C-u), send to all active sessions."
  (interactive "P")
  (claudemacs--send-action-to-sessions #'claudemacs--ask-without-context-action send-to-all))

(defun claudemacs--add-file-reference-action ()
  "Generate add file reference action message. Returns plist for action processor."
  (claudemacs--validate-file-and-session)
  (let* ((context (claudemacs--get-file-context))
         (cwd (plist-get context :project-cwd))
         (selected-file (read-file-name "Add file reference: "))
         (relative-path (file-relative-name selected-file cwd))
         (reference-text (format "@%s " relative-path)))
    (list :message reference-text
          :no-return t
          :no-switch (not claudemacs-switch-to-buffer-on-file-add)
          :user-message (format "Added file reference: @%s" relative-path))))

;;;###autoload
(defun claudemacs-add-file-reference (&optional send-to-all)
  "Add a file reference to the Claude conversation.
Prompts for a file and sends @rel/path/to/file without newline.
With prefix argument (C-u), send to all active sessions."
  (interactive "P")
  (claudemacs--send-action-to-sessions #'claudemacs--add-file-reference-action send-to-all))

(defun claudemacs--add-current-file-reference-action ()
  "Generate add current file reference action message. Returns plist for action processor."
  (claudemacs--validate-file-and-session)
  (let* ((context (claudemacs--get-file-context))
         (path (if (plist-get context :outside-cwd)
                   (plist-get context :absolute-path)
                 (plist-get context :relative-path)))
         (reference-text (format "@%s " path)))
    (list :message reference-text
          :no-return t
          :no-switch (not claudemacs-switch-to-buffer-on-file-add)
          :user-message (format "Added current file reference: @%s" path))))

;;;###autoload
(defun claudemacs-add-current-file-reference (&optional send-to-all)
  "Add current file reference to the Claude conversation.
Sends @rel/path/to/current/file without newline.
With prefix argument (C-u), send to all active sessions."
  (interactive "P")
  (claudemacs--send-action-to-sessions #'claudemacs--add-current-file-reference-action send-to-all))

(defun claudemacs--add-context-action ()
  "Generate add context action message. Returns plist for action processor."
  (claudemacs--validate-file-and-session)
  (let* ((context (claudemacs--get-file-context))
         (path (if (plist-get context :outside-cwd)
                   (plist-get context :absolute-path)
                 (plist-get context :relative-path)))
         (has-region (use-region-p))
         (start-line (if has-region
                         (line-number-at-pos (region-beginning))
                       (line-number-at-pos)))
         (end-line (if has-region
                       (claudemacs--region-end-line)
                     (line-number-at-pos)))
         (context-text (if (and has-region (not (= start-line end-line)))
                           (format "%s:%d-%d " path start-line end-line)
                         (format "%s:%d " path start-line))))
    (list :message context-text
          :no-return t
          :no-switch (not claudemacs-switch-to-buffer-on-add-context)
          :user-message (format "Added context: %s" (string-trim context-text)))))

;;;###autoload
(defun claudemacs-add-context (&optional send-to-all)
  "Add file context with line number(s) to the Claude conversation.
If a region is selected, uses line range (path:start-end).
Otherwise, uses current line (path:line).
Sends without newline so you can continue typing.
With prefix argument (C-u), send to all active sessions."
  (interactive "P")
  (claudemacs--send-action-to-sessions #'claudemacs--add-context-action send-to-all))


(defun claudemacs--implement-comment-action ()
  "Generate implement comment action message. Returns plist for action processor."
  (claudemacs--validate-file-and-session)
  (let* ((context (claudemacs--get-file-context))
         (path (if (plist-get context :outside-cwd)
                   (plist-get context :absolute-path)
                 (plist-get context :relative-path)))
         comment-bounds
         comment-text
         start-line
         end-line)

    (cond
     ;; Case 1: Region is active - use exact region
     ((use-region-p)
      (let ((region-start (region-beginning))
            (region-end (region-end)))
        (setq start-line (line-number-at-pos region-start))
        (setq end-line (claudemacs--region-end-line))
        (setq comment-text (claudemacs--extract-comment-text region-start region-end))))

     ;; Case 2: No region - find comment at point
     (t
      (setq comment-bounds (claudemacs--get-comment-bounds))
      (unless comment-bounds
        (error "Point is not inside a comment"))
      (setq start-line (line-number-at-pos (car comment-bounds)))
      (setq end-line (line-number-at-pos (cdr comment-bounds)))
      (setq comment-text (claudemacs--extract-comment-text
                         (car comment-bounds)
                         (cdr comment-bounds)))))

    ;; Validate we have comment text
    (when (string-empty-p (string-trim comment-text))
      (error "No comment text found to implement"))

    ;; Format the message with file context and implementation request
    (let* ((context-text (claudemacs--format-context-line-range
                         path start-line end-line))
           (message-text (format "%sPlease implement this comment:\n\n%s"
                                context-text comment-text)))
      (list :message message-text
            :no-return nil
            :no-switch nil
            :user-message (format "Sent comment implementation request to %s (%d lines)"
                                 (claudemacs--get-current-tool-name)
                                 (1+ (- end-line start-line)))))))

;;;###autoload
(defun claudemacs-implement-comment (&optional send-to-all)
  "Send comment at point or region to Claude for implementation.
If region is active, uses the exact region.
If no region, finds the comment block at point.
Extracts comment text and sends it to Claude with implementation instructions.
With prefix argument (C-u), send to all active sessions."
  (interactive "P")
  (claudemacs--send-action-to-sessions #'claudemacs--implement-comment-action send-to-all))

;;;###autoload
(defun claudemacs-toggle-buffer ()
  "Toggle Claude buffer visibility.
Hide if current, focus if visible elsewhere, show if hidden.
Works with the most relevant session (current buffer, or most recent)."
  (interactive)
  (claudemacs--validate-process)
  (let ((claude-buffer (claudemacs--get-current-session-buffer)))
    (cond
     ;; Case 1: Current buffer IS the Claude buffer - hide it
     ((eq (current-buffer) claude-buffer)
      (quit-window))

     ;; Case 2: Claude buffer visible in another window
     ((get-buffer-window claude-buffer)
      ;; Quit that window (automatically handles created vs reused)
      ;;
      ;; Edge case: the window was created for Claude, but in the meantime you
      ;; have switched to another workspace and back, the window is no longer
      ;; created just for claudemacs -- it has shown something previous, so it
      ;; will no longer go away if you toggle. Them's the breaks.
      (with-selected-window (get-buffer-window claude-buffer)
        (quit-window)))
     
     ;; Case 3: Claude buffer exists but not visible - show it
     (t
      (display-buffer claude-buffer)
      (when claudemacs-switch-to-buffer-on-toggle
        (select-window (get-buffer-window claude-buffer)))
      (with-current-buffer claude-buffer
        (set-window-point (get-buffer-window claude-buffer) (point-max)))))))

;;;; User Interface

(defun claudemacs--smart-switch-description ()
  "Generate dynamic description for switch menu item.
Shows the tool hint only when exactly one session exists."
  (condition-case err
      (let ((sessions (ignore-errors (claudemacs--list-sessions-for-workspace))))
        (cond
         ;; No sessions
         ((null sessions)
          "Switch to Session (none active)")
         ;; Exactly one session - show tool hint
         ((= (length sessions) 1)
          (let ((tool (ignore-errors (plist-get (car sessions) :tool))))
            (if tool
                (format "Switch to Session (→ %s)"
                        (propertize (symbol-name tool) 'face 'claudemacs-tool-name-face))
              "Switch to Session")))
         ;; Multiple sessions - no hint, will show menu
         (t "Switch to Session...")))
    (error
     (message "Error in smart-switch-description: %S" err)
     "Switch to Session")))

(defun claudemacs--kill-description ()
  "Generate dynamic description for kill menu item.
Shows which session will be killed."
  (condition-case err
      (let ((buffer (ignore-errors (claudemacs--get-current-session-buffer))))
        (if buffer
            (let ((tool (ignore-errors (buffer-local-value 'claudemacs--tool buffer))))
              (if tool
                  (format "Kill Session (→ %s)"
                          (propertize (symbol-name tool) 'face 'claudemacs-tool-name-face))
                "Kill Session"))
          "Kill Session"))
    (error
     (message "Error in kill-description: %S" err)
     "Kill Session")))

(defun claudemacs--smart-resume-description ()
  "Generate dynamic description for smart resume menu item.
Shows which tool will be resumed."
  (condition-case err
      (let ((sessions (ignore-errors (claudemacs--list-sessions-for-workspace))))
        (if (not sessions)
            ;; No sessions, will resume default
            (format "Smart Resume (→ %s)"
                    (propertize (symbol-name claudemacs-default-tool) 'face 'claudemacs-tool-name-face))
          ;; Sessions exist, will resume next available
          (let ((next-tool (ignore-errors (claudemacs--get-next-available-tool))))
            (if next-tool
                (format "Smart Resume (→ %s)"
                        (propertize (symbol-name next-tool) 'face 'claudemacs-tool-name-face))
              "Smart Resume (all running)"))))
    (error
     (message "Error in smart-resume-description: %S" err)
     "Smart Resume")))

(defun claudemacs--format-tool-menu-name (tool instance-name)
  "Format TOOL's INSTANCE-NAME for a menu, prefixed by its label when set.
Returns \"LABEL - INSTANCE-NAME\" when TOOL has a `:label', otherwise just
the instance name."
  (let ((label (claudemacs--get-tool-label tool))
        (name (propertize instance-name 'face 'claudemacs-tool-name-face)))
    (if label
        (concat (propertize label 'face 'claudemacs-tool-name-face) " - " name)
      name)))

(defun claudemacs--get-tool-start-description (tool &optional is-default)
  "Get the description for starting TOOL.
Show the tool label and next instance name, the configured model when enabled,
and a default indicator when IS-DEFAULT is non-nil."
  (let* ((next-instance (claudemacs--get-next-instance-number tool))
         (instance-name (claudemacs--format-tool-instance-name tool next-instance))
         (model-description
          (when claudemacs-show-model-in-menu
            (propertize
             (format "(%s)" (claudemacs--get-tool-model-display tool))
             'face 'font-lock-comment-face)))
         (default-description
          (when is-default
            (propertize "(default)" 'face 'font-lock-comment-face))))
    (concat (claudemacs--format-tool-menu-name tool instance-name)
            (when model-description (concat " " model-description))
            (when default-description (concat " " default-description)))))

(defun claudemacs--get-tool-resume-description (tool &optional is-default)
  "Get the description for resuming TOOL, showing next instance name with (resume).
The tool label is shown before the instance name when one is configured.
If IS-DEFAULT is non-nil, also append a default indicator."
  (let* ((next-instance (claudemacs--get-next-instance-number tool))
         (instance-name (claudemacs--format-tool-instance-name tool next-instance)))
    (format "%s %s%s"
            (claudemacs--format-tool-menu-name tool instance-name)
            (propertize "(resume)" 'face 'font-lock-comment-face)
            (if is-default
                (concat " " (propertize "(default)" 'face 'font-lock-comment-face))
              ""))))

(defun claudemacs--start-tool-by-index (index)
  "Start the tool at INDEX in the tool registry.
INDEX is 0-based."
  (let* ((tools (mapcar #'car claudemacs-tool-registry))
         (tool (nth index tools)))
    (when tool
      (let* ((args (transient-args 'claudemacs-start-menu))
             (prompt-for-dir (member "--prompt-project-root" args))
             (filtered-args (remove "--prompt-project-root" args))
             (expanded-args (claudemacs--expand-transient-args filtered-args))
             (model-type (and claudemacs-show-model-in-menu
                              (claudemacs--model-type-for-tool tool)))
             (model-switches (and model-type
                                  (claudemacs--model-type-switches
                                   tool model-type))))
        (apply #'claudemacs--run-with-args tool prompt-for-dir
               (append expanded-args model-switches))))))

(defun claudemacs--resume-tool-by-index (index)
  "Resume the tool at INDEX using an explicit or selected history ID.
INDEX is 0-based.  An explicit resume command entered through `-f' is passed
directly; otherwise the CLI's interactive picker is never launched because
Claudemacs could not attach its authoritative ID to the new buffer in that
case."
  (let* ((tools (mapcar #'car claudemacs-tool-registry))
         (tool (nth index tools)))
    (when tool
      (let* ((args (transient-args 'claudemacs-resume-menu))
             (prompt-for-dir (member "--prompt-project-root" args))
             (filtered-args (remove "--prompt-project-root" args))
             (expanded-args (claudemacs--expand-transient-args filtered-args))
             (work-dir (if prompt-for-dir
                           (read-directory-name "Project directory: ")
                         (claudemacs--project-root)))
             (explicit-resume-p
              (claudemacs--explicit-resume-command-p tool filtered-args))
             (session-id (unless explicit-resume-p
                           (claudemacs--select-history-session-id tool work-dir)))
             (resume-args (unless explicit-resume-p
                           (claudemacs--get-resume-args tool session-id)))
             (claudemacs-switch-to-buffer-on-create t))  ; Always switch when resuming
        ;; Pass the requested directory through the common launcher.  When the
        ;; ID came from history, recomputing `claudemacs--project-root' here
        ;; could launch the CLI in a different project than the selected ID.
        (apply #'claudemacs--run-with-args tool work-dir
               (append resume-args expanded-args))))))

(defun claudemacs--warn-on-duplicate-tool-keys ()
  "Warn once per menu build when `claudemacs-tool-registry' repeats a key.
Entries are looked up by key, so a repeated key makes every later entry
resolve to the first one's settings.  Distinct keys with the same `:tool'
are the supported way to run one CLI under several profiles."
  (let ((seen '())
        (duplicates '()))
    (dolist (entry claudemacs-tool-registry)
      (let ((tool (car-safe entry)))
        (when tool
          (if (memq tool seen)
              (cl-pushnew tool duplicates)
            (push tool seen)))))
    (when duplicates
      (display-warning
       'claudemacs
       (format (concat "`claudemacs-tool-registry' repeats the key%s %s; "
                       "only the first entry for each is used.  Give each "
                       "entry a distinct key and set `:tool' to the CLI it "
                       "runs.")
               (if (cdr duplicates) "s" "")
               (mapconcat (lambda (tool) (format "`%s'" tool))
                          (nreverse duplicates) ", "))
       :warning))))

(defun claudemacs--setup-start-tool-suffixes (_)
  "Generate tool suffixes dynamically for the start menu.
Returns a list of parsed transient suffix objects."
  (claudemacs--warn-on-duplicate-tool-keys)
  (cl-loop for index from 0
           for (tool . _) in claudemacs-tool-registry
           for key = (number-to-string (1+ index))
           for is-default = (= index 0)
           collect (let ((i index)
                         (desc (claudemacs--get-tool-start-description tool is-default)))
                     (transient-parse-suffix
                      'claudemacs-start-menu
                      (list key desc
                            (lambda () (interactive) (claudemacs--start-tool-by-index i)))))))

(defun claudemacs--setup-resume-tool-suffixes (_)
  "Generate tool suffixes dynamically for the resume menu.
Returns a list of parsed transient suffix objects."
  (claudemacs--warn-on-duplicate-tool-keys)
  (cl-loop for index from 0
           for (tool . _) in claudemacs-tool-registry
           for key = (number-to-string (1+ index))
           for is-default = (= index 0)
           collect (let ((i index)
                         (desc (claudemacs--get-tool-resume-description tool is-default)))
                     (transient-parse-suffix
                      'claudemacs-resume-menu
                      (list key desc
                            (lambda () (interactive) (claudemacs--resume-tool-by-index i)))))))

;;;###autoload (autoload 'claudemacs-start-menu "claudemacs" nil t)
(transient-define-prefix claudemacs-start-menu ()
  "Start a new AI coding session."
  ["Start New Session\n"
   ("-d" "Skip permissions on start" "--dangerous-skip-permissions")
   ("-p" "Prompt for project root" "--prompt-project-root")
   ("-f" "Add custom command-line arguments" "" :class transient-option :prompt "Custom arguments: ")]
  ["Model"
   :if claudemacs--model-type-toggle-visible-p
   ("m" claudemacs-toggle-model-type
    :description claudemacs--get-toggle-model-type-description
    :if claudemacs--model-type-toggle-visible-p
    :transient t)]
  ["Tools"
   :class transient-column
   :setup-children claudemacs--setup-start-tool-suffixes]
  ["" ("<return>" "Start default tool" (lambda () (interactive) (claudemacs--start-tool-by-index 0)))]
  (interactive)
  (claudemacs--reset-model-type-state)
  (add-hook 'transient-post-exit-hook #'claudemacs--reset-model-type-state)
  (transient-setup 'claudemacs-start-menu))

;;;###autoload (autoload 'claudemacs-resume-menu "claudemacs" nil t)
(transient-define-prefix claudemacs-resume-menu ()
  "Resume a previous AI coding session."
  ["Resume Session\n"
   ("-d" "Skip permissions on start" "--dangerous-skip-permissions")
   ("-p" "Prompt for project root" "--prompt-project-root")
   ("-u" "Resume with UUID" "resume " :class transient-option :prompt "Session UUID: ")
   ("-f" "Add custom command-line arguments" "" :class transient-option :prompt "Custom arguments: ")]
  ["Tools"
   :class transient-column
   :setup-children claudemacs--setup-resume-tool-suffixes]
  ["" ("<return>" "Resume default tool" (lambda () (interactive) (claudemacs--resume-tool-by-index 0)))])

;;;###autoload (autoload 'claudemacs-transient-menu "claudemacs" nil t)
(transient-define-prefix claudemacs-transient-menu ()
  "Claude Code AI Pair Programming Interface."
  ["Claudemacs: AI pair programming with Claude Code\n"
   ["Core"
    ("s" "Switch to Session" claudemacs-switch-to-session
     :description claudemacs--smart-switch-description)
    ("S" "Start Session..." claudemacs-start-menu)
    ("o" "Switch Other Session" claudemacs-switch-other)
    ("r" "Resume Session..." claudemacs-resume-menu)
    ("k" "Kill Session..." claudemacs-kill-specific-session)
    ("b" "Branch Current Session" claudemacs-branch-session)
    ("l" "List Live Sessions" claudemacs-session-list)
    ("t" "Toggle Buffer" claudemacs-toggle-buffer)]
   ["Actions (Use C-u to send to all sessions)"
    ("e" "Fix Error at Point" claudemacs-fix-error-at-point)
    ("x" "Execute Request (with context)" claudemacs-execute-request)
    ("X" "Execute Request (no context)" claudemacs-ask-without-context)
    ("i" "Implement Comment" claudemacs-implement-comment)
    ("f" "Add File Reference" claudemacs-add-file-reference)
    ("F" "Add Current File" claudemacs-add-current-file-reference)
    ("a" "Add Context" claudemacs-add-context)]
   ["Quick Responses"
     ("y" "Send Yes (RET)" claudemacs-send-yes)
     ("n" "Send No (ESC)" claudemacs-send-no)]]
  ["Maintenance"
    ("u" "Unstick Claude buffer" claudemacs-unstick-terminal)])

;;;###autoload
(defvar claudemacs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-e") #'claudemacs-transient-menu)
    map)
  "Keymap for `claudemacs-mode'.")

;;;###autoload
(define-minor-mode claudemacs-mode
  "Minor mode for Claude Code AI pair programming.

\\{claudemacs-mode-map}"
  :lighter " Claude"
  :keymap claudemacs-mode-map
  :group 'claudemacs)

(defun claudemacs-unstick-terminal ()
  "Ask the active session's terminal backend to recover its display."
  (interactive)
  (claudemacs--validate-process)
  (when (claudemacs--is-claudemacs-buffer-p)
    (error "Reset buffer cannot be used while visiting the claudemacs buffer itself"))
  (with-current-buffer (claudemacs--get-current-session-buffer)
    (claudemacs--terminal-unstick)))

;;;###autoload
(defun claudemacs--migrated-codex-notification-switches ()
  "Return the OSC9 default when the old BEL default was not customized."
  (when (and (equal claudemacs-codex-notification-switches
                    claudemacs--legacy-codex-notification-switches)
             (null (get 'claudemacs-codex-notification-switches 'saved-value))
             (null (get 'claudemacs-codex-notification-switches
                        'customized-value)))
    (copy-tree claudemacs--default-codex-notification-switches)))

;;;###autoload
(defun claudemacs-setup ()
  "Set up integrations for loaded Claudemacs terminal backends.
This is called automatically when the package is loaded.
Safe to call multiple times."
  (interactive)
  (when-let* ((migrated (claudemacs--migrated-codex-notification-switches)))
    (setq-default claudemacs-codex-notification-switches migrated))
  (claudemacs--terminal-setup-loaded-backends))

(defun claudemacs-unload-function ()
  "Clean up terminal backend integrations."
  (claudemacs--terminal-teardown-loaded-backends)
  nil)

;; Auto-setup when package is loaded
(claudemacs-setup)

(provide 'claudemacs)
;;; claudemacs.el ends here
