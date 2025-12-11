# zlua.el - Emacs Integration for z.lua

[z.lua](https://github.com/skywind3000/z.lua) is a fast directory jumping tool (similar to z.sh / autojump / fasd) with better performance and more features. This project provides Emacs integration for z.lua, allowing you to quickly jump to frequently used directories in Emacs.

[中文文档](README-zh.md)

## Features

- 🚀 **Fast Jump**: Quickly jump to frequently used directories using fuzzy matching
- 📊 **Smart Learning**: Automatically learns your habits based on visit frequency and recency
- 🎯 **Interactive Selection**: Interactively select when multiple matches are found
- 📁 **Dired Integration**: Automatically track directories visited in dired
- 🔍 **File Search**: Open files in matching directories
- ⚡ **Caching Support**: Caching for faster file searches
- ⚙️ **Easy Configuration**: Simple configuration options

## Requirements

1. **z.lua Script**: Download z.lua from [skywind3000/z.lua](https://github.com/skywind3000/z.lua)
2. **Lua Interpreter**: lua, luajit, or lua 5.1/5.2/5.3 must be installed
3. **Emacs**: Version 24.4 or higher

## Installation

### Manual Installation

1. Download the z.lua script:

```bash
# Clone the z.lua repository
git clone https://github.com/skywind3000/z.lua.git ~/z.lua
```

2. Download zlua.el to your Emacs load-path:

```bash
# Clone this repository
git clone https://github.com/Kinneyzhang/zlua.git ~/.emacs.d/site-lisp/zlua
```

3. Add to your Emacs configuration:

```elisp
(add-to-list 'load-path "~/.emacs.d/site-lisp/zlua")
(require 'zlua)

;; Set the path to z.lua script
(setq zlua-script (expand-file-name "~/z.lua/z.lua"))

;; Optional: Set the lua executable path (if not in PATH)
;; (setq zlua-executable "/usr/bin/lua")

;; Enable auto-tracking mode
(zlua-mode 1)
```

### Using use-package

```elisp
(use-package zlua
  :load-path "~/.emacs.d/site-lisp/zlua"
  :custom
  (zlua-script (expand-file-name "~/z.lua/z.lua"))
  :config
  (zlua-mode 1))
```

### Using Straight.el

```elisp
(use-package zlua
  :straight (:host github :repo "Kinneyzhang/zlua")
  :custom
  (zlua-script (expand-file-name "~/z.lua/z.lua"))
  :config
  (zlua-mode 1))
```

## Usage

### Basic Commands

#### `zlua-jump` (alias: `zlua`, `z`)

Jump to a matching directory:

```elisp
M-x zlua-jump RET foo RET          ; Jump to the most frequently used directory matching "foo"
M-x z RET foo bar RET              ; Jump to directory matching both "foo" and "bar"
```

#### `zlua-jump-interactive`

Jump with interactive selection:

```elisp
M-x zlua-jump-interactive RET foo RET  ; Show all matches and select
C-u M-x zlua-jump RET foo RET          ; Same effect with prefix argument
```

#### `zlua-list`

List all matching directories with their scores:

```elisp
M-x zlua-list RET foo RET          ; Display matches in a new buffer
```

#### `zlua-find-file`

Find and open a file in a matching directory:

```elisp
M-x zlua-find-file RET foo RET     ; Jump to directory matching "foo" and open file selection
```

#### `zlua-find-file-by-name`

Search and open files by name across all tracked directories:

```elisp
M-x zlua-find-file-by-name RET readme RET    ; Find all files containing 'readme'
M-x zlua-find-file-by-name RET .txt RET      ; Find all .txt files
```

If multiple matches are found, an interactive selection list will be displayed (showing filename and directory).

#### `zlua-clear-cache`

Clear the directory cache to force refresh of the tracked directories list:

```elisp
M-x zlua-clear-cache RET           ; Clear cache
```

### Suggested Keybindings

```elisp
(global-set-key (kbd "C-c z") 'zlua-jump)
(global-set-key (kbd "C-c Z") 'zlua-jump-interactive)
(global-set-key (kbd "C-c f z") 'zlua-find-file)
(global-set-key (kbd "C-c f n") 'zlua-find-file-by-name)
```

## Configuration Options

### `zlua-script`

Absolute path to z.lua script (required).

```elisp
(setq zlua-script (expand-file-name "~/z.lua/z.lua"))
```

### `zlua-executable`

Path to Lua executable. If nil, will automatically search in PATH.

```elisp
(setq zlua-executable "/usr/local/bin/lua")
```

### `zlua-enable-auto-track`

Whether to automatically track directory visits in dired-mode. Default is `t`.

```elisp
(setq zlua-enable-auto-track t)  ; Enable auto-tracking
```

### `zlua-cache-timeout`

Directory cache expiration time in seconds. Default is 60 seconds. Set to 0 to disable caching.

```elisp
(setq zlua-cache-timeout 60)     ; Cache for 60 seconds
(setq zlua-cache-timeout 0)      ; Disable caching
```

## How It Works

1. **Directory Tracking**: When `zlua-mode` is enabled, each directory visited in dired will be added to the z.lua database.

2. **Smart Matching**: z.lua uses a "frecent" algorithm (combining frequency and recency) to sort matching directories.

3. **Fuzzy Search**: Supports regex and multi-keyword matching, e.g., "foo bar" can match `/foo/something/bar`.

4. **Caching Mechanism**: For better performance, `zlua-search-file` uses caching to store the tracked directories list, avoiding z.lua queries on each call.

## Shell Integration

If you also use z.lua in your shell, the Emacs integration shares the same database (default `~/.zlua`), meaning:

- Directories visited in shell are also available in Emacs
- Directories visited in Emacs are also available in shell
- History from both enhances each other

Installing z.lua in bash/zsh:

```bash
# Add to .bashrc or .zshrc
eval "$(lua ~/z.lua/z.lua --init bash)"   # For bash
eval "$(lua ~/z.lua/z.lua --init zsh)"    # For zsh
```

## Troubleshooting

### "lua executable not found"

Ensure lua is installed and in PATH, or set `zlua-executable`:

```bash
# Check if lua is available
which lua
```

### "z.lua script not found"

Ensure `zlua-script` points to the correct z.lua script path:

```elisp
(setq zlua-script (expand-file-name "~/z.lua/z.lua"))
```

### No matching results

z.lua needs time to learn your habits. After using it for a while, your visited directories will be recorded and available for jumping.

## Example Workflow

```elisp
;; 1. After enabling zlua-mode, use dired normally to browse directories
M-x dired RET ~/projects/my-project RET
M-x dired RET ~/documents/work RET
M-x dired RET ~/downloads RET

;; 2. Later you can quickly jump to these directories
M-x z RET proj RET                    ; Jump to ~/projects/my-project
M-x z RET work RET                    ; Jump to ~/documents/work
M-x z RET down RET                    ; Jump to ~/downloads

;; 3. Use interactive selection when multiple matches exist
M-x zlua-jump-interactive RET doc RET ; Show all directories containing "doc"

;; 4. Open files in matching directories
M-x zlua-find-file RET proj RET       ; Select file in ~/projects/my-project

;; 5. Search and open files by name directly
M-x zlua-find-file-by-name RET config RET  ; Find files containing "config" in all tracked directories
M-x zlua-find-file-by-name RET .el RET     ; Find all .el files
```

## Related Projects

- [z.lua](https://github.com/skywind3000/z.lua) - Original z.lua project
- [z.sh](https://github.com/rupa/z) - Original z shell script
- [autojump](https://github.com/wting/autojump) - Another directory jumping tool
- [fasd](https://github.com/clvv/fasd) - Quick access to files and directories

## License

MIT License - See LICENSE file for details.

## Acknowledgments

Thanks to [skywind3000](https://github.com/skywind3000) for creating the excellent z.lua tool.