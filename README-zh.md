# zlua.el - Emacs 中集成 z.lua

[z.lua](https://github.com/skywind3000/z.lua) 是一个快速路径切换工具，类似于 z.sh / autojump / fasd，但性能更好、功能更强。本项目为 Emacs 提供 z.lua 集成，让你可以在 Emacs 中快速跳转到常用目录。

[English Documentation](README.md)

## 功能特性

- 🚀 **快速跳转**: 使用模糊匹配快速跳转到常用目录
- 📊 **智能学习**: 基于访问频率和时间自动学习你的习惯
- 🎯 **交互式选择**: 多个匹配结果时可以交互式选择
- 📁 **Dired 集成**: 自动跟踪 dired 中访问的目录
- 🔍 **查找文件**: 在匹配的目录中打开文件
- ⚡ **缓存支持**: 使用缓存加速文件搜索
- ⚙️ **易于配置**: 简单的配置选项

## 依赖

1. **z.lua 脚本**: 从 [skywind3000/z.lua](https://github.com/skywind3000/z.lua) 下载 z.lua 脚本
2. **Lua 解释器**: 需要安装 lua, luajit, 或 lua 5.1/5.2/5.3
3. **Emacs**: 版本 24.4 或更高

## 安装

### 手动安装

1. 下载 z.lua 脚本：

```bash
# 克隆 z.lua 仓库
git clone https://github.com/skywind3000/z.lua.git ~/z.lua
```

2. 下载 zlua.el 到你的 Emacs load-path：

```bash
# 克隆本仓库
git clone https://github.com/Kinneyzhang/zlua.git ~/.emacs.d/site-lisp/zlua
```

3. 在你的 Emacs 配置文件中添加：

```elisp
(add-to-list 'load-path "~/.emacs.d/site-lisp/zlua")
(require 'zlua)

;; 设置 z.lua 脚本的路径
(setq zlua-script (expand-file-name "~/z.lua/z.lua"))

;; 可选：设置 lua 可执行文件的路径（如果不在 PATH 中）
;; (setq zlua-executable "/usr/bin/lua")

;; 启用自动跟踪模式
(zlua-mode 1)
```

### 使用 use-package

```elisp
(use-package zlua
  :load-path "~/.emacs.d/site-lisp/zlua"
  :custom
  (zlua-script (expand-file-name "~/z.lua/z.lua"))
  :config
  (zlua-mode 1))
```

### 使用 Straight.el

```elisp
(use-package zlua
  :straight (:host github :repo "Kinneyzhang/zlua")
  :custom
  (zlua-script (expand-file-name "~/z.lua/z.lua"))
  :config
  (zlua-mode 1))
```

## 使用方法

### 基本命令

#### `zlua-jump` (别名: `zlua`, `z`)

跳转到匹配的目录：

```elisp
M-x zlua-jump RET foo RET          ; 跳转到匹配 foo 的最常用目录
M-x z RET foo bar RET              ; 跳转到同时匹配 foo 和 bar 的目录
```

#### `zlua-jump-interactive`

使用交互式选择跳转：

```elisp
M-x zlua-jump-interactive RET foo RET  ; 显示所有匹配项并选择
C-u M-x zlua-jump RET foo RET          ; 与 prefix argument 效果相同
```

#### `zlua-list`

列出所有匹配的目录及其分数：

```elisp
M-x zlua-list RET foo RET          ; 在新 buffer 中显示匹配结果
```

#### `zlua-search-dir`

在匹配的目录中查找并打开文件：

```elisp
M-x zlua-search-dir RET foo RET    ; 跳转到匹配 foo 的目录并打开文件选择
```

#### `zlua-search-file`

根据文件名在所有跟踪的目录中搜索并打开文件：

```elisp
M-x zlua-search-file RET readme RET    ; 查找所有包含 'readme' 的文件
M-x zlua-search-file RET .txt RET      ; 查找所有 .txt 文件
```

如果找到多个匹配的文件，会显示选择列表（包含文件名和所在目录）。

#### `zlua-clear-cache`

清除目录缓存，强制刷新跟踪的目录列表：

```elisp
M-x zlua-clear-cache RET           ; 清除缓存
```

### 键绑定建议

```elisp
(global-set-key (kbd "C-c z") 'zlua-jump)
(global-set-key (kbd "C-c Z") 'zlua-jump-interactive)
(global-set-key (kbd "C-c f z") 'zlua-search-dir)
(global-set-key (kbd "C-c f n") 'zlua-search-file)
```

## 配置选项

### `zlua-script`

z.lua 脚本的绝对路径（必须设置）。

```elisp
(setq zlua-script (expand-file-name "~/z.lua/z.lua"))
```

### `zlua-executable`

Lua 可执行文件的路径。如果为 nil，会自动在 PATH 中查找。

```elisp
(setq zlua-executable "/usr/local/bin/lua")
```

### `zlua-enable-auto-track`

是否在 dired-mode 中自动跟踪目录访问。默认为 `t`。

```elisp
(setq zlua-enable-auto-track t)  ; 启用自动跟踪
```

### `zlua-cache-timeout`

目录缓存过期时间（秒）。默认为 100000 秒。设置为 0 可禁用缓存。

```elisp
(setq zlua-cache-timeout 100000) ; 缓存 100000 秒（默认）
(setq zlua-cache-timeout 60)     ; 缓存 60 秒
(setq zlua-cache-timeout 0)      ; 禁用缓存
```

## 工作原理

1. **目录跟踪**: 当 `zlua-mode` 启用时，每次在 dired 中访问目录时，该目录会被添加到 z.lua 数据库中。

2. **智能匹配**: z.lua 使用 "frecent" 算法（结合频率和最近访问时间）来排序匹配的目录。

3. **模糊搜索**: 支持正则表达式和多关键词匹配，例如 "foo bar" 可以匹配 `/foo/something/bar`。

4. **缓存机制**: 为了提高性能，`zlua-search-file` 使用缓存来存储跟踪的目录列表，避免每次调用都查询 z.lua。

## 与 Shell 集成

如果你也在 shell 中使用 z.lua，Emacs 集成会与 shell 共享同一个数据库（默认为 `~/.zlua`），这意味着：

- 在 shell 中访问的目录也会在 Emacs 中可用
- 在 Emacs 中访问的目录也会在 shell 中可用
- 两者的历史记录会互相增强

在 bash/zsh 中安装 z.lua：

```bash
# 在 .bashrc 或 .zshrc 中添加
eval "$(lua ~/z.lua/z.lua --init bash)"   # 对于 bash
eval "$(lua ~/z.lua/z.lua --init zsh)"    # 对于 zsh
```

## 故障排除

### "lua executable not found"

确保 lua 已安装并在 PATH 中，或者设置 `zlua-executable`：

```bash
# 检查 lua 是否可用
which lua
```

### "z.lua script not found"

确保 `zlua-script` 指向正确的 z.lua 脚本路径：

```elisp
(setq zlua-script (expand-file-name "~/z.lua/z.lua"))
```

### 没有匹配结果

z.lua 需要一段时间来学习你的习惯。在使用一段时间后，你访问过的目录会被记录并可以跳转。

## 示例工作流

```elisp
;; 1. 启用 zlua-mode 后，正常使用 dired 浏览目录
M-x dired RET ~/projects/my-project RET
M-x dired RET ~/documents/work RET
M-x dired RET ~/downloads RET

;; 2. 之后可以快速跳转到这些目录
M-x z RET proj RET                    ; 跳转到 ~/projects/my-project
M-x z RET work RET                    ; 跳转到 ~/documents/work
M-x z RET down RET                    ; 跳转到 ~/downloads

;; 3. 多个匹配时使用交互式选择
M-x zlua-jump-interactive RET doc RET ; 显示所有包含 "doc" 的目录

;; 4. 在匹配的目录中打开文件
M-x zlua-search-dir RET proj RET      ; 在 ~/projects/my-project 中选择文件

;; 5. 根据文件名直接搜索并打开文件
M-x zlua-search-file RET config RET   ; 在所有跟踪目录中查找包含 "config" 的文件
M-x zlua-search-file RET .el RET      ; 查找所有 .el 文件
```

## 相关项目

- [z.lua](https://github.com/skywind3000/z.lua) - 原始的 z.lua 项目
- [z.sh](https://github.com/rupa/z) - 原始的 z shell 脚本
- [autojump](https://github.com/wting/autojump) - 另一个目录跳转工具
- [fasd](https://github.com/clvv/fasd) - 快速访问文件和目录

## 许可证

MIT License - 详见 LICENSE 文件。

## 致谢

感谢 [skywind3000](https://github.com/skywind3000) 创建了优秀的 z.lua 工具。
