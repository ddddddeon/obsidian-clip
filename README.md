# obsidian-clip

Clip code snippets from Emacs into Obsidian

## Installation

Clone the repo and add the path to your init script, and optionally set your vault directory and add a keybinding:
```elisp
(add-to-list 'load-path "~/path/to/obsidian-clip")
(require 'obsidian-clip)

(setq obsidian-clip-directory "~/path/to/vault")
(global-set-key (kbd "C-c C-k") 'obsidian-clip)
```

## Usage

1. Select a region of code
2. Run `M-x obsidian-clip` or use a keybinding if you set one
3. Enter the name of the markdown file (without .md extension)
4. The code will be appended to the file in your Obsidian directory
