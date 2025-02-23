# obsidian-clip

Clip code snippets from Emacs into Obsidian

## Installation

Clone the repo and move the file somewhere to which your emacs environment has access:
```
git clone https://github.com/ddddddeon/obsidian-clip
cp obsidian-clip/obsidian-clip.el /path/to/
```

Load the file in your init script, and optionally set your vault directory and add a keybinding:
```elisp
(load "/path/to/obsidian-clip.el")
(setq obsidian-clip-directory "~/path/to/vault")
(global-set-key (kbd "C-c C-k") 'obsidian-clip)
```

## Usage

1. Select a region of code
2. Run `M-x obsidian-clip` or use a keybinding if you set one
3. Enter the name of the markdown file (without .md extension)
4. The code will be appended to the file in your Obsidian directory
