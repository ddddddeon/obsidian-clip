# obsidian-clip

Clip code snippets from Emacs into Obsidian

## Installation

Clone the repo, add the path to your init script, and optionally set your vault directory and tasks file, and add keybindings:
```elisp
(add-to-list 'load-path "~/path/to/obsidian-clip")
(require 'obsidian-clip)

(setq obsidian-clip-directory "~/path/to/vault")
(setq obsidian-clip-tasks-file "Tasks.md")
(global-set-key (kbd "C-c C-k") 'obsidian-clip)
(global-set-key (kbd "C-C C-t") 'obsidian-clip-new-task)
```

## Usage

### Clip
1. Select a region of code
2. Run `M-x obsidian-clip` or use a keybinding if you set one
3. Enter the name of the markdown file (without .md extension)
4. The code will be appended to the file in your Obsidian directory

### New Task
1. Run `M-x obsidian-clip-new-task` and enter a task name/description. 
2. Obsidian-clip will append the task to a tasks file defined by `obsidian-clip-tasks-file` inside the `obsidian-clip-directory`.
