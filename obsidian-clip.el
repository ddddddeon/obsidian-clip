;;; obsidian-clip.el --- Quickly clip code snippets to Obsidian notes  -*- lexical-binding: t; -*-

;; Author: ddddddeon
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))
;; Keywords: obsidian, productivity, notes
;; URL: https://github.com/ddddddeon/obsidian-clip

;;; Commentary:
;; Clip code snippets from Emacs into Obsidian markdown files


;;; Code:

(defgroup obsidian-clip nil
  "Quickly clip code snippets to Obsidian notes."
  :group 'tools
  :prefix "obsidian-clip-")

(defcustom obsidian-clip-directory "~/obsidian"
  "Directory where Obsidian vault is located."
  :type 'directory
  :group 'obsidian-clip)

(defcustom obsidian-clip-default-file "Inbox"
  "Default file name to use for clipping."
  :type 'string
  :group 'obsidian-clip)

(defvar obsidian-clip--most-recent-file obsidian-clip-default-file
  "Most recently used file for clipping.")

(defun obsidian-clip ()
  "Clip selected region to an Obsidian markdown file.
If region is selected, prompts for a file name and writes the region
as a code block to the specified markdown file in the Obsidian directory."
  (interactive)
  (if (use-region-p)
      (let* ((content (buffer-substring (region-beginning) (region-end)))
             (ext (file-name-extension (buffer-file-name)))
             (md-file-name (read-from-minibuffer "filename: " obsidian-clip--most-recent-file))
             (md-file (expand-file-name (concat md-file-name ".md") obsidian-clip-directory)))
        (with-temp-buffer
          (insert "\n```" ext "\n" content "```\n\n")
          (write-region (point-min) (point-max) md-file t))
        (setq obsidian-clip--most-recent-file md-file-name)
        (find-file md-file))
    (message "No region selected")))

(provide 'obsidian-clip)

;;; obsidian-clip.el ends here
