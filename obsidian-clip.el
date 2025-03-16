;;; Obsidian-clip.el --- Clip code snippets to Obsidian notes  -*- lexical-binding: t; -*-

;; Author: Chris d'Eon 
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))
;; Keywords: obsidian, productivity, notes
;; URL: https://github.com/ddddddeon/obsidian-clip

;;; Commentary:
;; Clip code snippets from Emacs into Obsidian markdown files

(defgroup obsidian-clip nil
  "Quickly clip code snippets to Obsidian notes"
  :group 'tools
  :prefix "obsidian-clip-")

(defcustom obsidian-clip-directory "~/obsidian"
  "Directory where Obsidian vault is located"
  :type 'directory
  :group 'obsidian-clip)

(defcustom obsidian-clip-default-file "Inbox"
  "Default file name to use for clipping"
  :type 'string
  :group 'obsidian-clip)

(defcustom obsidian-clip-tasks-file "Tasks.md"
  "Default file name to insert tasks"
  :type 'string
  :group 'obsidian-clip)

(defvar obsidian-clip-most-recent-file obsidian-clip-default-file
  "Most recently used file for clipping")

;;;###autoload
(defun obsidian-clip (md-file-name)
  "Clip selected region to an Obsidian markdown file.
Defaults first to default-file, then to most recent file to which a clip was sent." 
  (interactive "s> ")
  (if (use-region-p)
      (let* ((content (buffer-substring (region-beginning) (region-end)))
             (ext (file-name-extension (buffer-file-name)))
             (md-file-name (if (string= md-file-name "")
                              obsidian-clip-most-recent-file
                            md-file-name))
             (md-file (expand-file-name (concat md-file-name ".md") obsidian-clip-directory)))
        (with-temp-buffer
          (insert "\n```" ext "\n" content "\n```\n\n")
          (write-region (point-min) (point-max) md-file t))
        (setq obsidian-clip-most-recent-file md-file-name)
        (find-file md-file))
    (message "No region selected")))

;;;###autoload
(defun obsidian-clip-new-task (task)
  (interactive "s> ")
  (let* ((tasks-file (concat obsidian-clip-directory "/" obsidian-clip-tasks-file)))
    (with-temp-buffer
      (when (file-exists-p tasks-file)
        (insert-file-contents tasks-file))
      (goto-char (point-max))
      (insert "\n- [ ] " task)
      (write-region (point-min) (point-max) tasks-file))
    (find-file-other-window tasks-file)
    (goto-char (point-max))))

(provide 'obsidian-clip)

;;; obsidian-clip.el ends here
