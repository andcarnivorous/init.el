(defun night-day-theme ()
    (if (> (nth 2 (decode-time (current-time))) 16)
        (setq catppuccin-flavor 'macchiato)
      (setq catppuccin-flavor 'mocha))
  (catppuccin-reload))

(defun is-big-file ()
  (if (> (count-lines (point-min) (point-max)) 500)
      (minimap-mode 1)
    (minimap-mode -1)))

(defun return-git-foreground ()
  (if (magit-unstaged-files)
      `(:foreground "red")
    `(:foreground "yellow")))


(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell 
      (replace-regexp-in-string "[[:space:]\n]*$" "" 
        (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))


(defun fc/configure-elpy-from-env ()
  (dolist (elt process-environment)
    (when (string-match "\\`\\(ELPY_[^=]*\\)=\\(.*\\)\\'" elt)
      (let ((var (downcase
		  (replace-regexp-in-string "_" "-" (match-string 1 elt))))
	    (val (match-string 2 elt)))
	(set (intern var) (read val))))))


(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, "
          "sed do eiusmod tempor incididunt ut labore et dolore "
          "magnaaliqua. Ut enim ad minim veniam, quis nostrud "
          "exercitation ullamco laboris nisi ut aliquip ex ea commodo "
          "consequat. Duis aute irure dolor in reprehenderit in "
          "voluptate velit esse cillum dolore eu fugiat nulla pariatur. "
          "Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))



(defun endless/export-audio-link (path desc format)
  "Export org audio links to hmtl."
  (cl-case format
    (html (format "<audio src=\"%s\">%s</audio>" path (or desc "")))
    (latex (format "(HOW DO I EXPORT AUDIO TO LATEX? \"%s\")" path))))

(defun transparent ()
  (interactive)
  (if (not (eq (first (frame-parameter (selected-frame) 'alpha)) 75))
      (progn
	(set-frame-parameter (selected-frame) 'alpha '(75 85)))
    (set-frame-parameter (selected-frame) 'alpha '(100 100))))

;; (defun my-tab-related-stuff ()
;;   (setq indent-tabs-mode t)
;;   (setq tab-stop-list (number-sequence 4 200 4))
;;   (setq tab-width 4)
;;   (setq indent-line-function 'insert-tab))

(defun my-go-mode-hook ()
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
;  (if (not (string-match "go" compile-command))
;      (set (make-local-variable 'compile-command)
;           "go build -v && go test -v && go vet"))
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark))

(defun go-run-this-file ()
  "go run"
  (interactive)
  (compile (format "go run %s" (buffer-file-name))))

(defun go-compile ()
  "go compile"
  (interactive)
  (compile "go build -v && go test -v && go vet"))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

;; Add to `doom-modeline-mode-hook` or other hooks
(defun setup-custom-doom-modeline ()
   (doom-modeline-set-modeline 'my-simple-line 'default))
(add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline)

(defun open-eshell ()
  (interactive)
  (if (get-buffer-window "*eshell*")
      (progn
	(if (not (string= (buffer-name) "*eshell*")) (switch-to-buffer-other-window "*eshell*"))
	(kill-buffer "*eshell*")
	(delete-window))
    (let ((w (split-window-below 40)))
      (select-window w)
      (eshell))
    (switch-to-buffer "*eshell*")))

(provide 'functions)
