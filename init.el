(set-scroll-bar-mode 'right)
(scroll-bar-mode 0)
(recentf-mode 1)
(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(menu-bar-mode -1)
(display-time-mode 1)
(setq auto-save-file-name-transforms
  `((".*" "~/.emacs-saves/" t)))
(setq backup-directory-alist '(("." . "~/.emacs-backups")))
(defvar backup-dir (expand-file-name "~/.emacs-backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs-saves/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq pixel-scroll-mode t)
(setq pixel-scroll-precision-large-scroll-height 40.0)
(setq warning-minimum-level :error)
(setq display-line-numbers-type 'relative)
(setq truncate-partial-width-windows nil)
(setq-default truncate-lines t)
(require 'package)

(defun my-select-line ()
  (set-mark (line-beginning-position))
  (goto-char (line-end-position)))

(defun select-line ()
  (interactive)
  (my-select-line)
  (activate-mark))

(defun kill-line-from-any-col ()
  (interactive)
  (if (not (region-active-p))
      (my-select-line)
    (kill-region (line-beginning-position) (line-end-position))
    (delete-char 1)
    (activate-mark)))

(defun copy-line ()
  (interactive)
    (if (not (region-active-p))
      (my-select-line))
  (kill-ring-save (region-beginning) (region-end)))

(defun copy-paste-line ()
  (interactive)
  (copy-line)
  (goto-char (line-end-position))
  (newline)
  (goto-char (line-beginning-position))
  (yank))


(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-refresh-contents)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (setq mac-command-modifier 'control)
  (add-to-list 'default-frame-alist '(font . "Menlo 13")))

(when (memq window-system '(x))
  (exec-path-from-shell-initialize))

(add-to-list 'load-path "~/.emacs.d/functions/")
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; left, top, right, bottom
(setq window-sides-slots '(1 0 1 0))

(defun set-window-width-percentage (percentage)
  "Set the Emacs window width to a given PERCENTAGE of the screen width."
  (let* ((s-w (display-pixel-width))
         (c-w (frame-char-width)))
    (round (* s-w percentage 0.01) c-w)))
(setq fixed-w-width (set-window-width-percentage 25))
(add-to-list 'display-buffer-alist
          `(,(rx (| "*compilation*" "*grep*" "*Embark Export" "*Occur"))
            display-buffer-in-side-window
            (side . right)
            (slot . 0)
            (window-parameters . ((no-delete-other-windows . t)))
            (window-width . fixed-w-width)))

;(pdf-tools-install)

;(when (equal system-type 'gnu/linux) (set-exec-path-from-shell-PATH))

(electric-pair-mode 1)

(setq show-paren-delay 0
        show-paren-style 'parenthesis)
(show-paren-mode 1)

(defalias 'workon 'pyvenv-workon)

(setq electric-pair-pairs '(
                            (?\( . ?\))
                            (?\[ . ?\])
                            (?\{ . ?\})
                           ))
(electric-pair-mode t)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-dired-monochrome nil)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#3c3836" "#fb4933" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(awesome-tray-mode-line-active-color "#2fafff")
 '(awesome-tray-mode-line-inactive-color "#323232")
 '(custom-enabled-themes '(smart-mode-line-atom-one-dark))
 '(custom-safe-themes
   '("d80952c58cf1b06d936b1392c38230b74ae1a2a6729594770762dc0779ac66b7" "4b287bfbd581ea819e5d7abe139db1fb5ba71ab945cec438c48722bea3ed6689" "3199be8536de4a8300eaf9ce6d864a35aa802088c0925e944e2b74a574c68fd0" "7dc296b80df1b29bfc4062d1a66ee91efb462d6a7a934955e94e786394d80b71" "dad40020beea412623b04507a4c185079bff4dcea20a93d8f8451acb6afc8358" "a0415d8fc6aeec455376f0cbcc1bee5f8c408295d1c2b9a1336db6947b89dd98" "795d2a48b56beaa6a811bcf6aad9551878324f81f66cac964f699871491710fa" "e27c391095dcee30face81de5c8354afb2fbe69143e1129109a16d17871fc055" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "ed68393e901a88b9feefea1abfa9a9c5983e166e4378c71bb92e636423bd94fd" "7661b762556018a44a29477b84757994d8386d6edee909409fabe0631952dad9" "6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" "83e0376b5df8d6a3fbdfffb9fb0e8cf41a11799d9471293a810deb7586c131e6" "6bdcff29f32f85a2d99f48377d6bfa362768e86189656f63adbf715ac5c1340b" "78c4238956c3000f977300c8a079a3a8a8d4d9fee2e68bad91123b58a4aa8588" "4eb6fa2ee436e943b168a0cd8eab11afc0752aebb5d974bba2b2ddc8910fca8f" "e29a6c66d4c383dbda21f48effe83a1c2a1058a17ac506d60889aba36685ed94" "979525ee3cdbe0d4ceab63147ec710be4cbdac0e66aae9d280c05bcbff89b15d" "171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" "fef1ae76cbc3d5fe957160406cf034e5a352037eb5a7ace339fcddc26ada5f9f" "669e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" "e1d09f1b2afc2fed6feb1d672be5ec6ae61f84e058cb757689edb669be926896" "a06658a45f043cd95549d6845454ad1c1d6e24a99271676ae56157619952394a" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "939ea070fb0141cd035608b2baabc4bd50d8ecc86af8528df9d41f4d83664c6a" "4cf9ed30ea575fb0ca3cff6ef34b1b87192965245776afa9e9e20c17d115f3fb" "855eb24c0ea67e3b64d5d07730b96908bac6f4cd1e5a5986493cbac45e9d9636" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "aded61687237d1dff6325edb492bde536f40b048eab7246c61d5c6643c696b7f" "a622aaf6377fe1cd14e4298497b7b2cae2efc9e0ce362dade3a58c16c89e089c" "595617a3c537447aa7e76ce05c8d43146a995296ea083211225e7efc069c598f" "fd3c7bd752f48dcb7efa5f852ef858c425b1c397b73851ff8816c0580eab92f1" "3da031b25828b115c6b50bb92a117f5c0bbd3d9d0e9ba5af3cd2cb9db80db1c2" "ea6bf6e091026dc219c142b40afb8470d581a8ec70c5114fde919a3221ef91f5" "e2fd81495089dc09d14a88f29dfdff7645f213e2c03650ac2dd275de52a513de" default))
 '(ediff-diff-options "-w")
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(exwm-floating-border-color "#646464")
 '(fci-rule-color "#3E4451")
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-themes-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-themes-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-themes-fringe-yellow))
 '(gnus-select-method '(nnreddit ""))
 '(helm-completion-style 'vertico)
 '(hl-todo-keyword-faces
   '(("HOLD" . "#c0c530")
     ("TODO" . "#feacd0")
     ("NEXT" . "#b6a0ff")
     ("THEM" . "#f78fe7")
     ("PROG" . "#00d3d0")
     ("OKAY" . "#4ae2f0")
     ("DONT" . "#70b900")
     ("FAIL" . "#ff8059")
     ("BUG" . "#ff8059")
     ("DONE" . "#44bc44")
     ("NOTE" . "#d3b55f")
     ("KLUDGE" . "#d0bc00")
     ("HACK" . "#d0bc00")
     ("TEMP" . "#ffcccc")
     ("FIXME" . "#ff9077")
     ("XXX+" . "#ef8b50")
     ("REVIEW" . "#6ae4b9")
     ("DEPRECATED" . "#bfd9ff")))
 '(ibuffer-deletion-face 'modus-themes-mark-del)
 '(ibuffer-filter-group-name-face 'modus-themes-pseudo-header)
 '(ibuffer-marked-face 'modus-themes-mark-sel)
 '(ibuffer-title-face 'default)
 '(ignored-local-variable-values
   '((diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")
     (eval when
           (and
            (buffer-file-name)
            (not
             (file-directory-p
              (buffer-file-name)))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (unless
               (require 'package-recipe-mode nil t)
             (let
                 ((load-path
                   (cons "../package-build" load-path)))
               (require 'package-recipe-mode)))
           (unless
               (derived-mode-p 'emacs-lisp-mode)
             (emacs-lisp-mode))
           (package-build-minor-mode)
           (setq-local flycheck-checkers nil)
           (set
            (make-local-variable 'package-build-working-dir)
            (expand-file-name "../working/"))
           (set
            (make-local-variable 'package-build-archive-dir)
            (expand-file-name "../packages/"))
           (set
            (make-local-variable 'package-build-recipes-dir)
            default-directory))))
 '(linguistic-stopwords
   '("what" "am" "during" "him" "we" "me" "under" "the" "i" "myself" "again" "be" "you" "yourself" "once" "that" "my" "yourselves" "here" "your" "d" "there" "any" "it" "ll" "when" "of" "he" "yours" "where" "did" "she" "himself" "all" "so" "her" "herself" "same" "any" "our" "this" "own" "some" "they" "those" "could" "but" "their" "these" "can" "for" "on" "which" "won" "with" "in" "who" "will" "not" "at" "when" "just" "no" "or" "been" "now" "yes" "and" "is" "ve" "being" "because" "are" "hadn" "as" "an" "have" "needn" "from" "a" "has" "need" "by" "to" "had" "haven" "”" "his" "was" "hasn" "—" "wasn" "don" "t" "“" "do" "would" "isn" "does" "should" "were" "very" "was" "\"" "much" "many"))
 '(lsp-terraform-server "terraform-lsp")
 '(neo-autorefresh t)
 '(org-export-backends '(ascii beamer html icalendar latex))
 '(org-src-block-faces 'nil)
 '(package-selected-packages
   '(org-babel-eval-in-repl windresize nyan-mode command-log-mode company-quickhelp-terminal vertico-posframe-mode vertico-posframe project-tab-groups-mode nnreddit company wgrep blamer pyconf ace-window biblio-core yasnippet-classic-snippets yasnippet-snippets vertico-buffer consult-org-roam consult-project-extra consult-eglot sketch-themes with-venv gnuplot orderless ox-twbs auctex dirvish pyenv-mode ac-html ghub use-package ac-ispell package-build marginalia csv project-tab-groups magit vertico helm corfu-doc neotree linguistic smart-mode-line-atom-one-dark-theme org-ref dap-mode embark-consult atom-one-dark-theme which-key eshell-toggle org-bullets emojify json-mode move-text exec-path-from-shell elpy counsel docker lua-mode python-isort company-box web-mode gruvbox-theme lsp-pyright olivetti docker-compose-mode consult-lsp org-ac git-gutter go-mode mark-multiple python-black hungry-delete doom-modeline go-autocomplete ess flycheck-pycheckers dired-subtree org-pdftools rainbow-delimiters lsp-ui skewer-mode dockerfile-mode all-the-icons gnuplot-mode))
 '(pdf-view-midnight-colors '("#fdf4c1" . "#1d2021"))
 '(python-flymake-command '("flake8"))
 '(python-shell-interpreter "python3")
 '(safe-local-variable-values
   '((eval do--switch-pyvenv "test_lsp_bridge")
     (eval do--switch-pyvenv "testbootstrap")))
 '(tetris-x-colors
   [[229 192 123]
    [97 175 239]
    [209 154 102]
    [224 108 117]
    [152 195 121]
    [198 120 221]
    [86 182 194]])
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#ff8059")
     (40 . "#feacd0")
     (60 . "#f78fe7")
     (80 . "#ef8b50")
     (100 . "#d0bc00")
     (120 . "#c0c530")
     (140 . "#f8dec0")
     (160 . "#bfebe0")
     (180 . "#44bc44")
     (200 . "#70b900")
     (220 . "#6ae4b9")
     (240 . "#4ae2f0")
     (260 . "#00d3d0")
     (280 . "#c6eaff")
     (300 . "#2fafff")
     (320 . "#79a8ff")
     (340 . "#00bcff")
     (360 . "#b6a0ff")))
 '(vc-annotate-very-old-color nil)
 '(warning-suppress-types '((comp)))
 '(widget-link-prefix "[")
 '(widget-link-suffix "]")
 '(widget-mouse-face '(highlight widget-button))
 '(widget-push-button-prefix "[")
 '(widget-push-button-suffix "]")
 '(xterm-color-names
   ["black" "#ff8059" "#44bc44" "#d0bc00" "#2fafff" "#feacd0" "#00d3d0" "gray65"])
 '(xterm-color-names-bright
   ["gray35" "#ef8b50" "#70b900" "#c0c530" "#79a8ff" "#f78fe7" "#4ae2f0" "white"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(calendar-today ((t (:background "green" :foreground "black" :underline "black"))))
 '(diary ((t (:foreground "red" :weight semi-bold))))
 '(diary-anniversary ((t (:foreground "green"))))
 '(org-checkbox-face ((t (:foreground "yellow" :weight semibold))))
 '(vertico-posframe-border ((t (:background "cyan")))))

(use-package rainbow-delimiters
  :ensure t)
(use-package git-gutter
  :ensure t)
(use-package ace-window
  :ensure t)
(use-package blamer
  :ensure t
  :hook
  (prog-mode . blamer-mode))
(use-package org-ac
  :ensure t)
(use-package org-bullets
  :ensure t)
(use-package gruvbox-theme
  :ensure t)
(use-package python-isort
  :ensure t)
(use-package posframe
  :ensure t)
(use-package markdown-mode
  :ensure t)
(use-package go-autocomplete
  :ensure t)
(use-package mark-multiple
  :ensure t)
(use-package move-text
  :ensure t)
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))
(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 0.3)
  (setq which-key-use-C-h-commands nil)
  (which-key-setup-side-window-right-bottom)
  (which-key-mode t))
(use-package beacon
  :ensure t
  :config
  ;(beacon-mode t)
  (setq beacon-color "red")
  (setq beacon-blink-delay 0.4)
  (setq beacon-blink-duration 0.4))
(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode t))

(setq camcorder-output-directory "~/camcorder/")
(setq camcorder-gif-output-directory "~/camcorder/")

(add-hook 'prog-mode-hook 'git-gutter-mode)
(add-hook 'web-mode-hook 'skewer-html-mode)
(add-hook 'web-mode-hook 'flycheck-mode)
(add-hook 'html-mode-hook 'flycheck-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
(add-hook 'org-bullets-mode 'org-mode)
(add-hook 'emacs-lisp-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))
(global-set-key [f1] 'open-eshell)
(global-set-key [f2] 'dirvish-side)
(global-set-key [f8] 'helm-bibtex)
(global-set-key [f9] 'auto-complete-mode)
(global-set-key [f10] 'flyspell-mode)
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "C-M-s") 'consult-ripgrep)
(global-set-key (kbd "<C-tab>") 'yas-expand)
(global-set-key (kbd "C-è") 'transparent)
(global-set-key (kbd "C-ù") 'mark-next-like-this)
(global-set-key (kbd "C-<next>") 'tab-next)
(global-set-key (kbd "C-<prior>") 'tab-previous)
;; (global-set-key (kbd "M-n") 'embark-next-symbol)
;; (global-set-key (kbd "M-p") 'embark-previous-symbol)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "C-x C-m") 'consult-mark)
(global-set-key (kbd "C-x C-S-m") 'consult-global-mark)
(global-set-key (kbd "C-x C-r") 'consult-register)
(defvar my-C-l-map (make-sparse-keymap)
  "Keymap for my M-l prefix.")
(global-set-key (kbd "M-l") my-C-l-map)
(global-set-key (kbd "M-l l") 'downcase-word)
(global-set-key (kbd "M-l k") 'kill-whole-line)
(global-set-key (kbd "M-l M-k") 'kill-whole-line)
(global-set-key (kbd "M-l y") 'copy-paste-line)
(global-set-key (kbd "M-l M-y") 'copy-paste-line)
(global-set-key (kbd "M-l w") 'copy-line)
(global-set-key (kbd "M-l M-w") 'copy-line)
(global-set-key (kbd "M-l C-SPC") 'select-line)
(global-set-key (kbd "M-l M-SPC") 'select-line)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'org-mode-hook #'org-bullets-mode)
(add-hook 'org-mode-hook #'display-line-numbers-mode)
(add-hook 'org-mode-hook #'auto-fill-mode)
(add-hook 'go-mode-hook 'my-go-mode-hook)

(setenv "GO111MODULE" "on")

(setq gdb-many-windows t) 
(setq org-src-fontify-natively t)
(setq global-auto-complete-mode nil)
(setq org-image-actual-width nil)
(setq org-startup-with-inline-images t)
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)
      (add-hook 'TeX-after-compilation-finished-functions
           #'TeX-revert-document-buffer)
	   
(eval-after-load 'clojure-mode '(require 'clojure-mode-extra-font-locking))

(with-eval-after-load 'flycheck
  (flycheck-add-mode 'html-tidy 'web-mode)
  (flycheck-add-mode 'css-csslint 'web-mode))

(add-hook 'today-visible-calendar-hook 'calendar-mark-today)
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)

(with-eval-after-load 'org
    (require 'ob-python)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((python . t)))
    (setq org-babel-python-command "python3"))

(setq org-babel-python-command "python3")

(with-eval-after-load 'org
  (require 'ob-python)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t))))
   (with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t))))

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(use-package mwheel
  :ensure nil
  :custom
  (mouse-wheel-tilt-scroll t)
  (mouse-wheel-scroll-amount-horizontal 2)
  (mouse-wheel-flip-direction t))

(use-package docker-compose-mode
  :ensure t
  :mode "docker-compose.*\.yml\\'")

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile[a-zA-Z.-]*\\'")

(use-package emojify
  :ensure t
  :after erc
  :defer 15
  :config
  (global-emojify-mode))

(use-package ffap
  :bind ("C-c v" . ffap))
  
(use-package json-mode
  :ensure t
  :mode "\\.json\\'")
  
(use-package yasnippet
  :ensure t
  :demand t
  :diminish yas-minor-mode
  :bind (("C-c y d" . yas-load-directory)
         ("C-c y i" . yas-insert-snippet)
         ("C-c y f" . yas-visit-snippet-file)
         ("C-c y n" . yas-new-snippet)
         ("C-c y t" . yas-tryout-snippet)
         ("C-c y l" . yas-describe-tables)
         ("C-c y g" . yas/global-mode)
         ("C-c y m" . yas/minor-mode)
         ("C-c y r" . yas-reload-all)
         ("C-c y x" . yas-expand))
  :bind (:map yas-keymap
              ("C-i" . yas-next-field-or-maybe-expand))
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :config
  (yas-global-mode 1))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-status-with-prefix)))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode t)
  :config
  (setq doom-modeline-project-detection 'project)

  ;; (doom-modeline-def-modeline 'my-simple-line
  ;;   '(bar matches buffer-info remote-host buffer-position parrot selection-info)
  ;;   '(misc-info minor-modes input-method buffer-encoding major-mode vcs process checker))

  ;; ;; Set default mode-line
  ;; (add-hook 'doom-modeline-mode-hook
  ;;           (lambda ()
  ;;             (doom-modeline-set-modeline 'my-simple-line 'default)))
  )

(use-package go-mode
  :ensure t
  :bind (("C-c C-k" . go-run-this-file)
  ("C-c C-c" . go-compile))
  :hook ((before-save . eglot-format-buffer)))

(use-package lua-mode
  :ensure t)

(use-package pyenv-mode
  :ensure t
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :config
  (pyenv-mode))
  
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(use-package consult
  :ensure t
  :demand t
  :bind (("C-s" . consult-line)
         ("C-M-l" . consult-imenu)
         ("C-x p b" . consult-project-buffer)
	 ("C-x b" . consult-buffer)
	 ;("C-x C-b" . consult-bookmark)
	 ("C-x C-b" . consult-project-buffer)
	 ("C-M-s" . consult-ripgrep)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :custom
  (consult-project-root-function #'(lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (setq vertico-cycle t))

(use-package vertico-mouse
  :ensure nil
  :after vertico
  :init (vertico-mouse-mode))

(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless)))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim))        ;; good alternative: M-.
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (setq embark-mixed-indicator t)
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (:all embark consult)
  :demand t
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package eshell-toggle
  :ensure t
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-run-command nil)
  :bind
  ("C-M-`" . eshell-toggle))  

(use-package python-black
  :ensure t
  :demand t
  :after python
  :hook ((python-mode . python-black-on-save-mode)))

(with-eval-after-load 'lsp-mode
  (setq lsp-modeline-diagnostics-scope :workspace))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package dired-subtree
  :ensure t
  :bind
  (:map dired-mode-map
        ("TAB" . dired-subtree-toggle)))

(use-package dirvish
  :ensure t
  :custom
  (dirvish-bookmarks-alist
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("m" "/mnt/"                       "Drives")))
  :config
  (dirvish-override-dired-mode)
  (dirvish-peek-mode)
  (setq dirvish-attributes '(all-the-icons file-size))
  :bind
  (:map dired-mode-map
        ("SPC" . dirvish-show-history)
        ("r"   . dirvish-roam)
        ("b"   . dirvish-goto-bookmark)
        ("f"   . dirvish-file-info-menu)
        ("M-a" . dirvish-mark-actions-menu)
        ("M-s" . dirvish-setup-menu)
        ("M-f" . dirvish-toggle-fullscreen)
        ([remap dired-summary] . dirvish-dispatch)
        ([remap dired-do-copy] . dirvish-yank)
        ([remap mode-line-other-buffer] . dirvish-other-buffer)))

(use-package swiper
  :ensure t
  :bind ("C-c s" . swiper))

(use-package flycheck-golangci-lint
  :ensure t
  :hook (go-mode . flycheck-golangci-lint-setup))

(dirvish-override-dired-mode)

;; (doom-modeline-def-modeline 'my-simple-line
;;   '(bar matches buffer-info remote-host buffer-position parrot selection-info)
;;   '(misc-info minor-modes input-method buffer-encoding major-mode process vcs checker))

;(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
;  (setq completion-cycle-threshold 3)
  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
;  (setq tab-always-indent 'complete))

;; FIND NEW FACE NAME OF DOOM MODELINE PANEL
;(set-face-attribute 'tab-bar-tab nil :inherit doom-modeline-panel :foreground nil :background nil)

(use-package project-tab-groups
  :ensure
  :config
  (project-tab-groups-mode 1))

(setq minimap-minimum-width 20)

(setq-default indent-tabs-mode nil)

(use-package pyconf
  :ensure t)

(use-package eglot                                                         
  :ensure t                                                                
  :defer t                                                                 
  :hook ((python-mode . eglot-ensure)                                      
         (go-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure))
  :config                                                                  
  (add-to-list 'eglot-server-programs                                      
               `(python-mode                                               
                 . ,(eglot-alternatives '(                                 
                                          ("pyright-langserver" "--stdio") 
                                          "jedi-language-server"           
                                          "pylsp"                          
                                          )))))                            
                                                                           
(setq completion-auto-help 'always)                                        

;; New Pyconf Stuff
(setq pyconf-bootstrap-packages '("black" "'python-lsp-server[all]'" "pyright" "epc" "orjson" "sexpdata==0.0.3" "six" "mypy"))

;; `M-x combobulate' (or `C-c o o') to start using Combobulate
(use-package treesit
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css "https://github.com/tree-sitter/tree-sitter-css")
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
               (python "https://github.com/tree-sitter/tree-sitter-python")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
               (go . ("https://github.com/tree-sitter/tree-sitter-go"))
               (go-mod . ("https://github.com/camdencheek/tree-sitter-go-mod"))
               (yaml "https://github.com/ikatyang/tree-sitter-yaml")
               (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" "master" "ocaml/src"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  ;; (dolist (mapping '((python-mode . python-ts-mode)
  ;;                    (css-mode . css-ts-mode)
  ;;                    (typescript-mode . tsx-ts-mode)
  ;;                    (js-mode . js-ts-mode)
  ;;                    (css-mode . css-ts-mode)
  ;;                    (yaml-mode . yaml-ts-mode)))
  ;;   (add-to-list 'major-mode-remap-alist mapping))

  :config
  (mp-setup-install-grammars)
  ;; Do not forget to customize Combobulate to your liking:
  ;;
  ;;  M-x customize-group RET combobulate RET
  ;;
  (use-package combobulate
    ;; Optional, but recommended.
    ;;
    ;; You can manually enable Combobulate with `M-x
    ;; combobulate-mode'.
    :hook ((python-ts-mode . combobulate-mode)
           (go-ts-mode . combobulate-mode)
           (js-ts-mode . combobulate-mode)
           (css-ts-mode . combobulate-mode)
           (yaml-ts-mode . combobulate-mode)
           (typescript-ts-mode . combobulate-mode)
           (tsx-ts-mode . combobulate-mode))
    ;; Amend this to the directory where you keep Combobulate's source
    ;; code.
    :load-path ("~/.emacs.d/combobulate")))

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(add-hook 'project-find-functions #'project-find-go-module)

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-to-list 'load-path "~/.emacs.d/lsp-bridge")

(dolist (mapping '((python-mode . python-ts-mode)
                   (go-mode . go-ts-mode)
                     (css-mode . css-ts-mode)
                     (typescript-mode . tsx-ts-mode)
                     (js-mode . js-ts-mode)
                     (css-mode . css-ts-mode)
                     (yaml-mode . yaml-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))

;; Some requires
(require 'project)
(require 'pyconf)
(require 'functions)
(require 'ox-html)
(require 'cl-lib)
(require 'ox-publish)
(require 'calendar)
(require 'go-autocomplete)
(require 'mark-multiple)
(require 'init-windows)
(require 'embark)
(require 'python-isort)
(require 'init-flagged)
(require 'catppuccin-theme)
(night-day-theme)
(setq sml/no-confirm-load-theme t)
(setq sml/theme 'atom-one-dark)
(sml/setup)
(load-file "~/.emacs.d/eshell-p10k.el")
(setq eshell-prompt-function #'eshell-p10k-prompt-function
        eshell-prompt-regexp eshell-p10k-prompt-string)

(add-hook 'python-mode-hook 'python-isort-on-save-mode)

(move-text-default-bindings)
(org-display-inline-images t t)

(setq treesit-language-source-alist
  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
    (c "https://github.com/tree-sitter/tree-sitter-c")
    (cmake "https://github.com/uyha/tree-sitter-cmake")
    (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
    (css "https://github.com/tree-sitter/tree-sitter-css")
    (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
    (elisp "https://github.com/Wilfred/tree-sitter-elisp")
    (go "https://github.com/tree-sitter/tree-sitter-go")
    (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
    (html "https://github.com/tree-sitter/tree-sitter-html")
    (js . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
    (json "https://github.com/tree-sitter/tree-sitter-json")
    (lua "https://github.com/Azganoth/tree-sitter-lua")
    (make "https://github.com/alemuller/tree-sitter-make")
    (markdown "https://github.com/ikatyang/tree-sitter-markdown")
    (python "https://github.com/tree-sitter/tree-sitter-python")
    (r "https://github.com/r-lib/tree-sitter-r")
    (rust "https://github.com/tree-sitter/tree-sitter-rust")
    (toml "https://github.com/tree-sitter/tree-sitter-toml")
    (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
    (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
    (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(use-package vertico-posframe
  :config
  (setq vertico-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8)))
  (setq vertico-posframe-border-width 3)

  :init
  (vertico-posframe-mode)

  ;; This is from https://gist.github.com/lazy/3f3a1eff443adf5c1a4630055b5a5f6e
  (defface vertico-posframe-dim-face
    '((t (:foreground "gray40")))
    "Face for whole window background when posframe is active.")
  (setq vertico-posframe-aside-rules
        '("find-file" "consult-line" "consult-imenu" "consult-eglot-symbols" "project-find-regexp" "consult-yank" "xref"))
  (defun vertico-posframe--rules-match-p (rules)
    "Tests whether current command or buffer matches one of the RULES."
    (cl-some
     (lambda (rule)
       (cond ((functionp rule)
              (funcall rule))
             ((and rule
                   (stringp rule)
                   (symbolp this-command))
              (string-match-p rule (symbol-name this-command)))
             ((symbolp rule)
              (symbol-value rule))
             (t nil)))
     rules))
  (setq vertico-posframe--overlays-back nil)
  (defun vertico-posframe--add-overlays ()
    "Create a dim background overlay for each window on WND-LIST."
    (let* ((wnd (minibuffer-selected-window))
           (windows (window-list (window-frame wnd) 0))
           (windows-to-dim
            (if (vertico-posframe--rules-match-p vertico-posframe-aside-rules)
                (--select (not (eq it wnd)) windows)
              windows)))
      (setq vertico-posframe--overlays-back
            (append
             vertico-posframe--overlays-back
             (mapcar (lambda (w)
                       (let ((ol (make-overlay
                                  (window-start w)
                                  (window-end w)
                                  (window-buffer w))))
                         (overlay-put ol 'face 'aw-background-face)
                         ol))
                     windows-to-dim)))))

  (defun vertico-posframe--remove-overlays ()
    (mapc #'delete-overlay vertico-posframe--overlays-back)
    (setq vertico-posframe--overlays-back nil))

  (advice-add 'vertico-posframe--setup :after #'vertico-posframe--add-overlays)
  (advice-add 'vertico-posframe--minibuffer-exit-hook :after #'vertico-posframe--remove-overlays)

  (defun lazy/posframe-poshandler-besides-selected-window (info)
    (let* ((window-left (plist-get info :parent-window-left))
           (window-width (plist-get info :parent-window-width))
           (window-right (+ window-left window-width))
           (frame-width (plist-get info :parent-frame-width))
           (frame-height (plist-get info :parent-frame-height))
           (posframe-width (plist-get info :posframe-width))
           (posframe-height (plist-get info :posframe-height))
           (space-left window-left)
           (space-right (- frame-width window-right))
           (space-inside (- window-width posframe-width))
           (wanted-top (/ (- frame-height posframe-height) 2)))
      (cond
       ((>= space-right posframe-width)
        (cons window-right wanted-top))
       ((>= space-left posframe-width)
        (cons (- window-left posframe-width) wanted-top))
       ((>= space-inside posframe-width)
        (cons (- window-right posframe-width) wanted-top))
       (t
        (posframe-poshandler-frame-center info)))))
  (setq vertico-posframe-poshandler #'lazy/posframe-poshandler-besides-selected-window)
  (defun lazy/vertico-posframe-get-size (minibuffer-name)
    "The default functon used by `vertico-posframe-size-function'."
    (let ((width (- (round (* (frame-width) 0.5)) 2))
          (height (or vertico-posframe-min-height
                      (let ((height1 (+ vertico-count 1)))
                        (min height1 (or vertico-posframe-height height1))))))
      (list
       :height height
       :width width
       :min-height height
       :min-width width
       :max-height height
       :max-width width)))
  (setq vertico-posframe-size-function #'lazy/vertico-posframe-get-size))

;; (setq desktop-path '("~/"))
;; (desktop-save-mode 1)
;; (setq savehist-additional-variables
;;       '(shell-command-history
;;         register-alist))
;; (savehist-mode 1)

