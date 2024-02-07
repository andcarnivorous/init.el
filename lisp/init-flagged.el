(setq use-company t)

(when use-company
  (use-package company
    :ensure t
    :hook ((prog-mode . company-mode))
    :bind (:map company-active-map
                ("<return>" . nil)
                ("RET" . nil)
                ("C-<return>" . company-complete-selection)
                ([tab] . company-complete-selection)
                ("TAB" . company-complete-selection)))
  (use-package company-box
    :ensure t
    :hook (company-mode . company-box-mode)))

(unless use-company
  (use-package corfu                                                            
    :after orderless                                                            
    ;; Optional customizations                                                  
    :custom                                                                     
    (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'  
    (corfu-auto t)                 ;; Enable auto completion                    
    (corfu-separator ?\s)          ;; Orderless field separator                 
    (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary         
    (corfu-quit-no-match nil)      ;; Never quit, even if there is no match     
    (corfu-preview-current nil)    ;; Disable current candidate preview         
    ;; (corfu-preselect-first nil)    ;; Disable candidate preselection         
    ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches    
    ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area 
    (corfu-scroll-margin 5)        ;; Use scroll margin                         
    
    ;; Enable Corfu only for certain modes.                                     
    :hook ((prog-mode . corfu-mode)                                             
           (shell-mode . corfu-mode)                                            
           (eshell-mode . corfu-mode))                                          
    
    ;; Recommended: Enable Corfu globally.                                      
    ;; This is recommended since Dabbrev can be used globally (M-/).            
    ;; See also `corfu-excluded-modes'.                                         
    :init                                                                       
    (global-corfu-mode)                                                         
    (setq corfu-auto t))
  (define-key corfu-map (kbd "M-p") #'corfu-popupinfo-scroll-down) ;; corfu-next       
  (define-key corfu-map (kbd "M-n") #'corfu-popupinfo-scroll-up)  ;; corfu-previous    
                                                                                     
  (use-package kind-icon                                                               
    :ensure t                                                                          
    :after corfu                                                                       
    :custom                                                                            
    (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly 
    :config                                                                            
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

(unless (display-graphic-p)
  (use-package vertico-buffer
    :ensure nil
    :after vertico
    :init (vertico-buffer-mode)))

(when (display-graphic-p)
  (use-package vertico-posframe
  :config
  (setq vertico-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8)))
  (setq vertico-posframe-border-width 3)

  :init
  (vertico-posframe-mode)

  (defface vertico-posframe-dim-face
    '((t (:foreground "gray40")))
    "Face for whole window background when posframe is active.")
  (setq vertico-posframe-aside-rules
        '(;"find-file"
	  "consult-line" "consult-imenu" "consult-eglot-symbols" "consult-yank"
	  "project-find-regexp"
	  "xref"))
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
       ;; ((>= space-inside posframe-width)
       ;;  (cons (- window-right posframe-width) wanted-top))
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
  (setq vertico-posframe-size-function #'lazy/vertico-posframe-get-size)))
  
  ;; (use-package vertico-posframe
  ;;   :ensure nil
  ;;   :config (vertico-posframe-mode 1))

  ;; (setq vertico-multiform-commands
  ;;       '((consult-line
  ;;          posframe
  ;;          (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
  ;;          (vertico-posframe-border-width . 5)
  ;;          ;; NOTE: This is useful when emacs is used in both in X and
  ;;          ;; terminal, for posframe do not work well in terminal, so
  ;;          ;; vertico-buffer-mode will be used as fallback at the
  ;;          ;; moment.
  ;;          (vertico-posframe-fallback-mode . vertico-buffer-mode))
  ;;         (t posframe)))
  ;; (vertico-multiform-mode 1))

(provide 'init-flagged)
