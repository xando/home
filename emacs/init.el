;; Get rid of the startup screen
(setq inhibit-startup-message t)

;; Packages
(package-initialize)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

(load-file "~/.emacs.d/emacs-for-python/epy-init.el")
(epy-setup-checker "pyflakes %f")


;; Ido
(require 'ido)
(ido-mode t)

;; follow symbolic links
(setq vc-follow-symlinks t)

;; Fonts
(set-face-attribute 'default nil :family "Inconsolata-g" :height 110)

;; Enable linenumering
(global-linum-mode 1)

;; ;; Color theme Zenburn
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

;; Highlight line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#4A4A4A");; Nice color

;; Flyspell mode popup
(add-hook 'flyspell-mode-hook
 (lambda () (define-key
          flyspell-mode-map
          (kbd "C-x p") 'flyspell-correct-word-before-point)))

;; Comment or uncomment region or line
(defun comment-or-uncomment-region-or-line ()
  (interactive)
  (if (not mark-active)
      (comment-or-uncomment-region
        (line-beginning-position) (line-end-position))
      (if (< (point) (mark))
          (comment-or-uncomment-region (point) (mark))
        (comment-or-uncomment-region (mark) (point)))))
(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)


;; Switching between windows Shift+direction
(windmove-default-keybindings)

;; Movment
(global-set-key (kbd "C-'") 'forward-char)
(global-set-key (kbd "C-l") 'backward-char)
(global-set-key (kbd "C-;") 'next-line)
(global-set-key (kbd "C-p") 'previous-line)

(global-set-key (kbd "M-p") 'backward-paragraph) ;
(global-set-key (kbd "M-;") 'forward-paragraph)
(global-set-key (kbd "M-'") 'forward-word)
(global-set-key (kbd "M-l") 'backward-word)

(global-set-key (kbd "C-M-'") 'move-end-of-line)
(global-set-key (kbd "C-M-l") 'move-beginning-of-line)

;; Magit status
(global-set-key (kbd "C-x g") 'magit-status)
(add-hook 'magit-mode-hook
    (lambda ()
    (set-face-foreground 'magit-diff-add "green yellow")
    (set-face-foreground 'magit-diff-del "SandyBrown")
    (set-face-background 'magit-item-highlight "grey32")
))


(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))
(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)

;; Emacs re-center
(global-set-key (kbd "C-<tab>") 'recenter)

;; Visible bellmag
(setq visible-bell nil)

;; ;; Show in which function cursor stands
;; (which-func-mode 1)

;; Scrolling
(setq redisplay-dont-pause t)
(setq
 scroll-margin 0                  
 scroll-conservatively 100000
 scroll-preserve-screen-position 1)

(global-set-key (kbd "C-c w") 'whitespace-cleanup)

(require 'whitespace)

(add-hook 'python-mode-hook
    (lambda () 
      (font-lock-mode 1)
      ;; (setq whitespace-line-column 80
      ;;       whitespace-style '(face tabs trailing lines-tail))
      (whitespace-mode t)
      ;; (font-lock-mode t)
))

;; (add-hook 'js-mode-hook
;; 	  (lambda () 
;; 	    (setq whitespace-line-column 80
;; 		  whitespace-style '(face tabs trailing lines-tail))
;; 	    (whitespace-mode t)
;; 	    (setq indent-tabs-mode nil)
;; 	    (setq tab-width 4)
;; 	    ))

(add-hook 'css-mode-hook
    (lambda () 
      (setq whitespace-line-column 80
            whitespace-style '(face tabs trailing lines-tail))
      (whitespace-mode t)
))

(add-hook 'sgml-mode-hook
	  (lambda () 
	    (setq whitespace-line-column 80
	    	  whitespace-style '(face trailing lines-tail))
	    (whitespace-mode t)
	    (setq sgml-basic-offset 4)
	    (setq indent-tabs-mode t)
	    ))

(add-hook 'coffee-mode-hook 
	  (lambda () 
	    (setq whitespace-line-column 80
		  whitespace-style '(face trailing lines-tail))
	    (whitespace-mode t)
	    (setq indent-tabs-mode t)
	    (setq tab-width 4)
	    ))


;; ;; (add-hook 'html-mode-hook
;; ;;     (lambda () (whitespace-mode t)
;; ;;     (setq tab-width 4)))

;; ;; (add-hook 'emacs-lisp-mode-hook (lambda ()
;; ;;     (whitespace-mode t)
;; ;;     (setq tab-width 4)
;; ;; ))

(defun vc-git-grep-simple(search-term)
 (interactive (list (read-from-minibuffer "Search for: "
                                           (if (symbol-at-point)
                                               (symbol-name (symbol-at-point))))))
 (if (buffer-file-name)
     (let ((project-root (expand-file-name (vc-git-root (buffer-file-name)))))
       (if project-root
           (vc-git-grep search-term "*" project-root)
         (message "Couldn't find project root.")))
   (message "You need to be in a file buffer.")))

(global-set-key (kbd "M-g M-v") 'vc-git-grep-simple)

;; ;; (setq indent-line-function 'insert-tab)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(blink-cursor-mode nil)
 '(custom-safe-themes (quote ("374e79a81930979e673b8e0869e135fb2450b18c6474ca145f104e0c6f003267" default)))
 '(hl-line-mode nil t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata-g" :foundry "unknown" :slant normal :weight normal :height 98 :width normal))))
 '(region ((t (:background "gray32")))))
