;; AngeFtp passive mode
(defvar ange-ftp-hosts-no-pasv '("localhost")
  "*List of hosts that do not need PASV (e.g. hosts within your firewall).
  Used by `ange-ftp-set-passive'.")	; rephrased, added "*" // era

(defun ange-ftp-set-passive ()
  "Function to send a PASV command to hosts not named in the variable
  `ange-ft-hosts-no-pasv'. Intended to be called from the hook variable
  `ange-ftp-process-startup-hook'."	; rephrased significantly // era
  (if (not (member host ange-ftp-hosts-no-pasv))
      (ange-ftp-raw-send-cmd proc "passive")))
(add-hook 'ange-ftp-process-startup-hook 'ange-ftp-set-passive)


(defun comment-or-uncomment-line (&optional lines)
  "Comment current line. Argument gives the number of lines
forward to comment"
  (interactive "P")
  (comment-or-uncomment-region
   (line-beginning-position)
   (line-end-position lines)))

(defun comment-or-uncomment-region-or-line (&optional lines)
  "If the line or region is not a comment, comments region
if mark is active, line otherwise. If the line or region
is a comment, uncomment."
  (interactive "P")
  (if mark-active
      (if (< (mark) (point))
	  (comment-or-uncomment-region (mark) (point))
	(comment-or-uncomment-region (point) (mark))
	)
    (comment-or-uncomment-line lines)))
(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)

;; Flyspell mode popup
(add-hook 'flyspell-mode-hook
 (lambda () (define-key flyspell-mode-map (kbd "C-x p") 'flyspell-correct-word-before-point)))

;; Get rid of the startup screen
(setq inhibit-startup-message t)

;; Follow symbolic links
(setq vc-follow-symlinks t)

;; Full screen mode, set it by default (F11)
;; (defun switch-full-screen ()
;;   (interactive)
;;   (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;;   	    		 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
;;   (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;;   	    		 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))
;; (global-set-key [f11] 'switch-full-screen)
;; (run-with-idle-timer 0.01 nil 'switch-full-screen)

;; Enable linenumering
(global-linum-mode 1)

;; Disable scrollbars
(scroll-bar-mode nil)

;; Switch color theme
;; (require 'color-theme)
(color-theme-initialize)
(color-theme-blippblopp)
;; (color-theme-zenburn)


;; Turn off visible-bell
(setq visible-bell nil)

;; Wraping long lines off
(longlines-mode -1)

;; Longlines off
(add-hook 'html-mode-hook
          (lambda ()
             (auto-fill-mode 0)))

;; Switching between windows
(global-set-key (kbd "C-c <C-right>") 'windmove-right)
(global-set-key (kbd "C-c <C-left>") 'windmove-left)
(global-set-key (kbd "C-c <C-up>") 'windmove-up)
(global-set-key (kbd "C-c <C-down>") 'windmove-down)

(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)

;; My private to do mode ToDo:
(global-set-key (kbd "C-x t") (lambda ()
				(interactive)
				(find-file-other-window "~/Dropbox/home/todo.org")))

;; Put autosave files (ie #foo#) in one place
(defvar autosave-dir
 (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))

(make-directory autosave-dir t)

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)


;; Remove pretty anoying hook
(remove-hook 'espresso-mode-hook 'esk-paredit-nonlisp)


(add-hook 'python-mode-hook
          (lambda () (define-key python-mode-map (kbd "C-c w") 'whitespace-cleanup)
            (whitespace-mode 1)
            ))

(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))          
(setq tramp-chunksize 500)

