;; Pierre's init.el

(setq evil-want-C-u-scroll t)


(require 'package)
(add-to-list 'package-archives
 	     '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(setq compilation-scroll-output t)

;; default Latin font (e.g. Consolas)
;; (set-face-attribute 'default nil :family "Consolas")

;; default font size (point * 10)
;;
;; WARNING!  Depending on the default font,
;; if the size is not supported very well, the frame will be clipped
;; so that the beginning of the buffer may not be visible correctly.
;; (set-face-attribute 'default nil :height 165)

;; use specific font for Korean charset.
;; if you want to use different font size for specific charset,
;; add :size POINT-SIZE in the font-spec.
;; (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))

(setq initial-frame-alist '((font . "Inconsolata-g-12")))
(setq default-frame-alist '((font . "Inconsolata-g-12")))

;; you may want to add different for other charset in this way.


(scroll-bar-mode -1)

(require 'alchemist)

(setq custom-file (expand-file-name "evil-little-word.el" user-emacs-directory))
(load custom-file)


(require 's)

;; (require 'evil-little-word)

;; (load-theme 'solarized-dark t)

(global-evil-leader-mode)
(evil-leader/set-leader ",")

(require 'evil)
(evil-mode 1)


(defgroup navigate nil
  "seamlessly navigate between Emacs and tmux"
  :prefix "navigate-"
  :group 'evil)

; Without unsetting C-h this is useless
(global-unset-key (kbd "C-h"))

; This requires windmove commands
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(defun tmux-navigate (direction)
  (let
    ((cmd (concat "windmove-" direction)))
      (condition-case nil
          (funcall (read cmd))
        (error
          (tmux-command direction)))))

(defun tmux-command (direction)
  (shell-command-to-string
    (concat "tmux select-pane -"
      (tmux-direction direction))))

(defun tmux-direction (direction)
  (upcase
    (substring direction 0 1)))

(define-key evil-normal-state-map
            (kbd "C-h")
            (lambda ()
              (interactive)
              (tmux-navigate "left")))
(define-key evil-normal-state-map
            (kbd "C-j")
            (lambda ()
              (interactive)
              (tmux-navigate "down")))
(define-key evil-normal-state-map
            (kbd "C-k")
            (lambda ()
              (interactive)
              (tmux-navigate "up")))
(define-key evil-normal-state-map
            (kbd "C-l")
            (lambda ()
              (interactive)
              (tmux-navigate "right")))

(tool-bar-mode -1)
(menu-bar-mode -1)

(global-relative-line-numbers-mode)
(setq linum-format "%d ")
(global-set-key (kbd "C-x g") 'magit-status)

;; (require 'magit-gh-pulls)
;; (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)


(require 'evil-magit)

(setq git-commit-summary-max-length 70)

(setq fiplr-ignored-globs '((directories (".git" ".svn" "vendor" "tmp" "deps" "node_modules"))
                            (files ("*.jpg" "*.png" "*.zip" "*~" ".DS_Store"))))


(evil-leader/set-key "f" 'fiplr-find-file)

(global-evil-visualstar-mode)
(define-key evil-normal-state-map (kbd ";") 'evil-ex)

;; Replicate vim CamelCaseMotion
(define-key evil-normal-state-map (kbd "w") 'evil-forward-little-word-begin)
(define-key evil-normal-state-map (kbd "e") 'evil-forward-little-word-end)
(define-key evil-normal-state-map (kbd "b") 'evil-backward-little-word-begin)

(define-key evil-visual-state-map (kbd "w") 'evil-forward-little-word-begin)
(define-key evil-visual-state-map (kbd "e") 'evil-forward-little-word-end)
(define-key evil-visual-state-map (kbd "b") 'evil-backward-little-word-begin)

(evil-commentary-mode)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'editorconfig)
(editorconfig-mode 1)

;; (require 'emamux-ruby-test)
;; (global-emamux-ruby-test-mode)

(add-hook 'focus-out-hook 'save-buffer)

(require 'pbcopy)
(turn-on-pbcopy)

(load-theme 'zenburn t)

(global-hl-line-mode 1)
(set-face-background 'hl-line "#3e4446")
(set-face-foreground 'highlight nil)

(require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))



;; (evil-leader/set-key "sp" 'emamux-ruby-test:run-current-test)
;; (evil-leader/set-key "t" 'emamux-ruby-test:run-test-file)
(evil-leader/set-key "sp" 'rspec-verify-single)
(evil-leader/set-key "t" 'rspec-verify)
(evil-leader/set-key "l" 'rspec-rerun)


;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; enable recent files mode.
(recentf-mode t)

; 50 files ought to be enough.
(setq recentf-max-saved-items 50)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))


(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

(setq inhibit-startup-message t)

(fset 'evil-visual-update-x-selection 'ignore)

(setq ring-bell-function 'ignore)


(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-Buffer) 1)))


(evil-leader/set-key "," 'switch-to-previous-buffer)

(require 'editorconfig)
(editorconfig-mode 1)

(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

(defvar sanityinc/fci-mode-suppressed nil)
(defadvice popup-create (before suppress-fci-mode activate)
  "Suspend fci-mode while popups are visible"
  (set (make-local-variable 'sanityinc/fci-mode-suppressed) fci-mode)
  (when fci-mode
    (turn-off-fci-mode)))
(defadvice popup-delete (after restore-fci-mode activate)
  "Restore fci-mode when all popups have closed"
  (when (and (not popup-instances) sanityinc/fci-mode-suppressed)
    (setq sanityinc/fci-mode-suppressed nil)
    (turn-on-fci-mode)))

(column-number-mode)

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; (add-hook 'js-mode-hook
;; 	  (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

;; (add-hook 'ruby-mode-hook
;; 	  (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(add-hook 'ruby-mode-hook 'projectile-mode)

(ac-config-default)
