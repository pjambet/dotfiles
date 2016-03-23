;;; evil-little-word.el --- Emulate camelcasemotion.vim

;; Author: INA Lintaro <tarao.gnn at gmail.com>
;; URL: http://github.com/tarao/evil-plugins
;; Version: 0.1
;; Keywords: evil, plugin

;; This file is NOT part of GNU Emacs.

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'evil)

(defun maybe-define-category (cat doc &optional table)
  (unless (category-docstring cat table) (define-category cat doc table)))

(let (uc lc defs (table (standard-category-table)))
  (map-char-table
   #'(lambda (key value)
       (when (natnump value)
         (let (from to)
           (if (consp key)
               (setq from (car key) to (cdr key))
             (setq from (setq to key)))
           (while (<= from to)
             (cond ((/= from (downcase from))
                    (add-to-list 'uc from))
                   ((/= from (upcase from))
                    (add-to-list 'lc from)))
             (setq from (1+ from))))))
   (standard-case-table))
  (setq defs `(("Uppercase" ?U ,uc)
               ("Lowercase" ?u ,lc)
               ("Underscore" ?_ (?_))))
  (dolist (elt defs)
    (maybe-define-category (cadr elt) (car elt) table)
    (dolist (ch (car (cddr elt)))
      (modify-category-entry ch (cadr elt) table))))

(defgroup evil-little-word nil
  "CamelCase and snake_case word movement support."
  :prefix "evil-little-word-"
  :group 'evil)

(defcustom evil-little-word-separating-categories
  (append evil-cjk-word-separating-categories '((?u . ?U) (?_ . ?u) (?_ . ?U)))
  "List of pair (cons) of categories to determine word boundary
for little word movement. See the documentation of
`word-separating-categories'. Use `describe-categories' to see
the list of categories."
  :type '((character . character))
  :group 'evil-little-word)

(defcustom evil-little-word-combining-categories
  (append evil-cjk-word-combining-categories '())
  "List of pair (cons) of categories to determine word boundary
for little word movement. See the documentation of
`word-combining-categories'. Use `describe-categories' to see the
list of categories."
  :type '((character . character))
  :group 'evil-little-word)

(defmacro evil-with-little-word (&rest body)
  (declare (indent defun) (debug t))
  `(let ((evil-cjk-word-separating-categories
          evil-little-word-separating-categories)
         (evil-cjk-word-combining-categories
          evil-little-word-combining-categories))
     ,@body))

(defun forward-evil-little-word (&optional count)
  "Forward by little words."
  (evil-with-little-word (forward-evil-word count)))

(evil-define-motion evil-forward-little-word-begin (count)
  "Move the cursor to the beginning of the COUNT-th next little word."
  :type exclusive
  (evil-with-little-word (evil-forward-word-begin count)))

(evil-define-motion evil-forward-little-word-end (count)
  "Move the cursor to the end of the COUNT-th next little word."
  :type inclusive
  (evil-with-little-word (evil-forward-word-end count)))

(evil-define-motion evil-backward-little-word-begin (count)
  "Move the cursor to the beginning of the COUNT-th previous little word."
  :type exclusive
  (evil-with-little-word (evil-backward-word-begin count)))

(evil-define-motion evil-backward-little-word-end (count)
  "Move the cursor to the end of the COUNT-th previous little word."
  :type inclusive
  (evil-with-little-word (evil-backward-word-end count)))

(evil-define-text-object evil-a-little-word (count &optional beg end type)
  "Select a little word."
  (evil-select-an-object 'evil-little-word beg end type count))

(evil-define-text-object evil-inner-little-word (count &optional beg end type)
  "Select inner little word."
  (evil-select-inner-object 'evil-little-word beg end type count))

(define-key evil-motion-state-map (kbd "glw") 'evil-forward-little-word-begin)
(define-key evil-motion-state-map (kbd "glb") 'evil-backward-little-word-begin)
(define-key evil-motion-state-map (kbd "glW") 'evil-forward-little-word-end)
(define-key evil-motion-state-map (kbd "glB") 'evil-backward-little-word-end)
(define-key evil-outer-text-objects-map (kbd "lw") 'evil-a-little-word)
(define-key evil-inner-text-objects-map (kbd "lw") 'evil-inner-little-word)

(provide 'evil-little-word)
;;; evil-little-word.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(emamux:use-nearest-pane t)
 '(magit-branch-arguments nil)
 '(package-selected-packages
   (quote
    (fill-column-indicator auto-complete inf-ruby ag markdown-mode smartparens solarized-theme alchemist elixir-mode rspec-mode emamux-ruby-test projectile emamux zenburn-theme web-mode scss-mode ruby-end relative-line-numbers pbcopy magit-gh-pulls highlight-current-line fiplr evil-visualstar evil-surround evil-numbers evil-magit evil-leader evil-commentary editorconfig)))
 '(safe-local-variable-values
   (quote
    ((ruby-indent-level . 4)
     (css-indent-offset . 4)
     (sgml-basic-offset . 4)
     (js-indent-level . 4)
     (whitespace-line-column . 120)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3F3F3F" :foreground "#DCDCCC" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Inconsolata")))))
