;;; Emacs is not a package manager, and here we load its package manager!
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")
                  ;; TODO: Maybe, use this after emacs24 is released
                  ;; (development versions of packages)
                  ("melpa" . "http://melpa.milkbox.net/packages/")
                  ))
  (add-to-list 'package-archives source t))
(package-initialize)


;;; no message on start up
(setq inhibit-startup-screen t)
(add-to-list 'default-frame-alist '(width . 160))
(add-to-list 'default-frame-alist '(height . 50))


;;; theme setting and font
(load-theme 'monokai t)
(add-to-list 'default-frame-alist '(font . "Menlo-12"))
(require 'powerline)
(powerline-center-theme)

;;; frame window setting
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq frame-title-format "Tate")

;;; anthor setting
(ido-mode)
(column-number-mode)
(show-paren-mode)

;;;
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(ac-config-default)

;;;
(global-nlinum-mode)
(autopair-global-mode)

;;; undo-tree
(global-undo-tree-mode)
(global-set-key (kbd "M-/") 'undo-tree-visualize)


;;; switch-window
(global-set-key (kbd "C-M-z") 'switch-window)

;;;ace-jump-mode
(global-set-key (kbd "C->") 'ace-jump-mode)

;;; alpha
(global-set-key (kbd "C-M-)") 'transparency-increase)
(global-set-key (kbd "C-M-(") 'transparency-decrease)



;;;js2-mode
(add-hook 'js-mode 'js2-mode)
(setq js-indent-level 4)
(require 'tern)
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js2-mode (lambda()(tern-mode t)))

;;; web-mode
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
