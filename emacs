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
;;; package source



;; custome setting
(require 'main-line)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(scroll-bar-mode nil)
 '(tooltip-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata" :foundry "unknown" :slant normal :weight normal :height 120 :width normal)))))

;; global mode
(global-linum-mode 1)
(global-auto-revert-mode 1)
(setq inhibit-startup-message t)


;; quick switch windows, ace-window
(global-set-key (kbd "M-p") 'ace-window)

;; js2-mode setting
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(setq js-indent-level 4)
(setq-default indent-tabs-mode nil)
;; ac-js2-mode
(add-hook 'js2-mode-hook 'ac-js2-mode)
(require 'yasnippet)
(yas-global-mode 1)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

;; autopair
(require 'autopair)
(autopair-global-mode)

;; ternjs
(add-to-list 'load-path "/home/tate/.emacs.d/elpa/tern/emacs")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
 '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

;; nodejs
(require 'nodejs-repl)
(require 'js2-refactor)
(add-hook 'js2-mode-hook #'js2-refactor-mode)

;; backup
(setq backup-by-copying t
    backup-directory-alist '(("~/.emacs_backup"))
    delete-old-version t
    kept-new-version 6
    kept-old-version 2
    version-control t)

;; highlight
;; (require 'highlight-tail)
;; (highlight-tail-mode)

;;; jade-mode
(require 'jade-mode)
