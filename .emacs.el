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

; search setting
(setq-default case-fold-search nil)
(setq user-mial-address "yongtao.fan@adleida.com")


; input method change
(require 'ibus)
(add-hook 'after-init-hook 'ibus-mode-on)

(require 'yasnippet)
(yas-global-mode 1)

(require 'powerline)
(require 'auto-complete)
(powerline-center-theme)
; highlight current line
(global-hl-line-mode 1)
; theme
(load-theme 'wombat)
; smart programmer use spaces not tabs
(setq-default indent-tabs-mode nil)

; line number and col number
(line-number-mode 1)
(column-number-mode 1)

; set fill column
(setq-default fill-column 80)

; New buffer as text mode
(setq default-major-mode 'text-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (atom-dark)))
 '(custom-safe-themes (quote ("e8586a76a96fd322ccb644ca0c3a1e4f4ca071ccfdb0f19bef90c4040d5d3841" "f9d34593e9dd14b2d798494609aa0fddca618145a5d4b8a1819283bc5b7a2bfd" "beeb5ac6b65fcccfe434071d4624ff0308b5968bf2f0c01b567d212bcaf66054" "a1289424bbc0e9f9877aa2c9a03c7dfd2835ea51d8781a0bf9e2415101f70a7e" "8f0334c430540bf45dbcbc06184a2e8cb01145f0ae1027ce6b1c40876144c0c9" "a0bbe4dc3513cbd049eb95f79c467b6f19dc42979fec27a0481bb6980bd8d405" "51277c9add74612c7624a276e1ee3c7d89b2f38b1609eed6759965f9d4254369" "aab598c4d024d544b4e8b356a95ca693afa9de000b154bd2f86eed68c9e75557" "b869a1353d39ab81b19eb79de40ff3e7bb6eaad705e61f7e4dbdcb183f08c5a6" "b5fe3893c8808466711c1b55bb7e66b9c6aa2a86811783375a43e1beabb1af33" "fbcdb6b7890d0ec1708fa21ab08eb0cc16a8b7611bb6517b722eba3891dfc9dd" "8e7ca85479dab486e15e0119f2948ba7ffcaa0ef161b3facb8103fb06f93b428" default)))
 '(show-paren-mode t)
 '(tooltip-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "WenQuanYi Micro Hei Mono" :foundry "unknown" :slant normal :weight normal :height 98 :width normal)))))
(put 'upcase-region 'disabled nil)

