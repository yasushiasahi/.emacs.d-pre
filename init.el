;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @straightと@use-packageでコードを管理
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(setq straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq use-package-always-defer t
      use-package-verbose t
      use-package-minimum-reported-time 0.01
      use-package-enable-imenu-support t)


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; キーバインド
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>")) ; C-hでバックスペース


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @company-mode
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package company
  :config
  (global-company-mode))


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @fly-check
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package flycheck)



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @web-mode
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package web-mode
  :config
  (setq web-mode-content-type "jsx"))


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @lsp-mode
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "s-l")

(use-package lsp-mode)
(use-package lsp-ui :commands lsp-ui-mode)







