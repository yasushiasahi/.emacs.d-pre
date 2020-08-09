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
