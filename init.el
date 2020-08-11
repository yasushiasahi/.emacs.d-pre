;;; init.el ---  -*- coding: utf-8 ; lexical-binding: t -*-
;;; Commentary:

;;; Code:

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


;;; ++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;  オートセーブ、バックアップ
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(add-to-list 'backup-directory-alist (cons "." "~/.emacs.d/backups/")) ; バックアップファイルはbackups/へ保存
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "~/.emacs.d/backups/") t))) ; オートセーブファイルもbackups/へ保存

;; 更新されたファイルを自動で読み直す
(global-auto-revert-mode t)
(setq create-lockfiles nil)



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; キーバインド
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(global-unset-key (kbd "C-u")) ;C-uは一旦無効化
(global-unset-key (kbd "C-t")) ; デフォルトのC-tを無効化
(global-unset-key (kbd "C-q")) ; デフォルトのC-q(特殊文字入力)を無効化
(global-unset-key (kbd "C-\\")) ;C-\(日本語入力)を無効化
(global-set-key (kbd "C-m") 'newline-and-indent) ; 改行してインデント
(global-set-key (kbd "C-x ?") 'help-command) ; ヘルプコマンド
(global-set-key (kbd "C-^") 'universal-argument) ; defaultのC-u
(global-set-key (kbd "C-M-d") 'kill-word) ; 単語ごとに削除
(global-set-key (kbd "C-c l") 'toggle-truncate-lines) ; 折り返しをトグル
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>")) ; C-hでバックスペース

(use-package crux
  :bind (("C-u" . crux-smart-open-line-above) ;; 現在行を改行せずに上に空行を作ってその行に移動
	 ("C-j" . crux-smart-open-line) ;; 現在行を改行せずに下に空行を作ってその行に移動
	 ("M-k" . crux-kill-whole-line) ;; 現在行全体を削除して詰める
	 ("M-h" . crux-kill-line-backwards) ;; インデント位置からカーソル位置までを削除
	 ("C-a" . crux-move-beginning-of-line) ;; C-a連打で行頭→行の最初のインデント位置への移動を繰り返す
	 ("M-d" . crux-duplicate-current-line-or-region) ;; 現在行or選択行を下に複製
	 ("M-\\" . crux-duplicate-and-comment-current-line-or-region))) ;; 現在行or選択行を下に複製してコメントアウト



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; 一般設定（機能）
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; スタートアップメッセージを非表示
(setq inhibit-startup-screen t)

;; yes/noはすべてy/nで答える
(defalias 'yes-or-no-p 'y-or-n-p)

;; 一行ずつスクロール
(setq scroll-conservatively 1)

;; 文字コード
(set-language-environment "Japanese") ; 日本語推奨環境
(prefer-coding-system 'utf-8) ; utf-8が最優先
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)


(when (eq window-system 'ns)
  (tool-bar-mode 0) ;toolbarを非表示
  (scroll-bar-mode 0) ;scrollbarを非表示
  )

(when (eq window-system 'nil)
  ;; コピペの設定darwin専用
  (defun copy-from-osx ()
   (shell-command-to-string "pbpaste"))

  (defun paste-to-osx (text &optional push)
   (let ((process-connection-type nil))
       (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
         (process-send-string proc text)
         (process-send-eof proc))))

  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx)
  )

;; scratchバッファの初期表示メッセージを出さない
(setq initial-scratch-message "")


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; 一般設定（見た目）
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; カーソルの色
(set-cursor-color "#ff00ff")



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; カラーテーマ
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package solarized-theme
  :config
  (setq x-underline-at-descent-line t) ; 
  (setq solarized-emphasize-indicators nil) ; Use less colors for indicators such as git:gutter, flycheck and similar
  
  )
(load-theme 'solarized-dark-high-contrast t)



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @rainbow-delimiters
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  (use-package rainbow-delimiters
    :hook
    (prog-mode . rainbow-delimiters-mode))



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @hydra
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package hydra)

;; ウィンドウ操作
(defun split-window-horizontally-n (num_wins)
  "任意の数だけ横に分割"
  (interactive "p")
  (dotimes (i (- num_wins 1))
    (split-window-horizontally))
  (balance-windows))

(defhydra hydra-window ()
  ("t" neotree-toggle)
  ("b" windmove-left)
  ("n" windmove-down)
  ("p" windmove-up)
  ("f" windmove-right)
  ("<left>" windmove-left)
  ("<down>" windmove-down)
  ("<up>" windmove-up)
  ("<right>" windmove-right)
  ("C-<left>" shrink-window-horizontally)
  ("C-<right>" enlarge-window-horizontally)
  ("C-<up>" shrink-window)
  ("C-<down>" enlarge-window)
  ("s" window-swap-states)
  ("-" split-window-below)
  ("\\" split-window-right)
  ("0" delete-window)
  ("1" delete-other-windows)
  ("3" (lambda ()
	 "3分割"
	 (interactive)
	 (split-window-horizontally-n 3)))
  ("4" (lambda ()
	 "4分割"
	 (interactive)
	 (split-window-horizontally-n 4)))
  ("6" (lambda ()
	 "6分割"
	 (interactive)
	 (split-window-horizontally-n 3)
	 (split-window-vertically)
	 (setq i 0)
	 (while (< i 2)
	   (windmove-right)
	   (split-window-vertically)
	   (setq i (+ 1 i))))))

(global-set-key (kbd "C-t") 'hydra-window/body)



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @multiple-cursors
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package multiple-cursors
  :bind ("C-q" . hydra-multiple-cursors/body)
  :config
  (defhydra hydra-multiple-cursors (:hint nil)
    "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search
 [Click] Cursor at point       [_q_] Quit"
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("s" mc/mark-all-in-region-regexp :exit t)
    ("0" mc/insert-numbers :exit t)
    ("A" mc/insert-letters :exit t)
    ("<mouse-1>" mc/add-cursor-on-click)
    ;; Help with click recognition in this hydra
    ("<down-mouse-1>" ignore)
    ("<drag-mouse-1>" ignore)
    ("q" nil))
  )



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @all-the-icons
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package all-the-icons
  :config
  (add-to-list 'all-the-icons-icon-alist
   		   '("\\.tsx$"
   		     all-the-icons-alltheicon "react"
   		     :height 1.0
   		     :face all-the-icons-blue)))



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @neotree
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package neotree
  :config
  (setq neo-create-file-auto-open t)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  )


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @projectile
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy))


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @ivy
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package counsel)

(use-package ivy-yasnippet)

(when (require 'ivy nil t)

  ;; M-o を ivy-hydra-read-action に割り当てる．
  (when (require 'ivy-hydra nil t)
    (setq ivy-read-action-function #'ivy-hydra-read-action))

  ;; `ivy-switch-buffer' (C-x b) のリストに recent files と bookmark を含める．
  (setq ivy-use-virtual-buffers t)

  ;; ミニバッファでコマンド発行を認める
  (when (setq enable-recursive-minibuffers t)
    (minibuffer-depth-indicate-mode 1)) ;; 何回層入ったかプロンプトに表示．

  ;; ESC連打でミニバッファを閉じる
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)

  ;; プロンプトの表示が長い時に折り返す（選択候補も折り返される）
  (setq ivy-truncate-lines nil)

  ;; リスト先頭で `C-p' するとき，リストの最後に移動する
  (setq ivy-wrap t)

  ;; アクティベート
  (ivy-mode 1))
(when (require 'counsel nil t)

  ;; キーバインドは一例です．好みに変えましょう．
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "C-M-z") 'counsel-fzf)
  (global-set-key (kbd "C-M-r") 'counsel-recentf)
  (global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
  (global-set-key (kbd "C-M-f") 'counsel-ag)

  ;; アクティベート
  (counsel-mode 1))
(when (require 'swiper nil t)

  ;; キーバインドは一例です．好みに変えましょう．
  (global-set-key (kbd "M-s M-s") 'swiper-thing-at-point))



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @yasnippet
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package yasnippet
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets)



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @company-mode
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0) ; デフォルトは0.5
  (setq company-minimum-prefix-length 2) ; デフォルトは4
  (setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
  (setq completion-ignore-case nil) ; 大文字、小文字を区別しない Emacs自体の設定
  (setq company-dabbrev-downcase nil) ; lower caseで補完で保管されるのを防ぐ
  (global-set-key (kbd "C-M-i") 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next) ;; C-n, C-pで補完候補を次/前の候補を選択
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates) ;; C-sで絞り込む
  (define-key company-active-map (kbd "C-i") 'company-complete-selection) ;; TABで候補を設
  (define-key company-active-map [tab] 'company-complete-selection) ;; TABで候補を設定
  (define-key company-active-map (kbd "C-f") 'company-complete-selection) ;; C-fでも候補を設定
  (define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete) ;; 各種メジャーモードでも C-M-iで company-modeの補完を使う


  ;; yasppetとの連携
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
	backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  )

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode))

(use-package company-statistics
  :hook (company-mode . company-statistics-mode))



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @which-key
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package which-key
  :config
  (which-key-mode))



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @expand-region
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package expand-region
  :bind (("C-o" . er/expand-region))
  )



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @lsp-mode
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(setq lsp-keymap-prefix "C-c l")
(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  ;; :config
  ;; (setq lsp-eslint-server-command 
  ;; 	'("node" 
  ;; 	  "/Users/zero.asahi/ghq/github.com/microsoft/vscode-eslint/server/out/eslintServer.js" 
  ;; 	  "--stdio"))
  )

(use-package lsp-ui :commands lsp-ui-mode)



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @fly-check
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package flycheck)


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @prettier-js
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package prettier-js)


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @smartparens
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  (sp-local-pair 'typescript-mode "<" ">")
  )


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @undo-tree
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package undo-tree
  :config
  (global-undo-tree-mode))



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; add-node-modules-path
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package add-node-modules-path)




;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @web-mode
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package web-mode
  :init
  (define-derived-mode jsx-mode web-mode "JSX"
    (setq web-mode-content-type "jsx"))

  :config
  (setq web-mode-attr-indent-offset nil)              ; 属性ごとのインデントを浅く
  (setq web-mode-markup-indent-offset 2)              ; htmlのインデント幅
  (setq web-mode-css-indent-offset 2)                 ; cssのインデント幅
  (setq web-mode-code-indent-offset 2)                ; js、php、rubyとかのインデント幅
  (setq web-mode-sql-indent-offset 2)                 ; sqlのインデント幅
  (setq indent-tabs-mode nil)                         ; インデントをタブではなくスペースに
  (setq tab-width 2)                                  ; タブの幅をスペース2つに
  (setq web-mode-script-padding 0)                    ; <script>タグ直下のインデント幅
  (setq web-mode-style-padding 0)                     ; <style>タグ直下のインデント幅
  (setq web-mode-block-padding 0)                     ; php、erbとかコードブロックのインデント幅
  (setq web-mode-enable-current-element-highlight t)  ; カーソル位置にある要素に対応するタグをハイライト
  (setq web-mode-enable-current-column-highlight t)   ; カーソル位置にある要素に対応するタグまで縦線を表示
  (setq web-mode-enable-auto-closing t)               ; <タグ名>の直後に</と自動で</タグ名>を挿入してカーソル位置は><の間に移動
  (setq web-mode-enable-auto-expanding t)             ; d/ => <div></div> c/ <div class=""></div> みたいな感じになる
  (setq web-mode-comment-style 2)                     ; pnpとかのコメントのスタイルがいい感じに

  (push '("javascript" . "//") web-mode-comment-formats)
  (push '("jsx" . "//") web-mode-comment-formats)
  )


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @jsx-mode
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(add-hook 'jsx-mode-hook #'(lambda ()
			     (lsp)
			     (add-node-modules-path)))
	  
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . jsx-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . jsx-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . jsx-mode))



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @scss-mode CSS
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(setq css-indent-offset 2)
(use-package scss-mode)







(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
