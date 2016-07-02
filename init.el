(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; server
(require 'server)
(server-start)

;; basic configuration
(load-file "~/.emacs.d/config/common.el")

;; helm
(el-get-bundle 'helm)
(helm-mode 1)
(setq helm-completing-read-handlers-alist
      (append helm-completing-read-handlers-alist
              '((ffap . nil)
                (dired-create-directory . nil)
                (howm-list-grep-fixed . nil))))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-X") 'execute-extended-command)
(global-set-key (kbd "C-x C-f") 'ffap)
(global-set-key (kbd "C-c i") 'helm-imenu)
(global-set-key (kbd "C-M-c") 'helm-resume)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "<f1> h") 'helm-apropos)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-unset-key (kbd "M-."))
(global-set-key (kbd "M-.") 'helm-etags-select)
(global-set-key (kbd "M-/") 'helm-dabbrev)
(setq helm-ff-skip-boring-files t)

;; helm-ag
(el-get-bundle 'helm-ag)
(global-set-key (kbd "C-l g") 'helm-ag)

;; recentf
(require 'recentf)
(setq recentf-auto-cleanup 'never)
(setq recentf-max-saved-items 10000)
(el-get-bundle 'recentf-ext)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'helm-recentf)

;; sequential-command
(el-get-bundle 'elpa:sequential-command)
(sequential-command-setup-keys)

;;color
(add-to-list 'custom-theme-load-path "~/.emacs.d/theme/")
(global-set-key (kbd "<f1>C-d") 'describe-face)
(load-theme 'charcoal-black t)
(set-cursor-color "white")

;; smartrep  ;; 使っているので注意
(el-get-bundle! 'smartrep)

;; expand-region
(el-get-bundle 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)
(global-set-key (kbd "C-M-@") 'er/contract-region)

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;; anzu
(el-get-bundle 'anzu)
(global-anzu-mode 1)
;; isearchの数を出してくれるだけで十分
;; (global-set-key (kbd "C-x q") 'anzu-query-replace) 
;; (global-set-key (kbd "C-x Q") 'anzu-query-replace-regexp)
;; (global-set-key (kbd "C-x M-q") 'anzu-query-replace-at-cursor)

;; visual-regexp
(el-get-bundle 'visual-regexp)
(setq vr/default-replace-preview t)
(global-unset-key (kbd "C-x q"))
(global-set-key (kbd "C-x q") 'vr/query-replace)
(global-set-key (kbd "C-S-c m") 'vr/mc-mark)

;; point-undo
(el-get-bundle! 'point-undo)
(global-set-key (kbd "C-.") 'point-undo)
(global-set-key (kbd "C-M-.") 'point-redo)
(global-set-key (kbd "<f7>") 'point-undo)
(global-set-key (kbd "S-<f7>") 'point-redo)

;; undo-tree
(el-get-bundle 'undo-tree)
(setq undo-no-redo nil)
(setq undo-limit 600000)
(setq undo-strong-limit 900000)
(global-undo-tree-mode 1)
(global-set-key (kbd "C-M-/") 'undo-tree-redo)

;; helm-swoop
(el-get-bundle 'helm-swoop)
(global-set-key (kbd "M-i") 'helm-swoop)

;; dired
(el-get-bundle 'direx)
(global-set-key (kbd "C-x d") 'direx:jump-to-directory)
(el-get-bundle 'dired+)
(setq dired-dwim-target t)
(global-dired-hide-details-mode -1)
(setq delete-by-moving-to-trash t)
;; diredで開いたpdfをrecentfに追加するための設定．
(defadvice dired-find-file (before add-recentf)
  (let ((file (dired-filename-at-point)))
    (when file
      (recentf-add-file file))))
(ad-activate 'dired-find-file)

;; dired-hacks
(el-get-bundle 'dired-hacks)
(require 'dired-filter)
(setq dired-filter-group-saved-groups
      '(("default"
         ("PDF"
          (extension . "pdf"))
         ("LaTeX"
          (extension "tex" "bib"))
         ("Org"
          (extension . "org"))
         ("Archives"
          (extension "zip" "rar" "gz" "bz2" "tar")))))
;; (require 'dired-open)
;; (setq dired-open-extensions
;;       '(("pdf" . "evince")))
;; ("wav" . "audacious")
;; ("mp3" . "audacious")
;; ("ogg" . "audacious")
;; ("png" . "eog")
;; ("jpg" . "eog")
;; ("jpg-large" . "eog")
;; ("gif" . "eog")
;; ("bmp" . "eog")

;; junk-file
(el-get-bundle 'open-junk-file)
(global-set-key (kbd "C-x f") 'open-junk-file)

;; ess
(el-get-bundle 'elpa:ess)
(add-to-list 'load-path "~/.emacs.d/el-get/ess/lisp")
(require 'ess-site)

;; yatex
(el-get-bundle 'elpa:yatex)
(setq YaTeX-kanji-code nil)
(setq YaTeX-coding-system nil)
(add-to-list' auto-mode-alist
              (cons "\\.tex$" 'yatex-mode))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq YaTeX-fill-column 80)
(setq YaTeX-latex-message-code 'utf-8)

;; org
(load-file "~/.emacs.d/config/my-org.el")

;; smartparen
(el-get-bundle 'smartparens)
(smartparens-global-mode 1)
;; (smartparens-global-strict-mode 1)
(setq sp-highlight-pair-overlay nil)
(setq sp-highlight-wrap-overlay nil)
(sp-use-paredit-bindings)               ;(sp-use-smartparens-bindings)
;; (electric-pair-mode nil)
;; paredit 保留中
;; (el-get-bundle 'paredit)
;; (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
;; (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
;; (add-hook 'lisp-mode-hook 'enable-paredit-mode)
;; (add-hook 'ielem-mode-hook 'enable-paredit-mode)
;; (add-hook 'scheme-mode-hook 'enable-paredit-mode)

;; expand
(require 'hippie-exp)
;; (eval-after-load "hippie-exp"
;;   '(setq hippie-expand-try-functions-list
;;          '(try-expand-all-abbrevs
;;            try-expand-dabbrev
;;            try-expand-dabbrev-all-buffers
;;            try-expand-dabbrev-from-kill
;;            try-complete-file-name-partially
;;            try-complete-file-name
;;            try-complete-lisp-symbol-partially
;;            try-complete-lisp-symbol)))
(global-set-key (kbd "C-;") 'hippie-expand)

;; scroll 
(el-get-bundle 'yascroll)
(global-yascroll-bar-mode 1)
(setq scroll-conservatively 20)
(setq scroll-margin 5)
(setq scroll-step 1)
(setq next-screen-context-lines 20)

;; howm
(global-unset-key (kbd "C-q"))
(setq howm-prefix "\C-q"
      howm-view-title-header "*"
      howm-menu-lang 'ja
      howm-keyword-case-fold-search t)
(el-get-bundle! 'howm) ; '上との順序，重要なので変更しない
(setq howm-list-recent-title t                       ;; 「最近のメモ」一覧時にタイトル表示
      howm-list-all-title t                          ;; 全メモ一覧時にタイトル表示
      howm-menu-expiry-hours 2                       ;; メニューを 2 時間キャッシュ
      howm-menu-schedule-days-before 10              ;; 10 日前から
      howm-menu-schedule-days 7                      ;; 3 日後まで
      howm-file-name-format "%Y/%m/%Y-%m-%d-%H.howm" ;; howm のファイル名
      howm-view-grep-parse-line
      "^\\(\\([a-zA-Z]:/\\)?[^:]*\\.howm\\):\\([0-9]*\\):\\(.*\\)$"
      howm-excluded-file-regexp
      "/\\.#\\|[~#]$\\|\\.bak$\\|/CVS/\\|\\.doc$\\|\\.pdf$\\|\\.ppt$\\|\\.xls$\\|\\.html$\\|\\.png$\\|\\.gif$\\|\\.jpg$"
      howm-menu-refresh-after-save nil
      howm-view-summary-persistent nil
      howm-template "* %cursor\n"
      howm-template-file-format "[[%s]]"
      howm-view-use-grep t
      howm-menu-recent-num 10
      howm-list-recent-days 20)
(add-to-list 'auto-mode-alist '("\\.howm$" . org-mode))
(add-hook 'org-mode-hook 'howm-mode)
(set-face-attribute 'howm-mode-title-face nil :foreground nil)
(set-face-attribute 'howm-reminder-today-face nil :foreground nil :background "#2d37aa" :box nil)
(set-face-attribute 'howm-reminder-tomorrow-face nil :foreground nil :background "#2d4900" :box nil)
(define-key org-mode-map (kbd "C-c C-7") 'org-edit-special)
(define-key org-src-mode-map (kbd "C-c C-7") 'org-edit-src-exit)

;; face
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-keyword-face ((t (:foreground "lime green" :weight bold))))
 '(helm-selection ((t (:background "dark slate gray" :underline t))))
 '(howm-mode-title-face ((t nil)))
 '(org-agenda-date ((t (:inherit org-agenda-structure :box (:line-width 3 :color "dim gray" :style released-button)))) t)
 '(org-done ((t (:strike-through "black" :weight bold))))
 '(org-todo ((t (:foreground "dark green" :weight bold)))))
(set-face-attribute 'default nil :family "IPAGothic" :height 120) ; height は、30の倍数でないと全角半角にぶれ．org-tableで不便

;; yasnippet
(el-get-bundle 'yasnippet)
(yas-global-mode 1)
(global-unset-key (kbd "C-x i"))
(global-set-key (kbd "C-x i i") 'yas-insert-snippet)
(global-set-key (kbd "C-x i v") 'yas-visit-snippet-file)
(global-set-key (kbd "C-x i n") 'yas-new-snippet)

;; popwin
(el-get-bundle! 'popwin)
(popwin-mode 1)

;; google-translate
(el-get-bundle 'google-translate)
(defun google-en-to-ja ()
  (interactive)
  (google-translate-translate "en" "ja"
                              (if (use-region-pq)
                                  (buffer-substring-no-properties (region-beginning)
                                                                  (region-end))
                                (or (current-word t t)
                                    (error "No word at point.")))))
(global-set-key (kbd "C-l e") 'google-en-to-ja)


;; rotate
(el-get-bundle 'rotate)
(smartrep-define-key
    global-map "C-l" '(("w" . (rotate-window))
                       ("l" . (rotate-layout))))
(smartrep-define-key
    global-map "C-l" '(("{" . (shrink-window-horizontally 2))
                       ("}" . (enlarge-window-horizontally 2))))

;; jword
(el-get-bundle 'jaword)

;; c言語
(setq c-hungry-delete-key nil)
(add-hook 'c++-mode-hook 'my/electric-indent-mode-on)
(add-hook 'c-mode-hook 'my/electric-indent-mode-on)
;; (electric-indent-mode -1

;;;for next-line
;;;###autoload
(defun add-new-line-above (N)
  "Adds new line N times above and indent."
  (interactive "p")
  (add-new-line-below (1+ (- N))))
;;;###autoload
(defun add-new-line-below (N)
  "Add new line N times below and indent."
  (interactive "p")
  (setq N (or N 1))
  (end-of-line N)
  (comment-indent-new-line))
(global-set-key (kbd "C-o") 'add-new-line-below)
(global-set-key (kbd "M-o") 'add-new-line-above)

;; folding
(add-hook 'html-mode-hook 'hs-minor-mode)
(add-hook 'lisp-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'python-mode-hook 'hs-minor-mode)
(define-key hs-minor-mode-map (kbd "C-^") 'hs-toggle-hiding)
(define-key hs-minor-mode-map (kbd "C-M-^") 'hs-hide-all)
(define-key hs-minor-mode-map (kbd "C-M-~") 'hs-show-all)
(define-key hs-minor-mode-map (kbd "C-l ^") '(lambda () (interactive) (hs-hide-level 2)))

;; external program utility
;;;###autoload
(defun start-nautilus ()
  (interactive)
  (let ((dd (expand-file-name default-directory)))
    (async-start-process "nautilus" "nautilus" 'ignore dd)))
(global-set-key (kbd "C-\\ e") 'start-nautilus)
;;;###autoload
(defun start-gnome-terminal ()
  (interactive)
  (let ((dd (expand-file-name default-directory)))
    (async-start-process "gnome-terminal" "gnome-terminal" 'ignore default-directory)))
(global-set-key (kbd "C-\\ c") 'start-gnome-terminal)
(global-set-key (kbd "C-\\ E") 'eshell)
(global-set-key (kbd "C-\\ M-e") 'my/popup-eshell)
(defun my/popup-eshell (arg)
  (interactive "p")
  (let (eb)
    (save-window-excursion
      (setq eb (eshell arg)))
    (popwin:popup-buffer-tail eb)))
(global-set-key (kbd "C-\\ 9") 'popwin:stick-popup-window)

;; inhibit-splash-screen
(defun my/insert-emacs-init-time-in-scratch ()
  (interactive)
  (with-output-to-temp-buffer "*information*"
    (let ((str (format "Emacs init time: %s\n\n\n" (emacs-init-time))))
      (princ str))))
(add-hook 'after-init-hook 'my/insert-emacs-init-time-in-scratch)

;; window
(winner-mode 1)
;; 使えない
(smartrep-define-key
    global-map "C-c" '(("<left>" . (winner-undo))
                       ("<right>" . (winner-redo))))

;; multiple-cursors
(el-get-bundle 'multiple-cursors)
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-S-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c n") 'mc/insert-numbers)
(global-set-key (kbd "C-S-c s") 'mc/sort-regions)
(global-set-key (kbd "C-S-c r") 'mc/reverse-regions)
(global-set-key (kbd "C-S-c C-S-s") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-S-c C-S-m") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-S-c C-S-p") 'mc/skip-to-previous-like-this)
(global-set-key (kbd "C-S-c C-S-n") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-S-c C-S-e") 'mc/mark-more-like-this-extended)

;; mouse
(mouse-avoidance-mode 'exile)

;; calfw
(el-get-bundle 'calfw)
(require 'calfw-org)
(global-set-key (kbd "C-c M-c") 'cfw:open-org-calendar)

;; magit
(el-get-bundle 'magit)
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")

;;;###autoload
(defun my/copy-current-file-name ()
  (interactive)
  (kill-new (buffer-file-name) nil)
  (message "kill new '%s'" (buffer-file-name)))
(global-set-key (kbd "C-l C-w p") 'my/copy-current-file-name)

;; savehist
(savehist-mode 1)

;; saveplace
(setq-default save-place t)
(require 'saveplace)
(setq save-place-file "~/.emacs.d/saved-places")

;; openwith
(el-get-bundle 'openwith)
(setq openwith-associations
      '(("\\.pdf\\'" "evince" (file))
        ;; ("\\.\\(?:mp3\\|wav\\|ogg\\)\\'" "mplayer" (file))
        ;; ("\\.\\(?:png\\|jpg\\|jpg-large\\|gif\\|bmp\\)\\'" "display" (file)) ;eogからdisplayへ変更
        ;; ("\\.ods\\'" "libreoffice" (file))
        ))

;; number
(el-get-bundle 'number)
(global-set-key (kbd "C-c C-+") 'number/add)
(global-set-key (kbd "C-c C--") 'number/sub)
(global-set-key (kbd "C-c C-*") 'number/multiply)
(global-set-key (kbd "C-c C-/") 'number/divide)
(global-set-key (kbd "C-c C-0") 'number/pad)
(global-set-key (kbd "C-c C-=") 'number/eval)

;; backup
(add-to-list 'backup-directory-alist '(("\\.*") . "~/.emacs.d/backup/"))
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/backup"))
            backup-directory-alist))

;;; python
(require 'python)
;;;###autoload
(defun python-shell-send-line()
  "send current line to python shell."
  (interactive)
  (let* ((clbeg (line-beginning-position))
         (clend (line-end-position)))
    (python-shell-send-region clbeg clend)))
(define-key python-mode-map (kbd "C-c C-u") 'python-shell-send-line)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(setq python-shell-interpreter "python3")
(setq python-shell-interpreter-args "-i")

;; quickrun
(el-get-bundle 'quickrun)
(global-set-key (kbd "C-l r") 'quickrun)

;; gnuplot
(el-get-bundle 'gnuplot-mode)

;; scheme
(setq scheme-program-name "jakld")

;; avy  ,ace-jump-mode の後継
(el-get-bundle 'avy)
(global-set-key (kbd "C-M-j") 'avy-goto-char)
(el-get-bundle 'ace-jump-mode)
(load-file "~/.emacs.d/site-lisp/ace-pinyin-myconf.el")
(smartrep-define-key
    global-map "C-l" '(("[" . (backward-paragraph))
                       ("]" . (forward-paragraph))))

;; auto-complete
(el-get-bundle 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode 1)
(add-to-list 'ac-modes 'YaTeX-mode)

;; Standard Jedi.el setting
(el-get-bundle 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'jedi:ac-setup)
(setq jedi:complete-on-dot t)

;; clang-format
(require 'clang-format)
(setq clang-format-executable "clang-format-3.5")
(set-default 'clang-format-style "{BasedOnStyle: Google, IndentWidth: 4, Standard: C++11}")

;; org-babel python
(el-get-bundle 'f)
(el-get-bundle 'gregsexton/ob-ipython)
(require 'ob-ipython)
(require 'ob-python)
(setq org-src-preserve-indentation t)

;; restart-eamcs
(el-get-bundle 'restart-emacs)

(load-file "~/.emacs.d/private.el")
