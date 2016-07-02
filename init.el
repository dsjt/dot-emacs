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

;; noneed key-binding
(global-unset-key (kbd "C-l"))        ;prefix key
(global-unset-key (kbd "C-x l"))        ;prefix key
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-\\"))
(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "C-x C-n"))
(global-unset-key (kbd "C-x m"))
(global-unset-key (kbd "<f1> h"))
(global-unset-key (kbd "<f10>"))
(global-unset-key (kbd "C-q"))

;; new key-binding
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-l c") 'compile)
(global-set-key (kbd "C-,") 'other-window-or-split)
(global-set-key (kbd "C-x C-\\") 'indent-region)
(global-set-key (kbd "C-x C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-:") 'recenter-top-bottom)
(global-set-key (kbd "C-x C-l") 'load-file)
(global-set-key (kbd "C-l C-r") 'revert-buffer)
(global-set-key (kbd "C-q M-i") 'quoted-insert)
(global-set-key (kbd "C-x C-r") 'eval-region)

;; basic configuration
(setq inhibit-startup-screen t)
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))
(defface hlline-face
  '((((class color)
      (background dark))
     (:background "gray0"))
    (((class color)
      (background light))
     (:background "SeaGreen"))
    (t
     ()))
  "used fave l-line.")
(setq hl-line-face 'hlline-face)
(global-hl-line-mode 1)
(setq gc-cons-threshold (* 10 gc-cons-threshold))
(setq-default tab-width 4 indent-tabs-mode nil) ;;tabを半角スペース2個にする。
(defalias 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(setq-default fill-column 80) ;;auto-fill
(transient-mark-mode 1)
(setq mark-ring-max 64
      kill-whole-line t
      visible-bell nil)
(ffap-bindings)
(put 'narrow-to-region 'disabled nil)
(global-visual-line-mode 1)
(setq-default line-move-visual nil)
(set-default 'cursor-type 'bar)
(add-hook 'emacs-lisp-mode 'electric-indent-mode)
;;kill-buffers
;;;###autoload
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (loop for buf in (buffer-list)
        unless (or 
                (get-buffer-window buf)
                (string= (substring (buffer-name buf) 0 1) " ")
                (get-buffer-process buf)
                (member (buffer-name buf) ;; 消さないバッファ名を指定
                        '("*Messages*" "*Compile-Log*" "*Help*"
                          "*scratch*" "*init log*")))
        do (kill-buffer buf)))
(global-set-key (kbd "C-x C-c C-c") 'kill-other-buffers)
;;;###autoload
(defun switch-scratch-buffer ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (delete-other-windows))
(global-set-key (kbd "C-l <SPC>") 'switch-scratch-buffer)
(global-set-key (kbd "C-l C-<SPC>") 'switch-scratch-buffer)
;; one-line-comment
;;;###autoload
(defun one-line-comment ()
  (interactive)
  (move-beginning-of-line 1)
  (let ((b (point)))
    (move-end-of-line 1)
    (let ((e (point)))
      (comment-or-uncomment-region b e))))
(global-set-key (kbd "C-M-;") 'one-line-comment)
;;;###autoload
(defun duplicate-line ()
  (interactive)
  (save-excursion
    (let ((cl (thing-at-point 'line)))
      (forward-line 0)
      (insert cl))))
(global-set-key (kbd "C-S-y") 'duplicate-line)
;;;###autoload
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
(global-set-key (kbd "C-c e") 'eval-and-replace)
(defalias 'exit 'save-buffers-kill-emacs)
;;;###autoload
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

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

;; org-mode
(el-get-bundle 'org)
(setq org-startup-folded nil)
(setq org-hide-leading-stars t)
(setq org-log-done 'time)
(setq org-use-fast-todo-selection nil)
(setq org-use-speed-commands t)
(smartrep-define-key
    org-mode-map "C-c" '(("p" . (outline-previous-visible-heading 1))
                         ("n" . (outline-next-visible-heading 1))
                         ("u" . (outline-up-heading 1))
                         ("f" . (org-forward-heading-same-level 1))
                         ("b" . (org-backward-heading-same-level 1))
                         ("<tab>" . (org-cycle))
                         ))
(global-set-key (kbd "C-c l") 'org-store-link)
(define-key org-mode-map (kbd "C-,") 'other-window-or-split)
(global-set-key "\C-cc" 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c C-x C-j") 'org-clock-goto)
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'org-display-inline-images)

;; org-archive
(setq org-archive-default-command 'org-archive-to-archive-sibling)
(define-key org-mode-map (kbd "C-S-n") 'org-metaright)
(define-key org-mode-map (kbd "C-S-p") 'org-metaleft)

;; org-agenda
;; http://hpcgi1.nifty.com/spen/index.cgi?OrgMode%2fOrg%2dmode%a4%c7GTD%bc%c2%c1%a9%a1%ca%cb%dd%cc%f5%a1%cb
(setq org-agenda-custom-commands
      '(("n" "Agenda and all TODO's" ((agenda "") (alltodo "")))
        ("H" "Office and Home Lists"
         ((agenda "" ((org-agenda-ndays 1)))
          (tags "Buckets")
          (todo "TODO" ((org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'all)))))
        ("D" "Daily Action List"
         ((agenda "" ((org-agenda-ndays 1)
                      ;; (org-agenda-sorting-strategy
                      ;;  (quote ((agenda time-up priority-down tag-up))))
                      (org-deadline-warning-days 0)
                      (org-agenda-log-mode 1)))))
        ("W" "Weekly Review"
         ((agenda "" ((org-agenda-ndays 1)
                      (org-deadline-warning-days 100)
                      (org-agenda-use-time-grid nil)
                      (org-agenda-todo-list-sublevels nil)))
          (todo "TODO" ((org-agenda-todo-list-sublevels t)))))
        ("o" "Originally Daily List"
         ((agenda "" ((org-agenda-ndays 3)
                      (org-deadline-warning-days 7)
                      (org-agenda-log-mode-items '(closed state))
                      (org-agenda-show-log t)))
          (tags "Buckets")))
        ("O" "Originally Daily List"
         ((agenda "" ((org-agenda-ndays 3)
                      (org-deadline-warning-days 7)
                      (org-agenda-log-mode-items '(state))
                      (org-agenda-show-log t)))
          (tags "Buckets")))))
(add-hook 'org-timer-set-hook 'org-clock-in)
(add-hook 'org-timer-done-hook 'org-clock-out)
(add-hook 'org-timer-stop-hook 'org-clock-out)

;; calender
(require 'solar)
(setq holiday-general-holidays nil
      holiday-local-holidays t
      holiday-solar-holidays nil
      holiday-bahai-holidays nil
      holiday-christian-holidays nil
      holiday-hebrew-holidays nil
      holiday-islamic-holidays nil
      holiday-oriental-holidays nil
      holiday-other-holidays nil
      mark-holidays-in-calendar t)
(setq holiday-local-holidays
      '((holiday-fixed 1 1 "元日")
        (holiday-float 1 1 2 "成人の日")
        (holiday-fixed 2 11 "建国記念の日")
        (holiday-sexp '(map 'list 'truncate (solar-equinoxes/solstices 0 year)) "春分の日")
        (holiday-fixed 4 29 "昭和の日")
        (holiday-fixed 5 3 "憲法記念日")
        (holiday-fixed 5 4 "みどりの日")
        (holiday-fixed 5 5 "こどもの日")
        (holiday-float 7 1 3 "海の日")
        (holiday-float 7 1 3 "敬老の日")
        (holiday-sexp '(map 'list 'truncate (solar-equinoxes/solstices 2 year)) "秋分の日")
        (holiday-float 10 1 2 "体育の日")
        (holiday-fixed 11 3 "文化の日")
        (holiday-fixed 11 23 "勤労感謝の日")
        (holiday-fixed 12 23 "天皇誕生日")))

;; column
(setq org-columns-default-format "%38ITEM(Details) %TAGS(Context) %7TODO(To Do) %14Effort(Time){:} %6CLOCKSUM{Total}")

;; export
;; org->latex
;; (setq org-latex-default-class "jsarticle")
;; (defvar org-latex-classes nil)
;; (setq org-latex-default-packages-alist
;;       '(("dvipdfmx" "graphicx" t)
;;         ("" "amsmath" t)
;;         ("" "amssymb" t)
;;         ("" "amsfonts" t)
;;         ))
;; (add-to-list 'org-latex-classes 
;;       '("jsarticle" "\\documentclass[a4paper,12pt,titlepage]{jsarticle}"))

;; org->html
;; (setq org-html-with-latex 'mathjax)
;; (setq org-html-with-latex 'dvipng)
;; (setq org-html-mathjax-options '((path "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML") (scale "100") (align "center") (indent "2em") (mathml nil)))
;; (setq org-export-default-language "ja")
;; (setq org-html-postamble-format
;;       '(("ja" "<!-- <p class=\"author\">Author: %a (%e)</p> -->
;; <p class=\"date\">Date: %T</p>
;; <p class=\"creator\">%c</p>
;; <p class=\"validation\">%v</p>")))
;; (setq org-html-postamble t)

;; babel
;; ob-python
(el-get-bundle 'f)
(el-get-bundle 'gregsexton/ob-ipython)
(setq org-src-preserve-indentation t)

;; ob-http
(el-get-bundle 'ob-http)
(setq org-confirm-babel-evaluate nil)
(setq org-babel-load-languages
      '((R . t)
        (C . t)
        (emacs-lisp . t)
        (sh . t)
        (gnuplot . t)
        (http . t)
        (ruby . t)))
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
;;;###autoload
(defun my/toggle-yatex-mode-temporarily ()
  (interactive)
  (cond ((not (eq major-mode 'yatex-mode))
         (yatex-mode))
        (t (normal-mode))))
(global-set-key (kbd "C-c Y") 'my/toggle-yatex-mode-temporarily)

(setq org-latex-with-hyperref nil)
(setq org-latex-pdf-process '("platex %f" "dvipdfmx %b.dvi"))

;; org-external
(setq org-file-apps '(("\\.rd\\'" . emacs)
                      ("\\.pdf\\'" . evince)
                      (auto-mode . emacs)
                      ("\\.mm\\'" . default)
                      ("\\.x?html?\\'" . default)))
(setq org-link-file-path-type 'relative)
(defvar pdf-viewer "evince")
;;;###autoload
(defun view-pdf()
  (interactive)
  (let ((pdf-file (concat (car (split-string (buffer-file-name) "\\.")) ".pdf"))
        (viewer pdf-viewer))
    (when (file-exists-p pdf-file)
      (start-process "*view-pdf*" nil viewer pdf-file))))
(define-key org-mode-map (kbd "C-\\ p") 'view-pdf)

;; smartparen
(el-get-bundle 'smartparens)
(smartparens-global-mode 1)
(smartparens-global-strict-mode -1)
(setq sp-highlight-pair-overlay nil)
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
(sp-use-paredit-bindings)               ;(sp-use-smartparens-bindings)
;; (electric-pair-mode nil)

;; expand
(global-set-key (kbd "C-;") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))
 
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

;; jedi
(el-get-bundle 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'jedi:ac-setup)
(setq jedi:complete-on-dot t)

;; clang-format
(require 'clang-format)
(setq clang-format-executable "clang-format-3.5")
(set-default 'clang-format-style "{BasedOnStyle: Google, IndentWidth: 4, Standard: C++11}")

;; restart-eamcs
(el-get-bundle 'restart-emacs)

(load-file "~/.emacs.d/private.el")
