;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(when (eq system-type 'darwin)
  (setq mac-command-modifier (quote meta))
  (setq mac-option-modifier (quote super)))

;; El-Get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(require 'info)
(add-to-list 'Info-additional-directory-list "/Users/admin/.emacs.d/el-get/el-get/")
(setq el-get-bundle-byte-compile nil)

;; Server
(require 'server)
(server-start)

;; A Part Of Key-Bindings
(global-unset-key (kbd "C-l"))          ;prefix key
(global-unset-key (kbd "C-x l"))        ;prefix key
(global-unset-key (kbd "C-\\"))
(global-unset-key (kbd "C-q"))
(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-x C-n"))
(global-unset-key (kbd "C-x m"))
(global-unset-key (kbd "<f1> h"))
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-l c") 'compile)
(global-set-key (kbd "C-x C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-:") 'recenter-top-bottom)
(global-set-key (kbd "C-l C-r") 'revert-buffer)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Basic Configuration
(progn 
  (setq inhibit-startup-screen t)
  (show-paren-mode 1)
  (setq show-paren-style 'parenthesis)
  (setq frame-title-format
        (format "%%f - Emacs@%s" (system-name)))
  (global-hl-line-mode)
  (setq gc-cons-threshold (* 10 gc-cons-threshold))
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (column-number-mode 1)
  (setq-default fill-column 80)
  (transient-mark-mode 1)
  (setq mark-ring-max 64
        kill-whole-line t
        visible-bell nil
        ring-bell-function 'ignore)
  (ffap-bindings)
  (put 'narrow-to-region 'disabled nil)
  (global-visual-line-mode 1)
  (setq-default line-move-visual nil)
  (set-default 'cursor-type 'bar)
  (mouse-avoidance-mode 'exile)
  (add-hook 'emacs-lisp-mode 'electric-indent-mode)
  (savehist-mode 1)
  (setq-default save-place t)
  (require 'saveplace)
  (setq save-place-file "~/.emacs.d/saved-places"))
(global-set-key (kbd "C-l <SPC>") (lambda ()
                                    (interactive)
                                    (switch-to-buffer (get-buffer-create "*scratch*"))
                                    (delete-other-windows)))
(global-set-key (kbd "C-M-;") 'comment-line)
(defalias 'exit 'save-buffers-kill-emacs)
(global-set-key (kbd "C-,") (lambda ()
                              (interactive)
                              (when (one-window-p)
                                (split-window-horizontally))
                              (other-window 1)))

;; Smartrep
(el-get-bundle! 'smartrep)

;; Org-Mode
(el-get-bundle! 'org
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c C-x C-j") 'org-clock-goto))
(with-eval-after-load 'org
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
                           ("<tab>" . (org-cycle))))
  (setq org-global-properties
        '(("Effort_ALL" . "00:10 00:20 00:30 01:00 01:30 02:00 02:30 03:00")))
  (define-key org-mode-map (kbd "C-,") 'other-window-or-split)
  (define-key org-mode-map (kbd "C-S-n") 'org-metaright)
  (define-key org-mode-map (kbd "C-S-p") 'org-metaleft)  (add-hook 'org-mode-hook 'turn-on-font-lock)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'org-display-inline-images)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0)))
(require 'org-agenda)
(with-eval-after-load 'org-agenda
  (setq org-agenda-custom-commands
        '(("n" "Agenda and all TODO's" ((agenda "") (alltodo "")))
          ("D" "1 day agenda"
           ((agenda "TODO" ((org-agenda-ndays 1)
                            (org-agenda-prefix-format '((agenda . "     %s %-8 e")))
                            ))))
          ("d" "4 days agenda"
           ((agenda "TODO" ((org-agenda-ndays 4)
                            (org-agenda-start-day "-1")
                            (org-agenda-prefix-format '((agenda . "     %s %-8 e")))
                            ;; (org-agenda-show-log t)
                            ;; (org-agenda-log-mode-items '(closed state))
                            ))
            (tags "buckets")))
          ("w" "8 days agenda"
           ((agenda "TODO" ((org-agenda-ndays 8)
                            (org-agenda-start-day "-1")
                            (org-agenda-prefix-format '((agenda . "     %s %-8 e")))
                            (org-agenda-columns-add-appointments-to-effort-sum t)
                            ))))))
  (setq org-agenda-columns-add-appointments-to-effort-sum t)
  (add-hook 'org-timer-set-hook 'org-clock-in)
  (add-hook 'org-timer-done-hook 'org-clock-out)
  (add-hook 'org-timer-stop-hook 'org-clock-out)
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
          (holiday-fixed 12 23 "天皇誕生日"))))
(with-eval-after-load 'org-columns
  (setq org-columns-default-format
        "%50ITEM{Task} %TODO %8EFFORT{:} %6CLOCKSUM_T{Total}"))
(progn
  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-load-languages
        '((R . t)
          (C . t)
          (emacs-lisp . t)
          (sh . t)
          (gnuplot . t)
          (http . t)
          (ruby . t)
          (python . t)
          (dot . t)))
  (setq org-confirm-babel-evaluate nil)
  (define-key org-mode-map (kbd "C-c C-7") 'org-edit-special)
  (define-key org-src-mode-map (kbd "C-c C-7") 'org-edit-src-exit)
  (setq org-src-window-setup 'other-window)
  (setq org-image-actual-width '(256)))  
(el-get-bundle 'f)
(el-get-bundle! 'gregsexton/ob-ipython)
(autoload 'org-babel-execute:python "ob-python.el")
(with-eval-after-load 'ob-pytho
  (setq org-babel-python-command "python3")
  (setq org-src-preserve-indentation t)
  (setq org-babel-default-header-args:python '((:session . "my_session")
                                               (:results . "none")
                                               (:tangle . "yes"))))
(autoload 'org-babel-execute:sh "ob-sh.el")
(with-eval-after-load 'org-link
  (setq org-file-apps '(("\\.rd\\'" . emacs)
                        ("\\.pdf\\'" . evince)
                        (auto-mode . emacs)
                        ("\\.mm\\'" . default)
                        ("\\.x?html?\\'" . default)))
  (setq org-link-file-path-type 'relative))
(require 'org-archive)
(with-eval-after-load 'org-archive
  (setq org-archive-default-command 'org-archive-to-archive-sibling)
  (setq org-archive-location
        (concat "%s_archive_"
                (format-time-string "%Y::" (current-time)))))
;; Calfw
(el-get-bundle 'calfw
  (global-set-key (kbd "C-c M-c") 'cfw:open-org-calendar))
(require 'calfw-org)

;; Utility
(el-get-bundle 'restart-emacs)
(el-get-bundle! 'helm
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
  (global-set-key (kbd "M-/") 'helm-dabbrev))
(with-eval-after-load 'helm
  (setq helm-completing-read-handlers-alist
        (append helm-completing-read-handlers-alist
                '((ffap . nil)
                  (dired-create-directory . nil)
                  (howm-list-grep-fixed . nil))))
  (setq helm-ff-skip-boring-files t))
(helm-mode)
(require 'recentf nil t)
(setq recentf-auto-cleanup 'never
      recentf-max-saved-items 10000)
(el-get-bundle 'recentf-ext
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z") 'helm-recentf))
(el-get-bundle elpa:sequential-command)
(require 'sequential-command-config)
(sequential-command-setup-keys)

;; Visual
(add-to-list 'custom-theme-load-path "~/.emacs.d/theme/")
(global-set-key (kbd "<f1>C-d") 'describe-face)
(el-get-bundle solarized-emacs)
(add-to-list 'custom-theme-load-path "~/.emacs.d/el-get/solarized-emacs/")
(setq solarized-scale-org-headlines nil)
(load-theme 'solarized-dark t)
(if (eq system-type 'darwin)
    (set-face-attribute 'default nil :family "IPAGothic" :height 160)
 (set-face-attribute 'default nil :family "IPAGothic" :height 120))
(set-fontset-font t 'japanese-jisx0208
                  (font-spec :family "Hiragino Kaku Gothic ProN"
                             :size 16))
(add-to-list 'face-font-rescale-alist
             '((".*-Hiragino Kaku Gothic ProN-.*" . 1.2)))
(el-get-bundle rainbow-mode)
;;;###autoload
(defun set-alpha (alpha-num)
  "set frame parameter 'alpha"
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha (cons alpha-num '(90))))

;; Region
(el-get-bundle expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)
(global-set-key (kbd "C-M-@") 'er/contract-region)

;; Search
(el-get-bundle 'helm-ag
  (global-set-key (kbd "C-l g") 'helm-ag))
(el-get-bundle anzu
  (global-anzu-mode 1))
(el-get-bundle! 'visual-regexp
  (global-unset-key (kbd "C-x q"))
  (global-set-key (kbd "C-x q") 'vr/query-replace)
  (global-set-key (kbd "C-S-c m") 'vr/mc-mark))
(with-eval-after-load 'visual-regexp
  (setq vr/default-replace-preview t)
  (setq case-replace nil))

;; Move
(el-get-bundle point-undo)
(require 'point-undo)
(global-set-key (kbd "C-.") 'point-undo)
(global-set-key (kbd "C-M-.") 'point-redo)
(global-set-key (kbd "<f7>") 'point-undo)
(global-set-key (kbd "S-<f7>") 'point-redo)
(el-get-bundle 'helm-swoop
  (global-set-key (kbd "M-i") 'helm-swoop))
(el-get-bundle! bbatsov/crux
  (global-set-key (kbd "C-o") 'crux-smart-open-line)
  (global-set-key (kbd "M-o") 'crux-smart-open-line-above)
  (global-set-key (kbd "C-S-y") 'crux-duplicate-current-line-or-region)
  (global-set-key (kbd "C-c e") 'crux-eval-and-replace)
  (global-set-key (kbd "C-l d") 'crux-delete-file-and-buffer)
  (global-set-key (kbd "C-x C-c C-c") 'crux-kill-other-buffers))
(el-get-bundle! 'avy
  (global-set-key (kbd "C-M-j") 'avy-goto-char))
(el-get-bundle 'ace-jump-mode
  ;; (load-file "~/.emacs.d/site-lisp/ace-pinyin-myconf.el")
  (smartrep-define-key
      global-map "C-l" '(("[" . (backward-paragraph))
                         ("]" . (forward-paragraph)))))
(el-get-bundle 'bm
  (global-set-key (kbd "<C-f2>") 'bm-toggle)
  (global-set-key (kbd "<f2>")   'bm-next)
  (global-set-key (kbd "<S-f2>") 'bm-previous))

;; Buffer
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-ignore-buffers-re "*[^*]+*")
;; Window
(winner-mode 1)

;; Undo
(el-get-bundle 'undo-tree)
(global-undo-tree-mode 1)
(global-set-key (kbd "C-M-/") 'undo-tree-redo)
(with-eval-after-load 'undo-tree
  (setq undo-no-redo nil
        undo-limit 600000
        undo-strong-limit 900000))


;; Dired
(el-get-bundle 'dired+)
(setq delete-by-moving-to-trash t)
(when (eq system-type 'darwin)
  (setq trash-directory "~/.Trash"))
(with-eval-after-load 'dired
  (setq dired-dwim-target t)
  (put 'dired-find-alternate-file 'disabled nil)
  ;; diredで開いたpdfをrecentfに追加するための設定．
  (defadvice dired-find-file (before add-recentf)
    (let ((file (dired-filename-at-point)))
      (when file
        (recentf-add-file file))))
  (ad-activate 'dired-find-file))
(el-get-bundle 'dired-hacks)
(el-get-bundle 'f)
(require 'dired-filter)
(with-eval-after-load 'dired-filter
  (setq dired-filter-group-saved-groups
        '(("default"
           ("PDF"      (extension  "pdf"))
           ("LaTeX"    (extension "tex" "bib"))
           ("Org"      (extension  "org"))
           ("Archives" (extension "zip" "rar" "gz" "bz2" "tar"))
           ("python"   (extension "py"))
           ("cpp"      (extension "cpp"))
           ("h"        (extension "h"))
           ("hpp"      (extension "hpp"))
           ("sh"       (extension "sh")))))
  (add-hook 'dired-mode-hook 'dired-filter-group-mode 1)
  (define-key dired-mode-map (kbd ")") #'dired-filter-group-mode))
;; my/dired-config
;;;###autoload
(defun my/dired-config()
  (dired-filter-group-mode 1))
(add-hook 'dired-mode-hook 'my/dired-config)
(el-get-bundle 'openwith)
(with-eval-after-load 'openwith
  (if (eq system-type 'darwin)
      (progn (setq openwith-associations
                   '(("\\.pdf\\'" "open" (file))
                     ("\\.xlsx\\'" "open" (file))
                     ("\\.eps\\'" "open" (file))
                     ("\\.jpg\\|\\.png\\'" "open" (file)))))
    ;; (setq openwith-associations
    ;;       (remove '("\\.jpg\\|\\.png\\'" "open" (file)) openwith-associations))
    ;; (add-to-list 'openwith-associations '("\\.jpg\\|\\.png\\'" "open" (file))))
    ;; (setq openwith-associations
    ;;       '(("\\.pdf\\'" "open" (file))
    ;;         ("\\.xlsx\\'" "open" (file))
    ;;         ("\\.eps\\'" "open" (file))))
    (add-to-list 'openwith-associations ("\\.pdf\\'" "evince" (file)))))

;; Junk-File
(el-get-bundle 'open-junk-file
  (global-set-key (kbd "C-x f") 'open-junk-file))

;; Latex
(el-get-bundle elpa:yatex)
(require 'yatex)
(add-to-list 'auto-mode-alist
             '("\\.tex\\'" . yatex-mode))

;; Parenthesis
(el-get-bundle 'smartparens)
(require 'smartparens)
(with-eval-after-load 'smartparens
  (smartparens-global-mode 1)
  (smartparens-global-strict-mode -1)
  (setq sp-highlight-pair-overlay nil)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'scheme-mode "'" nil :actions nil)
  (sp-local-pair 'inferior-scheme-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "\"" nil :actions nil)
  (sp-local-pair 'org-mode "\"" nil :actions nil)
  (sp-local-pair 'org-mode "$" "$")
  (sp-local-pair 'org-mode "\"" "\"")
  (sp-use-paredit-bindings)
  (electric-pair-mode t))

;; Complete
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
;; auto-complete
(el-get-bundle auto-complete-config in auto-complete)
(with-eval-after-load 'auto-complete-config
  (ac-config-default)
  (global-auto-complete-mode 1)
  (add-to-list 'ac-modes 'YaTeX-mode)
  (setq ac-auto-start 4
        ac-auto-show-menu 0.8
        ac-use-comphist t
        ac-candidate-limit nil
        ac-use-quick-help nil
        ac-use-menu-map t)
  (define-key ac-completing-map (kbd "<tab>") 'nil)
  (define-key ac-completing-map (kbd "M-/")   'ac-stop)
  (define-key ac-completing-map (kbd "RET") nil)
  (with-eval-after-load 'yasnippet
    (setf (symbol-function 'yas-active-keys)
          (lambda ()
            (remove-duplicates
             (mapcan #'yas--table-all-keys (yas--get-snippet-tables)))))))
;; Snippet
(el-get-bundle yasnippet
  (global-unset-key (kbd "C-x i"))
  (global-set-key (kbd "C-x i v") 'yas-visit-snippet-file)
  (global-set-key (kbd "C-x i n") 'yas-new-snippet))
(yas-global-mode 1)
(with-eval-after-load 'yasnippet
  (yas-load-directory "~/.emacs.d/snippets/"))
(el-get-bundle emacs-jp/helm-c-yasnippet
  (global-set-key (kbd "C-x i i") 'helm-yas-complete))
(with-eval-after-load 'helm-c-yasnippet
  (setq helm-yas-space-match-any-greedy t))

;; Scroll 
(el-get-bundle 'yascroll)
(global-yascroll-bar-mode 1)
(setq scroll-conservatively 20)
(setq scroll-margin 5)
(setq scroll-step 1)
(setq next-screen-context-lines 20)

;; Howm
(global-unset-key (kbd "C-q"))
(setq howm-prefix "\C-q"
      howm-view-title-header "*"
      howm-menu-lang 'ja
      howm-keyword-case-fold-search t)
(el-get-bundle 'howm)                  ; 上との順序，重要なので変更しない
(require 'howm)
(with-eval-after-load 'howm
  (setq howm-list-recent-title t)
  (setq howm-list-all-title t)
  (setq howm-menu-expiry-hours 2)
  (setq howm-menu-schedule-days-before 10)
  (setq howm-menu-schedule-days 7)
  (setq howm-file-name-format "%Y/%m/%d-%H%M%S.org")
  ;; (setq howm-view-grep-parse-line
  ;;       "^\\(\\([a-zA-Z]:/\\)?[^:]*\\.howm\\):\\([0-9]*\\):\\(.*\\)$")
  ;; howm-excluded-file-regexp
  ;;        "/\\.#\\|[~#]$\\|\\.bak$\\|/CVS/\\|\\.doc$\\|\\.pdf$\\|\\.ppt$\\|\\.xls$\\|\\.html$\\|\\.png$\\|\\.gif$\\|\\.jpg$\\|\\.h5$")
  (setq howm-menu-refresh-after-save nil)
  (setq howm-view-summary-persistent nil)
  (setq howm-template "* %cursor\n")
  (setq howm-template-file-format "[[%s]]")
  (setq howm-view-use-grep t)
  (setq howm-menu-recent-num 10)
  (setq howm-list-recent-days 20)
  (add-to-list 'auto-mode-alist '("\\.howm$" . org-mode))
  (add-hook 'org-mode-hook 'howm-mode)
  (set-face-attribute 'howm-mode-title-face nil :foreground nil)
  (set-face-attribute 'howm-reminder-today-face nil :foreground nil :background "#2d37aa" :box nil)
  (set-face-attribute 'howm-reminder-tomorrow-face nil :foreground nil :background "#2d4900" :box nil)
  (setq howm-view-grep-file-stdin-option nil) ;なぜか必要?
  )

;; Translate
(el-get-bundle! popwin)
(popwin-mode 1)
(el-get-bundle! 'google-translate)
;;;###autoload
(defun google-en-to-ja ()
  (interactive)
  (google-translate-translate "en" "ja"
                              (if (use-region-p)
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

;; C Language
(setq-default c-hungry-delete-key nil)
(add-hook 'c++-mode-hook 'electric-indent-mode)
(add-hook 'c-mode-hook 'electric-indent-mode)
;;;###autoload 
(defun my/astyle ()
  "Implement astyle to c-code when saving it."
  (interactive)
  (call-process "astyle" nil nil "--style=kr" "-s4" "-Y" "-M80" "-p" "-U" "-j" "-k3" "-c" (buffer-file-name))
  (revert-buffer nil t))
(el-get-bundle 'clang-format)
(setq clang-format-executable "clang-format-3.5")
(set-default 'clang-format-style "{BasedOnStyle: Google, IndentWidth: 4, Standard: C++11}")

;; Folding
(require 'hideshow)
(with-eval-after-load 'hideshow
  (add-hook 'html-mode-hook 'hs-minor-mode)
  (add-hook 'lisp-mode-hook 'hs-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
  (add-hook 'c-mode-common-hook 'hs-minor-mode)
  (add-hook 'latex-mode-hook 'hs-minor-mode)
  (add-hook 'YaTeX-mode-hook 'hs-minor-mode)
  (add-hook 'python-mode-hook 'hs-minor-mode)
  (define-key hs-minor-mode-map (kbd "C-^") 'hs-toggle-hiding)
  (define-key hs-minor-mode-map (kbd "C-M-^") 'hs-hide-all)
  (define-key hs-minor-mode-map (kbd "C-M-~") 'hs-show-all)
  (define-key hs-minor-mode-map (kbd "C-l ^") (lambda () (interactive) (hs-hide-level 2))))
(el-get-bundle 'yafolding
  (add-hook 'html-mode-hook 'yafolding-mode)
  (add-hook 'lisp-mode-hook 'yafolding-mode)
  (add-hook 'emacs-lisp-mode-hook 'yafolding-mode)
  (add-hook 'c-mode-common-hook 'yafolding-mode)
  (add-hook 'latex-mode-hook 'yafolding-mode)
  (add-hook 'YaTeX-mode-hook 'yafolding-mode)
  (add-hook 'python-mode-hook 'yafolding-mode))
(with-eval-after-load 'yafolding
  (setq yafolding-show-fringe-marks t))

;; External Program Utility
(defvar my/open-command nil)
(if (eq system-type 'darwin)
    (setq my/open-command "open")
  (setq my/open-command "exinautilus"))
;;;###autoload
(defun start-nautilus ()
  (interactive)
  (let ((dd (expand-file-name default-directory)))
    (async-start-process "Filer" my/open-command 'ignore dd)))
(global-set-key (kbd "C-\\ e") 'start-nautilus)
(global-set-key (kbd "C-\\ e") 'start-nautilus)
;;;###autoload
(defun start-gnome-terminal ()
  (interactive)
  (let ((dd (expand-file-name default-directory)))
    (async-start-process "gnome-terminal" "gnome-terminal" 'ignore default-directory)))
;;;###autoload
(defun start-mac-terminal ()
  (interactive)
  (let ((dd (expand-file-name default-directory)))
    (async-start-process "terminal" "open" 'ignore "-a" "Terminal" dd)))
;;;###autoload
(defun my/popup-eshell (arg)
  (interactive "p")
  (let (eb)
    (save-window-excursion
      (setq eb (eshell arg)))
    (popwin:popup-buffer-tail eb)))
(cond ((eq system-type 'darwin)
       (global-set-key (kbd "C-\\ c") 'start-mac-terminal)
       (global-set-key (kbd "C-\\ E") 'eshell)
       (global-set-key (kbd "C-\\ M-e") 'my/popup-eshell))
      (else
       (global-set-key (kbd "C-\\ c") 'start-gnome-terminal)
       (global-set-key (kbd "C-\\ E") 'eshell)
       (global-set-key (kbd "C-\\ M-e") 'my/popup-eshell)))
(global-set-key (kbd "C-\\ 9") 'popwin:stick-popup-window)

;;;###autoload
(defun my/insert-emacs-init-time-in-scratch ()
  (interactive)
  (with-output-to-temp-buffer "*information*"
    (let ((str (format "Emacs init time: %s\n\n\n" (emacs-init-time))))
      (princ str))))
(add-hook 'after-init-hook 'my/insert-emacs-init-time-in-scratch)

;; Multiple-Cursors
(el-get-bundle! multiple-cursors
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
  (global-set-key (kbd "C-S-c C-S-e") 'mc/mark-more-like-this-extended))

;; Magit
(el-get-bundle magit)
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-revert-buffers t)

;;;###autoload
(defun my/copy-current-file-name ()
  (interactive)
  (kill-new (buffer-file-name) nil)
  (message "kill new '%s'" (buffer-file-name)))
(global-set-key (kbd "C-l C-w p") 'my/copy-current-file-name)

;; Number
(el-get-bundle 'number)
(require 'number)
(global-set-key (kbd "C-c C-+") 'number/add)
(global-set-key (kbd "C-c C--") 'number/sub)
(global-set-key (kbd "C-c C-*") 'number/multiply)
(global-set-key (kbd "C-c C-/") 'number/divide)
(global-set-key (kbd "C-c C-0") 'number/pad)
(global-set-key (kbd "C-c C-=") 'number/eval)

;; Backup
(let ((target-dir (expand-file-name "~/"))
      (dest-dir (expand-file-name "~/.Trash/")))
  ;; 自動保存ファイル(#*#)の作成先変更
  (add-to-list 'auto-save-file-name-transforms
               `(,(concat target-dir "\\([^/]*/\\)*\\([^/]*\\)$")
                 ,(concat dest-dir "\\2")
                 t))
  ;; バックアップファイル(*~)の作成先変更
  (add-to-list 'backup-directory-alist (cons target-dir dest-dir))
  ;; 自動保存リスト(.saves-<PID>-<HOSTNAME>)の作成先変更
  (setq auto-save-list-file-prefix (expand-file-name ".saves-" dest-dir)))
;; (setq backup-directory-alist
;;       `(("\\.*") . ,(expand-file-name "~/.emacs.d/backup/")))
;; (add-to-list 'backup-directory-alist
;;              (cons (expand-file-name "~/") (expand-file-name "~/.Trash/")))

;;; Python
(require 'python)
(with-eval-after-load 'python
;;;###autoload
  (defun python-shell-send-line()
    "send current line to python shell."
    (interactive)
    (python-shell-send-region (line-beginning-position)
                              (line-end-position)))
  (define-key python-mode-map (kbd "C-c C-u") 'python-shell-send-line)
  (setq python-shell-interpreter "python3")
  (setq python-shell-interpreter-args "-i")
  (setq indent-tabs-mode nil)
  (setq indent-level 4)
  (setq python-indent 4)
  (setq tab-width 4)
  (setq python-indent-guess-indent-offset nil)
  ;; 他のところでエラーがでるかもしれない.
  ;; run-python で日本語を通すために必要
  (setenv "LANG" "ja_JP.UTF-8")
  (setenv "LC_ALL" "ja_JP.UTF-8")
  (setenv "LC_CTYPE" "ja_JP.UTF-8"))
(el-get-bundle jedi
  (add-hook 'python-mode-hook 'jedi:ac-setup))
(require 'jedi)
(with-eval-after-load 'jedi
  ;; (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))
(el-get-bundle py-autopep8
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))
;; py-autopep8 は 自分でいじりました. (pop kill-ring) を抜きました.
;; これがあると,保存した際,kill-ringが消えます.
;; el-get-updateすると,元に戻ってしまいます.極力しないこと.
;; してしまい,症状が出てくる場合は,(pop kill-ring)を削除すること

;; Quickrun
(el-get-bundle! 'quickrun)
(global-set-key (kbd "C-l C-l r") 'quickrun)

;; Gnuplot
(el-get-bundle 'gnuplot-mode)

;; Tramp
(require 'tramp)
(setq tramp-default-method "scp")

;; mac / customize off
(when (eq system-type 'darwin)
  (global-unset-key (kbd "s-,")))

;; path config これがないと，platexが実行できなかったりします．
(el-get-bundle exec-path-from-shell
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; auto-insert
(auto-insert-mode)
(setq auto-insert-directory "~/.emacs.d/template/")
(setq auto-insert-query nil)
;; (define-auto-insert '(python-mode . "python header") ["template.py" end-of-buffer])
;; (define-auto-insert '("\\.tex\\'" . "latex header") ["template.tex" yas-minor-mode end-of-line yas-expand])

;; dokuwiki
(el-get-bundle ox-wk :type git :url "git@github.com:w-vi/ox-wk.el")

(el-get-bundle org-textile :type git :url "git@github.com:yashi/org-textile")
;;;###autoload
(defun org-textile-example-block(example-block contents info)
  (let ((value (org-element-property :value example-block)))
    (concat "<pre>" value "\n</pre>")))

;; Html
(el-get-bundle! zencoding-mode)

;; Scheme
;; (setq scheme-program-name "jakld")
(setq process-coding-system-alist
      (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))
(if (eq system-type 'darwin)
    (setq scheme-program-name "/usr/local/bin/gosh -i")
  (setq scheme-program-name "/usr/bin/gosh -i"))
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)
(setq cmuscheme-load-hook
      '((lambda () (define-key scheme-mode-map (kbd "C-c C-p") 'run-scheme))))

;; Json
(el-get-bundle 'json-mode)

;; Clipmon
(el-get-bundle bburns/clipmon)

;; key-bindings 2
(global-set-key (kbd "C-q M-i") 'quoted-insert)
(global-set-key (kbd "C-x C-r") 'eval-region)

;; private
(load-file "~/.emacs.d/private.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(package-selected-packages
   (quote
    (rainbow-mode nil yatex sequential-command rotate point-undo org number ess)))
 '(safe-local-variable-values (quote ((major-mode . org-mode)))))

