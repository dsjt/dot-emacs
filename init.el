;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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

;; Modification for Mac
(when (eq system-type 'darwin)
  (setq mac-command-modifier (quote meta))
  (setq mac-option-modifier (quote super))
  (global-unset-key (kbd "s-,")))

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
  (setq show-paren-style 'parenthesis)
  (setq frame-title-format
        (format "%%f - Emacs@%s" (system-name)))
  (setq gc-cons-threshold (* 10 gc-cons-threshold))
  (setq mark-ring-max 64
        kill-whole-line t
        visible-bell nil
        ring-bell-function 'ignore)
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)
  (setq-default fill-column 80)
  (setq-default line-move-visual nil)
  (setq-default cursor-type 'bar)
  (save-place-mode 1)
  (setq save-place t)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (defalias 'exit 'save-buffers-kill-emacs)
  (show-paren-mode +1)
  (global-hl-line-mode +1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (column-number-mode +1)
  (transient-mark-mode +1)
  (global-visual-line-mode 1)
  (ffap-bindings)
  (put 'narrow-to-region 'disabled nil)
  (mouse-avoidance-mode 'exile)
  (add-hook 'emacs-lisp-mode 'electric-indent-mode)
  (savehist-mode 1)
  (require 'saveplace)
  (setq save-place-file "~/.emacs.d/saved-places")
  (save-place-mode 1))
;;;###autoload
(defun my/switch-to-scratch-buffer ()
  "Switch to scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (delete-other-windows))
(global-set-key (kbd "C-l SPC") 'my/switch-to-scratch-buffer)
(global-set-key (kbd "C-M-;") 'comment-line)
;;;###autoload
(defun other-window-or-split ()
  "other window or split window"
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(global-set-key (kbd "C-,") 'other-window-or-split)
(require 'cl)
;;;###autoload
(defun my/kill-other-buffers ()
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
(global-set-key (kbd "C-x C-c C-c") 'my/kill-other-buffers)
;;;###autoload
(defun toggle-fill-and-unfill ()
  "Toggle fill and unfill paragraph."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'toggle-fill-and-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))
(global-set-key [remap fill-paragraph] #'toggle-fill-and-unfill)
;; (global-set-key (kbd "M-q") #'fill-paragraph)

;; Smartrep
(el-get-bundle! 'smartrep)

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
  (global-set-key (kbd "M-/") 'helm-dabbrev)
  (define-key helm-map (kbd "C-h") 'delete-backward-char))
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

;; Org-Mode
(setq load-path (remove "/usr/local/Cellar/emacs/25.1/share/emacs/25.1/lisp/org" load-path))
(el-get-bundle org)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c C-x C-j") 'org-clock-goto)
(with-eval-after-load 'org
  (setq org-startup-folded nil)
  (setq org-hide-leading-stars nil)
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
  (define-key org-mode-map (kbd "C-S-p") 'org-metaleft)
  (define-key org-mode-map (kbd "M-S-<up>") 'org-move-subtree-up)
  (define-key org-mode-map (kbd "M-S-<down>") 'org-move-subtree-down)
  (add-hook 'org-mode-hook 'turn-on-font-lock)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'org-display-inline-images)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0)))
(with-eval-after-load 'org-agenda
  (setq org-agenda-custom-commands
        '(("n" "Agenda and all TODO's" ((agenda "") (alltodo "")))
          ("D" "1 day agenda"
           ((agenda "TODO" ((org-agenda-span 1)
                            (org-agenda-prefix-format '((agenda . "     %s %-8 e")))
                            (org-agenda-log-mode-items '(state))
                            ))))
          ("d" "3 days agenda"
           ((agenda "" ((org-agenda-span 3)
                        (org-agenda-prefix-format '((agenda . "     %s %-8 e")))
                        (org-agenda-show-log t)
                        (org-agenda-log-mode-items '(closed))
                        ))
           ;; ((agenda "TODO" ((org-agenda-span 3)
           ;;                  ;; (org-agenda-start-day "-1")
           ;;                  (org-agenda-prefix-format '((agenda . "     %s %-8 e")))
           ;;                  ;; (org-agenda-show-log t)
           ;;                  (org-agenda-log-mode-items '(state))
           ;;                  ))
            (tags "inbox")
            (tags-todo "-inbox")))
          ("w" "8 days agenda"
           ((agenda "" ((org-agenda-span 8)
                            (org-agenda-start-day "-1")
                            (org-agenda-prefix-format '((agenda . "     %s %-8 e")))
                            (org-agenda-columns-add-appointments-to-effort-sum t)
                            ))
            (tags "inbox")
            (todo "TODO")))))
  ;; 日付単位ではあるが、次やるべきことのリストで古い物は浮上し、新しいものは下に追加する
  (setq org-agenda-sorting-strategy
      '((agenda habit-down time-up priority-down category-keep)
        (todo priority-down tsia-up category-keep)
        (tags priority-down tsia-up category-keep)
        (search category-keep)))
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
;; (with-eval-after-load 'org-org-colview
  ;; (setq org-columns-default-format
  ;;       "%50ITEM{Task} %TODO %8EFFORT{:} %6CLOCKSUM_T{Total}"))
(with-eval-after-load 'ob
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (emacs-lisp . t)
     (shell . t)
     (gnuplot . t)
     (ruby . t)
     (python . t)
     (dot . t)
     (R . t)
     (org . t)))
  (setq org-src-window-setup 'other-window)
  (setq org-image-actual-width '(256))
  (define-key org-mode-map (kbd "C-c C-7") 'org-edit-special)
  (define-key org-src-mode-map (kbd "C-c C-7") 'org-edit-src-exit))
(el-get-bundle 'f)
(el-get-bundle! 'gregsexton/ob-ipython)
(autoload 'org-babel-execute:python "ob-python.el")
(with-eval-after-load 'ob-python
  (setq org-babel-python-command "python3")
  (setq org-src-preserve-indentation t)
  (setq org-babel-default-header-args:python '((:session . "my_session")
                                               (:results . "output")
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
(el-get-bundle 'calfw
  (global-set-key (kbd "C-c M-c") 'cfw:open-org-calendar))
(require 'calfw-org)
(require 'ox)


;; Visual
(add-to-list 'custom-theme-load-path "~/.emacs.d/theme/")
(global-set-key (kbd "<f1>C-d") 'describe-face)
(el-get-bundle solarized-emacs)
(add-to-list 'custom-theme-load-path "~/.emacs.d/el-get/solarized-emacs/")
(setq solarized-scale-org-headlines nil
      solarized-high-contrast-mode-line t)
;; (load-theme 'solarized-dark t)
;; (load-theme 'solarized-light t)
(el-get-bundle madhat2r/madhat2r-theme)
(add-to-list 'custom-theme-load-path "~/.emacs.d/el-get/madhat2r-theme/")
(load-theme 'madhat2r t)
;; (set-face-attribute 'default nil :family "IPAGothic" :height 160)
;; (set-fontset-font t 'japanese-jisx0208
;;                   (font-spec :family "IPAGothic"
;;                              :size 14))
;; (set-fontset-font t 'japanese-jisx0208
;;                   (font-spec :family "Hiragino Kaku Gothic ProN"
;;                              :size 14))
(progn
  (set-face-attribute 'default nil :family "IPAGothic" :height 140)
  (set-fontset-font t 'japanese-jisx0208
                    (font-spec :family "Hiragino Kaku Gothic ProN"
                               :size 14)))
;; (load-theme 'whiteboard t)
(add-to-list 'face-font-rescale-alist
             '((".*-Hiragino Kaku Gothic ProN-.*" . 1.2)))
;; (add-to-list 'face-font-rescale-alist
;;              '(("IPAGothic" . 1.2)))
(el-get-bundle rainbow-mode)
;;;###autoload
(defun set-alpha (alpha-num)
  "set frame parameter 'alpha"
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha (cons alpha-num '(90))))
;; (load "~/.emacs.d/site-lisp/hwin.el")
(require 'hiwin)
(setq hiwin-deactive-color "#030900")
(setq hiwin-readonly-color "#030900")
(hiwin-mode)

;; Region
(el-get-bundle expand-region)
;; (smartrep-define-key
;;     global-map "C-c" '(("t" . 'er/expand-region)
;;                        ("T" . 'er/contract-region)))
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
(el-get-bundle 'goto-chg)
(global-set-key (kbd "<f8>") 'goto-last-change)
(global-set-key (kbd "S-<f8>") 'goto-last-change-reverse)
(el-get-bundle 'helm-swoop
  (global-set-key (kbd "M-i") 'helm-swoop))
(el-get-bundle! bbatsov/crux
  (global-set-key (kbd "C-o") 'crux-smart-open-line)
  (global-set-key (kbd "M-o") 'crux-smart-open-line-above)
  (global-set-key (kbd "C-S-y") 'crux-duplicate-current-line-or-region)
  (global-set-key (kbd "C-c e") 'crux-eval-and-replace))
(el-get-bundle! 'avy
  (global-set-key (kbd "C-M-j") 'avy-goto-char))
(el-get-bundle 'ace-jump-mode)
  ;; (load-file "~/.emacs.d/site-lisp/ace-pinyin-myconf.el")
(smartrep-define-key
    global-map "C-c" '(("[" . (backward-paragraph))
                       ("]" . (forward-paragraph))))
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
                     ("\\.eps\\'" "open" (file)))))
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
(require 'open-junk-file)
;;;###autoload
(defun my/goto-junk-directory()
  (interactive)
  (let* ((file (format-time-string open-junk-file-format (current-time)))
         (dir (file-name-directory file)))
    (make-directory dir t)
    (find-file dir)))
(global-set-key (kbd "C-l f") 'my/goto-junk-directory)

;; Latex
(el-get-bundle elpa:yatex)
(require 'yatex)
(add-to-list 'auto-mode-alist
             '("\\.tex\\'" . yatex-mode))
(with-eval-after-load 'yatexprc
  (add-to-list 'YaTeX-dvi2-command-ext-alist '("open" . ".pdf")))
(el-get-bundle 'latex-math-preview
  :type emacswiki)
(autoload 'latex-math-preview-expression "latex-math-preview" nil t)
(autoload 'latex-math-preview-insert-symbol "latex-math-preview" nil t)
(autoload 'latex-math-preview-save-image-file "latex-math-preview" nil t)
(autoload 'latex-math-preview-beamer-frame "latex-math-preview" nil t)
(defun my/latex-math-preview-settings()
  (YaTeX-define-key "p" 'latex-math-preview-expression)
  (YaTeX-define-key "\C-p" 'latex-math-preview-save-image-file)
  (YaTeX-define-key "j" 'latex-math-preview-insert-symbol)
  (YaTeX-define-key "\C-j" 'latex-math-preview-last-symbol-again)
  (YaTeX-define-key "\C-b" 'latex-math-preview-beamer-frame))
(add-hook 'yatex-mode-hook 'my/latex-math-preview-settings)
(setq latex-math-preview-in-math-mode-p-func 'YaTeX-in-math-mode-p)
(define-key org-mode-map (kbd "C-l C-p") 'latex-math-preview-expression)

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
  ;; (electric-pair-mode -1)
  )
(el-get-bundle 'paredit)

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
;; しばらくcompany-modeをお試し
;; (el-get-bundle company-mode)
;; (global-company-mode)
;; ;; C-n, C-pで補完候補を次/前の候補を選択
;; (define-key company-active-map (kbd "C-n") 'company-select-next)
;; (define-key company-active-map (kbd "C-p") 'company-select-previous)
;; (define-key company-search-map (kbd "C-n") 'company-select-next)
;; (define-key company-search-map (kbd "C-p") 'company-select-previous)

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
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-margin 5)
(setq next-screen-context-lines 10)
(setq scroll-preserve-screen-position t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

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
(global-set-key (kbd "C-q c") 'howm-create)
(global-set-key (kbd "C-q s") 'howm-list-grep-fixed)

;; Translate
(el-get-bundle! popwin)
(popwin-mode 1)
;; (el-get-bundle! 'google-translate)
;; ;;;###autoload
;; (defun google-en-to-ja ()
;;   (interactive)
;;   (google-translate-translate "en" "ja"
;;                               (if (use-region-p)
;;                                   (buffer-substring-no-properties (region-beginning)
;;                                                                   (region-end))
;;                                 (or (current-word t t)
;;                                     (error "No word at point.")))))
;; (global-set-key (kbd "C-l e") 'google-en-to-ja)

;; rotate
(el-get-bundle 'rotate)
(smartrep-define-key
    global-map "C-l" '(("w" . (rotate-window))
                       ("l" . (rotate-layout))))
;; (smartrep-define-key
;;     global-map "C-l" '(("{" . (shrink-window-horizontally 2))
;;                        ("}" . (enlarge-window-horizontally 2))))

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
  ;; (define-key hs-minor-mode-map (kbd "C-l ^") (lambda () (interactive) (hs-hide-level 2)))
  )
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
(global-set-key (kbd "C-l C-m C-<SPC>") 'magit-status)

;; Git-Gutter
(el-get-bundle git-gutter)
(global-git-gutter-mode t)
(global-set-key (kbd "C-l C-g s") 'git-gutter:stage-hunk)
(global-set-key (kbd "C-l C-g C-n") 'git-gutter:next-hunk)
(global-set-key (kbd "C-l C-g C-p") 'git-gutter:previous-hunk)

;; Git-Gutter-Fringe
(el-get-bundle git-gutter-fringe)
(require 'git-gutter-fringe)

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
;; (el-get-bundle! 'python)
(require 'python)
;;;###autoload
(defun python-shell-send-line()
  "send current line to python shell."
  (interactive)
  (python-shell-send-region (line-beginning-position)
                            (line-end-position)))
;;;###autoload
(defun python-shell-send-paragraph()
  "send current line to python shell."
  (interactive)
  (let ((para (thing-at-point 'paragraph t)))
    (python-shell-send-string para)
    (message "Sent ...")))
(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c C-u") 'python-shell-send-line)
  (define-key python-mode-map (kbd "C-c p") 'python-shell-send-paragraph)
  (define-key python-mode-map (kbd "C-M-f") 'python-nav-forward-defun)
  (define-key python-mode-map (kbd "C-M-b") 'python-nav-backward-defun)
  ;; (add-hook 'python-mode-hook 'turn-off-smartparens-mode)

  (setq python-shell-interpreter "python3")
  (setq python-shell-interpreter-args "-i")
  (setq indent-tabs-mode nil)
  (setq indent-level 4)
  (setq python-indent 4)
  (setq tab-width 4)
  (setq python-indent-guess-indent-offset nil)
  (setq python-fill-paren-function nil)
  ;; 他のところでエラーがでるかもしれない.
  ;; run-python で日本語を通すために必要
  (setenv "LANG" "ja_JP.UTF-8")
  (setenv "LC_ALL" "ja_JP.UTF-8")
  (setenv "LC_CTYPE" "ja_JP.UTF-8")
  (add-to-list 'python-shell-completion-native-disabled-interpreters "python3")
  )

;; (el-get-bundle jedi
;;   (add-hook 'python-mode-hook 'jedi:ac-setup))
;; (require 'jedi)
;; (with-eval-after-load 'jedi
;;   ;; (add-hook 'python-mode-hook 'jedi:setup)
;;   (setq jedi:complete-on-dot t))
(el-get-bundle py-autopep8)
;; (el-get-bundle py-autopep8
;;   (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))
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

;; path config これがないと，platexが実行できなかったりします．
(el-get-bundle exec-path-from-shell
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; auto-insert
;; (auto-insert-mode -1)
;; (setq auto-insert-directory "~/.emacs.d/template/")
;; (setq auto-insert-query nil)
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

;; Htmlize
(el-get-bundle htmlize)

;; key-bindings 2
(global-set-key (kbd "C-q M-i") 'quoted-insert)
(global-set-key (kbd "C-x C-r") 'eval-region)

;; Whitespace
(require 'whitespace)
(setq whitespace-style '(face           ; faceで可視化
                         trailing       ; 行末
                         tabs           ; タブ
                         spaces         ; スペース
                         empty          ; 先頭/末尾の空行
                         space-mark     ; 表示のマッピング
                         tab-mark
                         ))
(setq whitespace-display-mappings
      '((space-mark ?\u3000 [?\u25a1])
        ;; WARNING: the mapping below has a problem.
        ;; When a TAB occupies exactly one column, it will display the
        ;; character ?\xBB at that column followed by a TAB which goes to
        ;; the next TAB column.
        ;; If this is a problem for you, please, comment the line below.
        (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
;; スペースは全角のみを可視化
(setq whitespace-space-regexp "\\(\u3000+\\)")
;; 保存前に自動でクリーンアップ
(setq whitespace-action '(auto-cleanup))
(global-whitespace-mode 1)
(defvar my/bg-color "#232323")
(set-face-attribute 'whitespace-trailing nil
                    :background my/bg-color
                    :foreground "DeepPink"
                    :underline t)
(set-face-attribute 'whitespace-tab nil
                    :background my/bg-color
                    :foreground "LightSkyBlue"
                    :underline t)
(set-face-attribute 'whitespace-space nil
                    :background my/bg-color
                    :foreground "GreenYellow"
                    :weight 'bold)
(set-face-attribute 'whitespace-empty nil
                    :background my/bg-color)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Volitate
(el-get-bundle volatile-highlights)
(volatile-highlights-mode 1)

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
 '(helm-external-programs-associations (quote (("png" . "open"))))
 '(package-selected-packages
   (quote
    (rainbow-mode nil yatex sequential-command rotate point-undo org number ess)))
 '(safe-local-variable-values
   (quote
    ((major-mode . org)
     (eval font-lock-add-keywords nil
           (\`
            (((\,
               (concat "("
                       (regexp-opt
                        (quote
                         ("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl"))
                        t)
                       "\\_>"))
              1
              (quote font-lock-variable-name-face)))))
     (eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1))))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(el-get-bundle 'migemo)
(require 'migemo)
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))
;; (defun helm-compile-source--candidates-in-buffer (source)
;;   (helm-aif (assoc 'candidates-in-buffer source)
;;       (append source
;;               `((candidates
;;                  . ,(or (cdr it)
;;                         (lambda ()
;;                           ;; Do not use `source' because other plugins
;;                           ;; (such as helm-migemo) may change it
;;                           (helm-candidates-in-buffer (helm-get-current-source)))))
;;                 (volatile) (match identity)))
;;     source))
;; ;; [2015-09-06 Sun]helm-match-plugin -> helm-multi-match変更の煽りを受けて
;; (defalias 'helm-mp-3-get-patterns 'helm-mm-3-get-patterns)
;; (defalias 'helm-mp-3-search-base 'helm-mm-3-search-base)
(helm-migemo-mode 1)
;; org-refile で migemo を使うために、一部変更して定義
(cl-defun helm-comp-read (prompt collection
                          &key
                            test
                            initial-input
                            default
                            preselect
                            (buffer "*Helm Completions*")
                            must-match
                            fuzzy
                            reverse-history
                            (requires-pattern 0)
                            history
                            input-history
                            (case-fold helm-comp-read-case-fold-search)
                            (del-input t)
                            (persistent-action nil)
                            (persistent-help "DoNothing")
                            (mode-line helm-comp-read-mode-line)
                            help-message
                            (keymap helm-comp-read-map)
                            (name "Helm Completions")
                            header-name
                            candidates-in-buffer
                            match-part
                            exec-when-only-one
                            quit-when-no-cand
                            (volatile t)
                            sort
                            (fc-transformer 'helm-cr-default-transformer)
                            hist-fc-transformer
                            marked-candidates
                            nomark
                            (alistp t)
                            (candidate-number-limit helm-candidate-number-limit)
                            multiline
                            allow-nest)
  "Read a string in the minibuffer, with helm completion.

It is helm `completing-read' equivalent.

- PROMPT is the prompt name to use.

- COLLECTION can be a list, vector, obarray or hash-table.
  It can be also a function that receives three arguments:
  the values string, predicate and t. See `all-completions' for more details.

Keys description:

- TEST: A predicate called with one arg i.e candidate.

- INITIAL-INPUT: Same as input arg in `helm'.

- PRESELECT: See preselect arg of `helm'.

- DEFAULT: This option is used only for compatibility with regular
  Emacs `completing-read' (Same as DEFAULT arg of `completing-read').

- BUFFER: Name of helm-buffer.

- MUST-MATCH: Candidate selected must be one of COLLECTION.

- FUZZY: Enable fuzzy matching.

- REVERSE-HISTORY: When non--nil display history source after current
  source completion.

- REQUIRES-PATTERN: Same as helm attribute, default is 0.

- HISTORY: A list containing specific history, default is nil.
  When it is non--nil, all elements of HISTORY are displayed in
  a special source before COLLECTION.

- INPUT-HISTORY: A symbol. the minibuffer input history will be
  stored there, if nil or not provided, `minibuffer-history'
  will be used instead.

- CASE-FOLD: Same as `helm-case-fold-search'.

- DEL-INPUT: Boolean, when non--nil (default) remove the partial
  minibuffer input from HISTORY is present.

- PERSISTENT-ACTION: A function called with one arg i.e candidate.

- PERSISTENT-HELP: A string to document PERSISTENT-ACTION.

- MODE-LINE: A string or list to display in mode line.
  Default is `helm-comp-read-mode-line'.

- KEYMAP: A keymap to use in this `helm-comp-read'.
  (the keymap will be shared with history source)

- NAME: The name related to this local source.

- HEADER-NAME: A function to alter NAME, see `helm'.

- EXEC-WHEN-ONLY-ONE: Bound `helm-execute-action-at-once-if-one'
  to non--nil. (possibles values are t or nil).

- VOLATILE: Use volatile attribute.

- SORT: A predicate to give to `sort' e.g `string-lessp'
  Use this only on small data as it is ineficient.
  If you want to sort faster add a sort function to
  FC-TRANSFORMER.
  Note that FUZZY when enabled is already providing a sort function.

- FC-TRANSFORMER: A `filtered-candidate-transformer' function
  or a list of functions.

- HIST-FC-TRANSFORMER: A `filtered-candidate-transformer'
  function for the history source.

- MARKED-CANDIDATES: If non--nil return candidate or marked candidates as a list.

- NOMARK: When non--nil don't allow marking candidates.

- ALISTP: \(default is non--nil\) See `helm-comp-read-get-candidates'.

- CANDIDATES-IN-BUFFER: when non--nil use a source build with
  `helm-source-in-buffer' which is much faster.
  Argument VOLATILE have no effect when CANDIDATES-IN-BUFFER is non--nil.

- MATCH-PART: Allow matching only one part of candidate.
  See match-part documentation in `helm-source'.

- ALLOW-NEST: Allow nesting this `helm-comp-read' in a helm session.
  See `helm'.

- MULTILINE: See multiline in `helm-source'.

Any prefix args passed during `helm-comp-read' invocation will be recorded
in `helm-current-prefix-arg', otherwise if prefix args were given before
`helm-comp-read' invocation, the value of `current-prefix-arg' will be used.
That's mean you can pass prefix args before or after calling a command
that use `helm-comp-read' See `helm-M-x' for example."

  (when (get-buffer helm-action-buffer)
    (kill-buffer helm-action-buffer))
  (let ((action-fn `(("Sole action (Identity)"
                      . (lambda (candidate)
                          (if ,marked-candidates
                              (helm-marked-candidates)
                              (identity candidate)))))))
    ;; Assume completion have been already required,
    ;; so always use 'confirm.
    (when (eq must-match 'confirm-after-completion)
      (setq must-match 'confirm))
    (let* ((minibuffer-completion-confirm must-match)
           (must-match-map (when must-match helm-comp-read-must-match-map))
           (loc-map (if must-match-map
                        (make-composed-keymap
                         must-match-map (or keymap helm-map))
                      (or keymap helm-map)))
           (minibuffer-completion-predicate test)
           (minibuffer-completion-table collection)
           (helm-read-file-name-mode-line-string
            (replace-regexp-in-string "helm-maybe-exit-minibuffer"
                                      "helm-confirm-and-exit-minibuffer"
                                      helm-read-file-name-mode-line-string))
           (get-candidates
            (lambda ()
              (let ((cands (helm-comp-read-get-candidates
                            collection test sort alistp)))
                (setq helm-cr--unknown-pattern-flag nil)
                (unless (or (eq must-match t)
                            (string= helm-pattern "")
                            (assoc helm-pattern cands)
                            (assoc (intern helm-pattern) cands)
                            (member helm-pattern cands)
                            (member (downcase helm-pattern) cands)
                            (member (upcase helm-pattern) cands))
                  (setq cands (append (list
                                       ;; Unquote helm-pattern
                                       ;; when it is added
                                       ;; as candidate.
                                       (replace-regexp-in-string
                                        "\\s\\" "" helm-pattern))
                                      cands))
                  (setq helm-cr--unknown-pattern-flag t))
                (helm-cr-default default cands))))
           (history-get-candidates
            (lambda ()
              (let ((cands (helm-comp-read-get-candidates
                            history test nil alistp)))
                (when cands
                  (delete "" (helm-cr-default default cands))))))
           (src-hist (helm-build-sync-source (format "%s History" name)
                       :candidates history-get-candidates
                       :fuzzy-match fuzzy
                       :multiline multiline
                       :match-part match-part
                       :filtered-candidate-transformer
                       (append '((lambda (candidates sources)
                                   (cl-loop for i in candidates
                                            ;; Input is added to history in completing-read's
                                            ;; and may be regexp-quoted, so unquote it
                                            ;; but check if cand is a string (it may be at this stage
                                            ;; a symbol or nil) Issue #1553.
                                            when (stringp i)
                                            collect (replace-regexp-in-string "\\s\\" "" i))))
                               (and hist-fc-transformer (helm-mklist hist-fc-transformer)))
                       :persistent-action persistent-action
                       :persistent-help persistent-help
                       :keymap loc-map
                       :mode-line mode-line
                       :help-message help-message
                       :action action-fn
                       :migemo t))
           (src (helm-build-sync-source name
                  :candidates get-candidates
                  :match-part match-part
                  :multiline multiline
                  :header-name header-name
                  :filtered-candidate-transformer fc-transformer
                  :requires-pattern requires-pattern
                  :persistent-action persistent-action
                  :persistent-help persistent-help
                  :fuzzy-match fuzzy
                  :keymap loc-map
                  :mode-line mode-line
                  :help-message help-message
                  :action action-fn
                  :volatile volatile
                  :migemo t))
           (src-1 (helm-build-in-buffer-source name
                    :data get-candidates
                    :match-part match-part
                    :multiline multiline
                    :header-name header-name
                    :filtered-candidate-transformer fc-transformer
                    :requires-pattern requires-pattern
                    :persistent-action persistent-action
                    :fuzzy-match fuzzy
                    :keymap loc-map
                    :persistent-help persistent-help
                    :mode-line mode-line
                    :help-message help-message
                    :action action-fn
                    :migemo t))
           (src-list (list src-hist
                           (if candidates-in-buffer
                               src-1 src)))
           (helm-execute-action-at-once-if-one exec-when-only-one)
           (helm-quit-if-no-candidate quit-when-no-cand)
           result)
      (when nomark
        (setq src-list (cl-loop for src in src-list
                             collect (cons '(nomark) src))))
      (when reverse-history (setq src-list (nreverse src-list)))
      (add-hook 'helm-after-update-hook 'helm-comp-read--move-to-first-real-candidate)
      (unwind-protect
           (setq result (helm
                         :sources src-list
                         :input initial-input
                         :default default
                         :preselect preselect
                         :prompt prompt
                         :resume 'noresume
                         :allow-nest allow-nest
                         :candidate-number-limit candidate-number-limit
                         :case-fold-search case-fold
                         :history (and (symbolp input-history) input-history)
                         :buffer buffer))
        (remove-hook 'helm-after-update-hook 'helm-comp-read--move-to-first-real-candidate))
      ;; Avoid adding an incomplete input to history.
      (when (and result history del-input)
        (cond ((and (symbolp history) ; History is a symbol.
                    (not (symbolp (symbol-value history)))) ; Fix Issue #324.
               ;; Be sure history is not a symbol with a nil value.
               (helm-aif (symbol-value history) (setcar it result)))
              ((consp history) ; A list with a non--nil value.
               (setcar history result))
              (t ; Possibly a symbol with a nil value.
               (set history (list result)))))
      (or result (helm-mode--keyboard-quit)))))

;; Set your installed path
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")

(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(migemo-init)
