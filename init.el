;;; init.el --- Martin's init file

;;; Commentary:

;;; Code:

;; global variables
(setq
 inhibit-startup-screen t
 create-lockfiles nil
 make-backup-files nil
 column-number-mode t
 scroll-error-top-bottom t
 show-paren-delay 0.5
 use-package-always-ensure t
 use-package-always-defer t
 sentence-end-double-space nil
 mac-option-modifier 'meta
 mac-command-modifier 'super
)

;; Use Fira Code as default font
(set-face-attribute 'default nil :font "Fira Code Regular")
(set-frame-font "Fira Code Regular" nil t)

;; Hopefully this will mean ENSIME can find sbt (needs to happen before we load ENSIME)
(add-to-list 'exec-path "/usr/local/bin")

;; Custom packages culled from the internet
(add-to-list 'load-path "~/.emacs.d/snippets/")
(require 'fira-code-mode)

;; buffer local variables
(setq-default
 indent-tabs-mode nil
 tab-width 4
 c-basic-offset 4)

;; modes
(electric-indent-mode 0)

;; small font - value is in 1/10th of a pt, so 100 gives you 10pt
(set-face-attribute 'default nil :height 100)

;; Smoother, slower mouse scrolling
(setq mouse-wheel-scroll-amount '(0.02))
(setq mouse-wheel-progressive-speed nil)

;; Stop bloody dinging all the time
(setq ring-bell-function 'ignore)

;; global keybindings
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "s-u") (lambda () (interactive) (revert-buffer nil t t)))

;; the package manager
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Toggle tree view
(use-package project-explorer)
(global-set-key (kbd "<C-tab>") 'project-explorer-toggle)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("69831e572dc46ced47c5309bff8fc2f4a9e237e2bad2c76f313da814a4628694" default)))
 '(elfeed-feeds
   (quote
    ("https://eev.ee/feeds/blog.atom.xml" "https://danluu.com/atom.xml" "https://daringfireball.net/feeds/main" "https://www.jwz.org/blog/feed/")))
 '(fci-rule-color "#3C3D37")
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "target" ".ensime_cache" ".ensime_snapshot" "node_modules" "dist" ".sass-cache" "build" ".terraform" ".metals" ".bloop" "coverage" ".jest" ".cache")))
 '(grep-find-ignored-files
   (quote
    (".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" ".ensime" "*.min.css" "*.min.css.map" "*.bundle.js")))
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(js-indent-level 2)
 '(magit-diff-arguments (quote ("--stat")))
 '(magit-diff-refine-hunk (quote all))
 '(magit-diff-use-overlays nil)
 '(magit-pull-arguments nil)
 '(package-selected-packages
   (quote
    (sql-mode sql-indent ag kotlin-mode lsp-java lsp-metals typescript-mode company company-lsp flymd terraform-mode fill-column-indicator scala-mode flycheck sbt-mode lsp-mode lsp-scala lsp-ui elfeed expand-region csv-mode popup-imenu yaml-mode markdown-mode magit multi-term project-explorer helm projectile exec-path-from-shell monokai-theme use-package csv-mode csharp-mode prettier-js web-mode)))
 '(pe/omit-gitignore t)
 '(pe/omit-regex "^\\.git\\|^#\\|~$\\|^node_modules$\\|\\.ensime_snapshot")
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(sbt:program-options (quote ("-Djline.terminal=none" "-J-Xmx2g" "-mem" "2048")))
 '(term-bind-key-alist
   (quote
    (("C-c C-c" . term-interrupt-subjob)
     ("C-c C-e" . term-send-esc)
     ("C-p" . previous-line)
     ("C-n" . next-line)
     ("C-s" . isearch-forward)
     ("C-r" . isearch-backward)
     ("C-m" . term-send-return)
     ("C-y" . term-paste)
     ("M-f" . term-send-forward-word)
     ("M-b" . term-send-backward-word)
     ("M-o" . term-send-backspace)
     ("M-p" . term-send-up)
     ("M-n" . term-send-down)
     ("M-M" . term-send-forward-kill-word)
     ("M-N" . term-send-backward-kill-word)
     ("<C-backspace>" . term-send-backward-kill-word)
     ("M-r" . term-send-reverse-search-history)
     ("M-d" . term-send-delete-word)
     ("M-," . term-send-raw)
     ("M-." . comint-dynamic-complete)
     ("<M-backspace>" . term-send-backward-kill-word))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package csharp-mode)

;; Make sure multi-term terminals are login shells because then they have the normal $PATH, etc
(use-package multi-term)
(setq multi-term-program-switches "--login")

;; Use bash for M-x shell when on Windows
(if (string-equal system-type "windows-nt")
    (setq explicit-shell-file-name "C:/Users/mmcnulty/AppData/Local/Programs/Git/bin/bash.exe"
          explicit-bash.exe-args '("--login" "-i")))

;; Don't show the toolbar or scroll bar
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; A few other customisations suggested on the ENSIME page
(setq
 inhibit-startup-screen t
 create-lockfiles nil
 make-backup-files nil
 column-number-mode t)

;; Let's try out this monokai theme
(use-package monokai-theme)
(load-theme 'monokai t)

;; Use left Windows key as super
(if (string-equal system-type "windows-nt")
    (setq w32-pass-lwindow-to-system nil
          w32-lwindow-modifier 'super))

;; Make it easier to switch buffers
(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "M-[") 'previous-buffer)

;; Shortcut for toggling truncation
(global-set-key (kbd "C-c t") 'toggle-truncate-lines)

;; Use Helm for M-x
(use-package helm)
(global-set-key (kbd "M-x") 'helm-M-x)
;; Use Helm for C-x b (switch buffer)
(global-set-key (kbd "C-x b") 'helm-mini)
;; And for finding files
(global-set-key (kbd "C-x C-f") 'helm-find-files)
;; Use C-o for file outline popup thanks to ENSIME and popup-imenu
(use-package popup-imenu)
(global-set-key (kbd "C-o") 'popup-imenu)

;; Shortcut key for magit
(use-package magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c g") 'magit-file-popup)

;; Make sure Emacs' env vars are the same as you'd normally get in a shell
;; (relies on the exec-path-from-shell package)
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Make it easy to switch windows
(global-set-key (kbd "s-]") 'other-window)
(defun mhm-prev-window ()
  "Call 'other-window' with negative arg to move in opposite direction."
  (interactive)
  (other-window -1))
(global-set-key (kbd "s-[") 'mhm-prev-window)

;; Helpful error navigation
(global-set-key (kbd "s-,") 'previous-error)
(global-set-key (kbd "s-.") 'next-error)
(global-set-key (kbd "s-n") 'flycheck-next-error)
(global-set-key (kbd "s-m") 'flycheck-previous-error)

;; Turn on projectile everywhere
(use-package projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-switch-project-action
      (lambda ()
        (when (file-exists-p "README.md") (find-file-existing "README.md"))
        (delete-other-windows)
        (project-explorer-open)))


;; Get better filename matching than Projectile's default (ido)
;;(use-package grizzl
;;  :ensure t
;;  :pin melpa)
;;(setq projectile-completion-system 'grizzl)
;;(use-package flx-ido)
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; Try out markdown-mode
(use-package markdown-mode)

;; Allow hash to be entered
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
;; Allow hash to be entered in isearch
(define-key isearch-mode-map (kbd "M-3") '(lambda () (interactive) (isearch-process-search-char ?\#)))

;; Use yaml-mode
(use-package yaml-mode)

;; Use scala-mode
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package prettier-js)

;; Remove trailing whitespace on save
(add-to-list 'write-file-functions 'delete-trailing-whitespace)

(defun remove-from-list (lst elem)
  "Remove ELEM from LST using equal comparison."
  (let ((filtered (seq-filter (lambda (x) (not (equal elem x))) (symbol-value lst))))
    (set lst filtered)))
;; Execute this form to remove delete-trailing-whitespace from write-file-functions:
;; (remove-from-list 'write-file-functions 'delete-trailing-whitespace)

;; Make M-f and M-b camel-case aware
(add-hook 'scala-mode-hook 'subword-mode)
(add-hook 'web-mode-hook 'subword-mode)

;; Use nice ligatures in scala-mode
(add-hook 'scala-mode-hook 'fira-code-mode)

;; Make ? part of a word (to fix M-f over ??? identifiers)
;;(modify-syntax-entry ?\? "w" scala-mode-syntax-table)

(put 'downcase-region 'disabled nil)

(use-package expand-region
  :commands 'er/expand-region
  :bind ("C-=" . er/expand-region))

(defun sbt-compile ()
  "Run test:compile in active sbt buffer."
  (interactive)
  (sbt-clear)
  (sbt-command "test:compile"))

(global-set-key (kbd "C-c C-b c") 'sbt-compile)
(global-set-key (kbd "C-c C-b s") 'sbt-start)

(defun toggle-window-split ()
  "Toggle between horizontal and vertical window splits."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)

;;;; Install lsp-scala for metals
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  (setq sbt:program-options '("-Dsbt.supershell=false"))
)

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
 :pin melpa
 :hook (scala-mode . lsp)
       (lsp-mode . lsp-lens-mode)

 :config (setq lsp-prefer-flymake nil)
         (setq lsp-enable-snippet nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

;; Add metals backend for lsp-mode
(use-package lsp-metals)

(setq-default fill-column 127)

;;(use-package fill-column-indicator
;;  :after scala-mode
;;  :hook (scala-mode . fci-mode))

(use-package company
  :diminish company-mode
  :commands company-mode
  :init
  (setq
   company-dabbrev-ignore-case nil
   company-dabbrev-code-ignore-case nil
   company-dabbrev-downcase nil
   company-idle-delay 0
   company-minimum-prefix-length 4)
  :config
  ;; disables TAB in company-mode, freeing it for yasnippet
  ;;(define-key company-active-map [tab] nil)
  ;;(define-key company-active-map (kbd "TAB") nil)
  )

(use-package company-lsp
  :demand t
  :after lsp-scala
  :config
  (push 'company-lsp company-backends))

;;;; End metals/lsp-scala section

(use-package elfeed)

(use-package terraform-mode)

(use-package flymd)

(use-package csv-mode)

(use-package typescript-mode)

;; Flymd doesn't like Chrome on Mac OS (because by default Chrome doesn't let it open local files or something?)
;; See: https://github.com/mola-T/flymd/blob/master/browser.md#user-content-chrome-macos
(defun flymd-open-firefox (url)
  "Open URL in Firefox for flymd."
  (let ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "firefox " url)
           nil
           "/usr/bin/open"
           (list "-a" "firefox" url))))
(setq flymd-browser-open-function 'flymd-open-firefox)

;; Use tide for Typescript/TSX files
(defun setup-tide-mode ()
  (interactive)
  (message "%s" "Setting up Tide mode...")
  (tide-setup)
  (subword-mode +1)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1)
  (prettier-js-mode)
  (setq-default typescript-indent-level 2))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . setup-tide-mode)
         (typescript-mode . tide-hl-identifier-mode)
         ;;(before-save . tide-format-before-save)
         ))

(setq tide-format-options '(
  :insertSpaceAfterFunctionKeywordForAnonymousFunctions t
  :placeOpenBraceOnNewLineForFunctions nil
  :indentSize 2
))

(use-package web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; Stop web-mode from reindenting things (wrongly!) on yank
(setq web-mode-enable-auto-indentation nil)
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

;; syntax highlighting for wsdl files
(add-to-list 'auto-mode-alist '("\\.wsdl\\'" . sgml-mode))

;; Java IDE features
(use-package lsp-java)
(add-hook 'java-mode-hook #'lsp)

;; Kotlin?
(use-package kotlin-mode)

;; The silver searcher
(use-package ag)

;; sql-indent
(use-package sql-indent)
(add-to-list 'auto-mode-alist '("\\.elsql\\'" . sql-mode))

(put 'upcase-region 'disabled nil)

(provide 'init)
;;; init.el ends here
