
;; My emacs init file
;; JU Han
;; ju.han.felix@gmail.com

;; Load path ----------------------------------------------------------
;; (add-to-list 'load-path "~/.emacs.d")
;; (progn (cd "~/.emacs.d")
;;         (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path "~/.emacs.d/vendors")
;; (progn (cd "~/.emacs.d/vendors")
;;         (normal-top-level-add-subdirs-to-load-path))

;; User settings ------------------------------------------------------
(setq user-full-name "JU Han")
(setq user-mail-address "ju.han.felix@gmail.com")

;; Convenient set-ups -------------------------------------------------
(show-paren-mode t)
(setq show-paren-style 'parentheses)
(mouse-avoidance-mode 'animate)
(setq frame-title-format "emacs@ %b")
(global-font-lock-mode t)
(setq scroll-mqrgin 3
      scroll-conservatively 10000)
(setq default-major-mode 'text-mode)
(setq redisplay-dont-pause t)

;; Line numbers -------------------------------------------------------
(require 'linum)
(global-linum-mode t)

;; ELPA source  -------------------------------------------------------
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Set default browser to Chromium ------------------------------------
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium-browser")

;; Scheme -------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ibus-python-shell-command-name "/usr/bin/python2")
 '(org-agenda-files (quote ("~/Dropbox/Notes/algorithm.org")))
 '(quack-default-program "racket")
 '(quack-newline-behavior (quote indent-newline-indent))
 '(quack-programs (quote ("petite" "mzscheme" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "1mit-scheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(quack-switch-to-scheme-method (quote other-window))
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil nil (tool-bar)))
(require 'quack)
(set-face-attribute 'default nil :font "DejaVu Sans Mono-10")

(require 'scheme-complete)
(autoload 'scheme-smart-complete "scheme-complete" nil t)
(eval-after-load 'scheme
   '(progn (define-key scheme-mode-map "\t" 'scheme-complete-or-indent)))
(add-hook 'scheme-mode-hook 'lisp-coding-hook)


;; Color theme --------------------------------------------------------

(require 'color-theme)
(setq color-theme-is-global t)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     ;; (color-theme-orico-black)))
     (color-theme-monokai)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-3 ((t (:inherit outline-5))))
 '(org-level-5 ((t (:inherit outline-6))))
 '(org-level-6 ((t (:inherit outline-3))))
 '(quack-pltish-comment-face ((((class color) (background dark)) (:foreground "red1"))))
 '(quack-pltish-defn-face ((t (:foreground "yellow1" :weight bold))))
 '(quack-pltish-keyword-face ((t (:foreground "darkturquoise" :weight bold))))
 '(quack-pltish-paren-face ((((class color) (background dark)) (:foreground "orange1")))))

;; Configurations for LaTex -------------------------------------------

(add-to-list 'load-path "~/.emacs.d/vendors/auctex-11.86")
(load "auctex.el" nil t t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)

(setq TeX-view-program-selection
      '((output-dvi "DVI Viewer")
        (output-pdf "PDF Vixewer")
        (output-html "Google Chrome")))
(setq TeX-view-program-list
      '(("DVI Viewer" "evince %o")
        ("PDF Viewer" "zathura %o")
        ("Google Chrome" "google-chrome %o")))

(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook (lambda () (abbrev-mode +1)))

;; (load "preview-latex.el" nil t t)


;; Configurations for Python ------------------------------------------
(add-to-list 'load-path "~/.emacs.d/vendors/python-mode/")
(setq py-install-directory "~/.emacs.d/vendors/python-mode/")
(setq py-shell-name "ipython2")
(require 'python-mode)

;; (require 'ipython)
;; (setq py-python-command-args '("-pylab" "-colors" "nocolor"))

(require 'lambda-mode)
(add-hook 'python-mode-hook #'lambda-mode 1)
(setq lambda-symbol (string (make-char 'greek-iso8859-7 107)))
;; Autopair -----------------------------------------------------------

(autoload 'autopair-global-mode "autopair" nil t)
(autopair-global-mode t)
;; (add-hook 'lisp-mode-hook
;;           #'(lambda () (setq autopair-dont-activate t)))
;;Autopair for python
;; (add-hook 'python-mode-hook
;;           #'(lambda ()
;;                (push '(?' . ?')

;;                      (getf autopair-extra-pairs :code))
;;                (setq autopair-handle-action-fns
;;                      (list #'autopair-default-handle-action
;;                            #'autopair-python-triple-quote-action))))

;;delete trailing spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; Yasnippet ----------------------------------------------------------

;; (require 'yasnippet-bundle)
;; (yas/initialize)
;; (yas/load-directory "~/.emacs.d/snippets/")


;; Auto complete ------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/vendors/auto-complete-1.3")
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/vendors/auto-complete-1.3/dict")
(require 'auto-complete-config)
(ac-config-default)

;; use C-n, C-p to navigate completion menu
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

;; Make Ac mode aware of latex1
(require 'ac-math)
(add-to-list 'ac-modes 'latex-mode 'org-mode)

(defun ac-latex-mode-setup ()         ; add ac-sources to default ac-sources
  (setq ac-sources
	(append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
		ac-sources))
)
(add-hook 'LaTeX-mode-hook 'ac-latex-mode-setup)

;(autoload 'ac-latex-setup "auto-complete-latex" "ac and aucTeX" t)
;(add-hook 'LaTeX-mode-hook (lambda() (ac-latex-setup)))

;; (require 'smart-operator)


;; ido-mode ------------------------------------------------------------

;; (when (fboundp 'ido-mode)
;;   (ido-mode t)
;;   (setq ido-save-directory-list-file nil))
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)


;; Javascript mode -- js2-mode -----------------------------------------

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))


;; C Programming Languauge ---------------------------------------------

(require 'cc-mode)
;;(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
(define-key c-mode-map [return] 'newline-and-indent)


;; Perl Programming ----------------------------------------------------

;; Use cperl-mode instead of the default perl-mode
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))


;; Code folding --------------------------------------------------------

(autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")
(autoload 'hideshowvis-minor-mode
  "hideshowvis"
  "Will indicate regions foldable with hideshow in the fringe."
  'interactive)
(dolist (hook (list 'emacs-lisp-mode-hook
                    'c++-mode-hook
		    'cperl-mode-hook
		    'c-mode-hook))
  (add-hook hook 'hideshowvis-enable))


;; Eshell ---------------------------------------------------------------

;; ;; Execute time for each command
;; (add-hook 'eshell-load-hook
;;           (lambda()(setq last-command-start-time (time-to-seconds))))
;; (add-hook 'eshell-pre-command-hook
;;           (lambda()(setq last-command-start-time (time-to-seconds))))
;; (add-hook 'eshell-before-prompt-hook
;;           (lambda()
;;               (message "spend %g seconds"
;;                        (- (time-to-seconds) last-command-start-time))))

;; (defvar ac-source-eshell-pcomplete
;;   '((candidates . (pcomplete-completions))))
;; (defun ac-complete-eshell-pcomplete ()
;;   (interactive)
;;   (auto-complete '(ac-source-eshell-pcomplete)))
;; ;; 自动开启 ac-mode
;; ;; 需要 (global-auto-complete-mode 1)
;; (add-to-list 'ac-modes 'eshell-mode)
;; (setq ac-sources '(ac-source-eshell-pcomplete
;;                    ;; ac-source-files-in-current-dir
;;                    ;; ac-source-filename
;;                    ;; ac-source-abbrev
;;                    ;; ac-source-words-in-buffer
;;                    ;; ac-source-imenu
;; ))

;; ESS for R ----------------------------------------------------------

;; (add-to-list 'load-path "/home/darkjh/emacs-packages/ess-12.09-2/lisp/")
;; (require 'ess-site)


;; ;; Ibus input mode ----------------------------------------------------

;; ;; http://www.emacswiki.org/emacs/IBusMode
;; ;; 开启ibus 提供中文输入
;; (require 'ibus)
;; ;; 因为系统默认python3，所以给ibus专门设置python2

;; ;; 自动开启ibus，并且再每次启动emacsclient的时候也这么做
;; (add-hook 'after-init-hook 'ibus-mode-on)
;; (add-hook 'after-make-frame-functions
;; 	  (lambda (new-frame)
;; 	    (select-frame new-frame)
;; 	    (or ibus-mode (ibus-mode-on))))
;; ;; 保留选择和undo的快捷键，ibus暂时用鼠标来操纵
;; (ibus-define-common-key ?\C-\s nil)
;; (ibus-define-common-key ?\C-/ nil)
;; ;; ibus激活时游标为橙色，平时为黄色
;; (setq ibus-cursor-color '("orange" "yellow" "limegreen"))
;; ;; 使用win键（s）加space来切换ibus状态
;; (ibus-define-common-key ?\S-\s nil)
;; (global-set-key (kbd "s-SPC") 'ibus-toggle)

;; Org-Mode ------------------------------------------------------------

;; (require 'org-install)
;; (require 'org-publish)
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; (define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-ca" 'org-agenda)
;; (setq org-log-done t)
;; (add-hook 'org-mode-hook 'turn-on-font-lock)
;; (add-hook 'org-mode-hook
;; 	  (lambda () (setq truncate-lines nil)))
;; (global-set-key "\C-cb" 'org-iswitchb)
;; (setq org-directory "~/Dropbox/org-notes")
;; (setq org-mobile-directory "~/Dropbox/org-notes")
;; (setq org-mobile-inbox-for-pull "~/Dropbox/org-notes")

;; Word Count Mode -----------------------------------------------------

(autoload 'word-count-mode "word-count"
  "Minor mode to count words." t nil)
(global-set-key "\M-+" 'word-count-mode)


;; Octave Mode ---------------------------------------------------------

;; (autoload 'octave-mode "octave-mod" nil t)
;; (setq auto-mode-alist
;; (cons '("\\.m$" . octave-mode) auto-mode-alist))

;; (add-hook 'octave-mode-hook
;; (lambda ()
;; (abbrev-mode 1)
;; (auto-fill-mode 1)
;; (if (eq window-system 'x)
;; (font-lock-mode 1))))

;; ;; run an inferior Octave process in a special Emacs buffer
;; (autoload 'run-octave "octave-inf" nil t)

;; Ruby -----------------------------------------------------------------

;; (autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
;; (autoload 'inf-ruby-keys "inf-ruby" "" t)
;; (eval-after-load 'ruby-mode
;;   '(add-hook 'ruby-mode-hook 'inf-ruby-keys))

;; (require 'inf-ruby)
;; (autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
;; (autoload 'inf-ruby-setup-keybindings "inf-ruby" "" t)
;; (eval-after-load 'ruby-mode
;;   '(add-hook 'ruby-mode-hook 'inf-ruby-setup-keybindings))

;; Markdown -------------------------------------------------------------

(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
(add-hook 'markdown-mode-hook
	  '(lambda ()
	     (auto-fill-mode 0)))

;; HTML/xHtml ----------------------------------------------------------

;; (load "~/.emacs.d/vendors/nxhtml/autostart")

;; Common Lisp ---------------------------------------------------------

(setq inferior-lisp-program "/usr/bin/clisp")


;; GDB -----------------------------------------------------------------
(add-hook 'gdb-mode-hook '(lambda ()
                            (define-key c-mode-base-map [(f5)] 'gud-go)
                            (define-key c-mode-base-map [(f7)] 'gud-step)
                            (define-key c-mode-base-map [(f8)] 'gud-next)))


;; Tabbar mode  --------------------------------------------------------
(require 'tabbar)
(tabbar-mode 1)

;; http://blog.csdn.net/CherylNatsu/article/details/6204737
;; 设置tabbar外观
;; 设置默认主题: 字体, 背景和前景颜色，大小
(set-face-attribute 'tabbar-default nil
		    :family "Terminus"
		    :background "#272822"
		    :foreground "#F8F8F2"
		    :height 1.0)
;; 设置左边按钮外观：外框框边大小和颜色
(set-face-attribute 'tabbar-button nil
		    :inherit 'tabbar-default
		    :box '(:line-width 1 :color "#F0DFAF"))
;; 设置当前tab外观：颜色，字体，外框大小和颜色
(set-face-attribute 'tabbar-selected nil
		    :inherit 'tabbar-default
		    :foreground "black"
		    :background "white"
		    :box '(:line-width 2 :color "#F92672")
		    :overline "black"
		    :underline "black"
		    :weight 'bold)
;; 设置非当前tab外观：外框大小和颜色
(set-face-attribute 'tabbar-unselected nil
		    :inherit 'tabbar-default
		    :box '(:line-width 1 :color "#00B2BF"))

;; Undo-tree  --------------------------------------------------------
(require 'undo-tree)
(global-undo-tree-mode)

;; Dirtree  --------------------------------------------------------
(autoload 'dirtree "dirtree" "Add directory to tree view" t)

;; Scala mode  --------------------------------------------------------
;; (add-to-list 'load-path "~/.emacs.d/vendors/scala-mode2")
;; (require 'scala-mode2)

;; Golang
(require 'go-mode)
(autoload 'go-mode "go-mode" "" t nil)
(add-to-list 'auto-mode-alist (cons "\\.go\\'" 'go-mode))
(autoload 'godoc "go-mode" "" t nil)
(autoload 'go-download-play "go-mode" "" t nil)
(autoload 'gofmt-before-save "go-mode" "" t nil)
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda ()
			  (local-set-key (kbd "M-\.") 'godef-jump)))

;; display tab as 2 spaces in golang mode
(add-hook 'go-mode-hook
	  (lambda ()
	    (setq-default)
	    (setq tab-width 2)
	    (setq standard-indent 2)
	    (setq indent-tabs-mode nil)))

;; Auto-completion for Golang
(require 'go-autocomplete)
(require 'auto-complete-config)


;; Rust-lang
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
