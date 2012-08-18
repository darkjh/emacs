;; My emacs init file
;; JU Han
;; ju.han.felix@gmail.com

;; Load path ----------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d")
(progn (cd "~/.emacs.d")
        (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path "~/.emacs.d/vendors")
(progn (cd "~/.emacs.d/vendors")
        (normal-top-level-add-subdirs-to-load-path))

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


;; Set default browser to Chromium ------------------------------------
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium-browser")

;; (add-to-list 'default-frame-alist '(height . 42))
;; (add-to-list 'default-frame-alist '(width . 80))

;; Scheme -------------------------------------------------------------

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(quack-default-program "racket")
 '(quack-newline-behavior (quote indent-newline-indent))
 '(quack-programs (quote ("mzscheme" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(quack-switch-to-scheme-method (quote other-window))
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil nil (tool-bar)))
(require 'quack)
;; (set-face-attribute 'default nil :font "DejaVu Sans Mono-10")

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
 '(quack-pltish-comment-face ((((class color) (background dark)) (:foreground "red1"))))
 '(quack-pltish-defn-face ((t (:foreground "yellow1" :weight bold))))
 '(quack-pltish-keyword-face ((t (:foreground "darkturquoise" :weight bold))))
 '(quack-pltish-paren-face ((((class color) (background dark)) (:foreground "orange1")))))

;; Configurations for LaTex -------------------------------------------

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
        (output-pdf "PDF Viewer")
        (output-html "Google Chrome")))
(setq TeX-view-program-list
      '(("DVI Viewer" "evince %o")
        ("PDF Viewer" "zathura %o")
        ("Google Chrome" "google-chrome %o")))

(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook (lambda () (abbrev-mode +1)))

;; (load "preview-latex.el" nil t t)


;; Configurations for Python ------------------------------------------

(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;;pymacs
;; (require 'pymacs)
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t)


;; (require 'ipython)

(require 'lambda-mode)
(add-hook 'python-mode-hook #'lambda-mode 1)
(setq lambda-symbol (string (make-char 'greek-iso8859-7 107)))

;;set up Anything for compelation
;; (require 'anything)
;; (require 'anything-ipython)
;; (when (require 'anything-show-completion nil t)
;;    (use-anything-show-completion 'anything-ipython-complete
;;                                  '(length initial-pattern)))

;; (setq ipython-completion-command-string "print(';'.join(get_ipython().Completer.complete('%s')[1])) #PYTHON-MODE SILENT\n")


;;set up Comint
;; (require 'comint)
;; (define-key comint-mode-map (kbd "M-") 'comint-next-input)
;; (define-key comint-mode-map (kbd "M-") 'comint-previous-input)
;; (define-key comint-mode-map [down] 'comint-next-matching-input-from-input)
;; (define-key comint-mode-map [up] 'comint-previous-matching-input-from-input)


;; Autopair -----------------------------------------------------------

(autoload 'autopair-global-mode "autopair" nil t)
(autopair-global-mode t)
;; (add-hook 'lisp-mode-hook
;;           #'(lambda () (setq autopair-dont-activate t)))
;;Autopair for python
(add-hook 'python-mode-hook
          #'(lambda ()
               (push '(?' . ?')

                     (getf autopair-extra-pairs :code))
               (setq autopair-handle-action-fns
                     (list #'autopair-default-handle-action
                           #'autopair-python-triple-quote-action))))

;;delete trailing spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; Yasnippet ----------------------------------------------------------

(require 'yasnippet-bundle)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets/")


;; Auto complete ------------------------------------------------------

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendors/auto-complete-1.3/dict")
(ac-config-default)

;; Make Ac mode aware of latex
(require 'ac-math)
(add-to-list 'ac-modes 'latex-mode)

(defun ac-latex-mode-setup ()         ; add ac-sources to default ac-sources
  (setq ac-sources
	(append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
		ac-sources))
)
(add-hook 'LaTeX-mode-hook 'ac-latex-mode-setup)

;(autoload 'ac-latex-setup "auto-complete-latex" "ac and aucTeX" t)
;(add-hook 'LaTeX-mode-hook (lambda() (ac-latex-setup)))

(require 'smart-operator)


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

;; Execute time for each command
(add-hook 'eshell-load-hook
          (lambda()(setq last-command-start-time (time-to-seconds))))
(add-hook 'eshell-pre-command-hook
          (lambda()(setq last-command-start-time (time-to-seconds))))
(add-hook 'eshell-before-prompt-hook
          (lambda()
              (message "spend %g seconds"
                       (- (time-to-seconds) last-command-start-time))))

(defvar ac-source-eshell-pcomplete
  '((candidates . (pcomplete-completions))))
(defun ac-complete-eshell-pcomplete ()
  (interactive)
  (auto-complete '(ac-source-eshell-pcomplete)))
;; 自动开启 ac-mode
;; 需要 (global-auto-complete-mode 1)
(add-to-list 'ac-modes 'eshell-mode)
(setq ac-sources '(ac-source-eshell-pcomplete
                   ;; ac-source-files-in-current-dir
                   ;; ac-source-filename
                   ;; ac-source-abbrev
                   ;; ac-source-words-in-buffer
                   ;; ac-source-imenu
))

;; ESS for R ----------------------------------------------------------

(add-to-list 'load-path "/usr/share/emacs/site-lisp/ESS/lisp/")
(require 'ess-site)

;; Minimap ------------------------------------------------------------
(require 'minimap)

;; Prediction Mode ----------------------------------------------------

;; (add-to-list 'load-path "/home/darkjh/.emacs.d/vendors/predictive")
;; (add-to-list 'load-path "/homes/darkjh/.emacs.d/vendors/predictive/texinfo")
;; (add-to-list 'load-path "/home/darkjh/.emacs.d/vendors/predictive/html")
;; (add-to-list 'load-path "/home/darkjh/.emacs.d/vendors/predictive/latex")
;; (require 'predictive)


;; Ibus input mode ----------------------------------------------------

;; http://www.emacswiki.org/emacs/IBusMode
;; 开启ibus 提供中文输入
(require 'ibus)
;; 因为系统默认python3，所以给ibus专门设置python2
(custom-set-variables '(ibus-python-shell-command-name "/usr/bin/python2"))
;; 自动开启ibus，并且再每次启动emacsclient的时候也这么做
(add-hook 'after-init-hook 'ibus-mode-on)
(add-hook 'after-make-frame-functions
	  (lambda (new-frame)
	    (select-frame new-frame)
	    (or ibus-mode (ibus-mode-on))))
;; 保留选择和undo的快捷键，ibus暂时用鼠标来操纵
(ibus-define-common-key ?\C-\s nil)
(ibus-define-common-key ?\C-/ nil)
;; ibus激活时游标为橙色，平时为黄色
(setq ibus-cursor-color '("orange" "yellow" "limegreen"))
;; 使用win键（s）加space来切换ibus状态
(ibus-define-common-key ?\S-\s nil)
(global-set-key (kbd "s-SPC") 'ibus-toggle)


;; Org-Mode ------------------------------------------------------------

(require 'org-install)
(require 'org-publish)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook
	  (lambda () (setq truncate-lines nil)))
(global-set-key "\C-cb" 'org-iswitchb)


;; Word Count Mode -----------------------------------------------------

(autoload 'word-count-mode "word-count"
  "Minor mode to count words." t nil)
(global-set-key "\M-+" 'word-count-mode)


;; Octave Mode ---------------------------------------------------------

(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
(cons '("\\.m$" . octave-mode) auto-mode-alist))

(add-hook 'octave-mode-hook
(lambda ()
(abbrev-mode 1)
(auto-fill-mode 1)
(if (eq window-system 'x)
(font-lock-mode 1))))

;; Ruby -----------------------------------------------------------------

(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(autoload 'inf-ruby-keys "inf-ruby" "" t)
(eval-after-load 'ruby-mode
  '(add-hook 'ruby-mode-hook 'inf-ruby-keys))

;; Markdown -------------------------------------------------------------

(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
(add-hook 'markdown-mode-hook
	  '(lambda ()
	     (auto-fill-mode 0)))

;; Common Lisp ---------------------------------------------------------

(setq inferior-lisp-program "/usr/bin/clisp")


;; GDB -----------------------------------------------------------------
(add-hook 'gdb-mode-hook '(lambda ()
                            (define-key c-mode-base-map [(f5)] 'gud-go)
                            (define-key c-mode-base-map [(f7)] 'gud-step)
                            (define-key c-mode-base-map [(f8)] 'gud-next)))