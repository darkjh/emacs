 ;;My emacs init file

;;Load path
(add-to-list 'load-path "~/.emacs.d")
(progn (cd "~/.emacs.d")
        (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path "~/.emacs.d/vendors")
(progn (cd "~/.emacs.d/vendors")
        (normal-top-level-add-subdirs-to-load-path))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;User settings
(setq user-full-name "JU Han")
(setq user-mail-address "ju.han.felix@gmail.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Convenient set-ups
(show-paren-mode t)
(setq show-paren-style 'parentheses)
(mouse-avoidance-mode 'animate)
(setq frame-title-format "emacs@ %b")
(global-font-lock-mode t)
(setq scroll-mqrgin 3
      scroll-conservatively 10000)
(setq default-major-mode 'text-mode)
(require 'linum)
(global-linum-mode)


;;Set default browser to Chromium
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium-browser")

;;(add-to-list 'default-frame-alist '(height . 42))
;;(add-to-list 'default-frame-alist '(width . 168))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Scheme

(custom-set-variables
 '(quack-default-program "racket")
 '(quack-newline-behavior (quote indent-newline-indent))
 '(quack-programs (quote ("mzscheme" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(quack-switch-to-scheme-method (quote other-window))
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil nil (tool-bar)))
(require 'quack)
(set-face-attribute 'default nil :font "DejaVu Sans Mono-10")

(require 'scheme-complete)
(autoload 'scheme-smart-complete "scheme-complete" nil t)
(eval-after-load 'scheme
   '(progn (define-key scheme-mode-map "\t" 'scheme-complete-or-indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Color theme

(require 'color-theme)
(setq color-theme-is-global t)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-orico-black)))
(custom-set-faces
 '(quack-pltish-comment-face ((((class color) (background dark)) (:foreground "red1"))))
 '(quack-pltish-defn-face ((t (:foreground "yellow1" :weight bold))))
 '(quack-pltish-keyword-face ((t (:foreground "darkturquoise" :weight bold))))
 '(quack-pltish-paren-face ((((class color) (background dark)) (:foreground "orange1")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Configurations for LaTex

(load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Configurations for Python

(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;;pymacs
;; (require 'pymacs)
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t)

;;autocomplete
(add-to-list 'load-path "~/.emacs.d/vendors/auto-complete-1.3")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendors/auto-complete-1.3/dict")
(ac-config-default)

(require 'ipython)

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


;;Autopair

(autoload 'autopair-global-mode "autopair" nil t)
(autopair-global-mode)
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

;;Yasnippet
(require 'yasnippet-bundle)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets/")


;(require 'auto-complete)
;(global-auto-complete-mode t)

;(require 'smart-operator)
;;(require 'auto-complete-config)

;; (add-hook 'after-init-hook 'session-initialize)
;; (load "wcy-desktop")
;; (wcy-desktop-init)

;; ido-mode improve open file and change frame experience
(when (fboundp 'ido-mode)
  (ido-mode t)
  (setq ido-save-directory-list-file nil))

;; Javascript mode -- js2-mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; C Programming Languauge
(require 'cc-mode)
;;(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
(define-key c-mode-map [return] 'newline-and-indent)

;; Perl Programming
;; Use cperl-mode instead of the default perl-mode
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

;; Code folding
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


