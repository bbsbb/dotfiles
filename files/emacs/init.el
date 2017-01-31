;;; Code:
(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                     ("marmalade" . "http://marmalade-repo.org/packages/")
                     ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

;;;;;;;;;;;;;;;;;;END;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;PACKAGES;;;;;;;;;;;;;;;;;;;;;;;
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(ansible
    ac-cider
    auto-complete
    cider
    clojure-mode
    elixir-mode
    ensime
    flycheck
    git-gutter
    golint
    go-mode
    jinja2-mode
    js2-mode
    helm
    helm-ag
    helm-projectile
    magit
    markdown-mode
    paredit
    php-mode
    rainbow-delimiters
    rainbow-mode
    sass-mode
    smex
    yaml-mode
    yasnippet))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;;;;;;;;;;;;;;;;END;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;AUTO COMPLETE CONFIGURATION;;;;
(add-to-list 'load-path "~/.emacs.d/elisp/")
(require 'go-autocomplete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(global-git-gutter-mode t)
(setq ac-quick-help-delay 0.5)
(setq ac-auto-start 1)
(setq ac-ignore-case nil)
(ac-config-default)
;;;;;;;;;;;;;;;;;;END;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;Enable paredit for some modes;;
(defun activate-paredit-mode-custom ()
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (global-set-key (kbd "C-l") 'paredit-forward-up)
  (paredit-mode 1)
  (show-paren-mode 1))

;;;;;;;;;;;;;;;;;;END;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;Helm & Projectile;;;;;;;;;;;;;;;;
(require 'helm-config)
(helm-mode 1)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(setq projectile-enable-caching nil)
(helm-projectile-on)
(setq helm-display-header-line nil)
(setq helm-split-window-in-side-p t)
(setq helm-autoresize-max-height 30)
(setq helm-autoresize-min-height 30)

;;This is bad, but I am used to different behavior on C-x f
(ido-mode t)
;;;;;;;;;;;;;;;;;;END;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;GLOBAL CONFIUGURATION;;;;;;;;;;;;
;;Too used to those shortcuts.
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x h") 'helm-projectile-find-file) ; Search for file
(global-set-key (kbd "C-x r") 'helm-projectile-ag)           ; Search in files
(global-set-key (kbd "C-c C-d") 'make-directory)
(global-set-key (kbd "C-c C-x") 'delete-directory)
;;Paredit config, sorry
;;Spaces please.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-basic-offset 4)
(setq c-basic-indent 4)

;;Always use snippets
(yas-global-mode 1)

;;Clean up after us. Those features often cause issues
;;with live reload tasks in certain languages.
(setq make-backup-files nil)
(setq auto-save-default nil)

;;;;;;;;;;;;;;;;;;END;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;WORKSPACE/GENERAL;;;;;;;;;;;;;;;;;;;;;;
(defun setup-workspace ()
  "Setup the workspace."
  (interactive)
  (split-window-vertically)
  (enlarge-window 5))

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;;;;;;;;;;;;;;;;;HOOKS;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Delimiter coloring for code.
(require 'ac-cider)

(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)

(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
          (add-to-list 'ac-modes 'cider-repl-mode)))

(add-hook 'emacs-startup-hook 'setup-workspace)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'activate-paredit-mode-custom)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;;;;;;;;;;;;;;;;;END;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 '(clojure-align-forms-automatically t)
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "hicv"))))
(custom-set-faces
 )
