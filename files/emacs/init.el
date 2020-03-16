;;; Code:
(require 'package)

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

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
    exec-path-from-shell
    flycheck
    flymake-eslint
    geiser
    git-gutter
    golint
    go-mode
    graphql-mode
    jinja2-mode
    js2-mode
    haskell-mode
    helm
    helm-ag
    helm-projectile
    lsp-mode
    magit
    markdown-mode
    org
    paredit
    php-mode
    racket-mode
    rainbow-delimiters
    rainbow-mode
    rjsx-mode
    sass-mode
    smex
    tide
    terraform-mode
    tuareg
    yaml-mode
    yasnippet
    use-package))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;;;;;;;;;;;;;;;;END;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;AUTO COMPLETE CONFIGURATION;;;;
(add-to-list 'load-path "~/.emacs.d/elisp/")
(require 'go-autocomplete)
(require 'auto-complete-config)
(add-hook 'go-mode-hook
          (lambda()
            (make-local-variable 'ac-auto-start)
            (make-local-variable 'ac-trigger-key)
            (setq ac-auto-start nil)
            (setq ac-trigger-key "TAB")))

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

;;;;;;;;;;;;;;;;;;END;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;Cider integration + profile jack in.

(defun jack-in-with-profile ()
  (interactive)
  (letrec ((profile (read-string "Profiles: "))
           (lein-params (concat "with-profile +" profile " repl :headless")))
    (set-variable 'cider-lein-parameters lein-params)
    (cider-jack-in)))

(setq  cider-refresh-before-fn "integrant.repl/suspend")
(setq  cider-refresh-after-fn "integrant.repl/resume")

;;;;;;;;;;;;;;;;;;END;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;Helm & Projectile;;;;;;;;;;;;;;;;
(require 'helm-config)
(helm-mode 1)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(setq projectile-enable-caching nil)
(helm-projectile-on)
;;(setq helm-ag-use-grep-ignore-list t)
(setq helm-ag-use-agignore t)
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
(global-prettify-symbols-mode 1)

;;Paredit config, sorry
;;Spaces please.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-basic-offset 4)
(setq c-basic-indent 4)

;;Always show row position
(column-number-mode t)

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

;;Js dogshit.
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

(setq-default flycheck-temp-prefix ".flycheck")
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
          '(json-jsonlist)))

(defun loljs/force-eslint ()
  (flycheck-mode t)
  (flycheck-select-checker 'javascript-eslint))

(defun loljs/local-eslint ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint
          (and root
               (expand-file-name "node_modules/.bin/eslint"
                                 root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'js2-mode-hook #'loljs/force-eslint)
(add-hook 'flycheck-mode-hook #'loljs/local-eslint)

;; lol macs
(exec-path-from-shell-initialize)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

;; Fucking racket, man.
(setenv "PATH" (concat (getenv "PATH") ":/usr/racket/bin"))
(setq exec-path (append exec-path '("/usr/racket/bin")))


(use-package lsp-mode
  :ensure t
  :hook ((clojure-mode . lsp))
  :commands lsp
  :custom
  ((lsp-clojure-server-command '("java" "-jar" "/usr/local/bin/clj-kondo-lsp")))
  :config
  (dolist (m '(clojure-mode
               clojurescript-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure"))))


(setq gofmt-command "goimports")
(add-hook 'before-save-hook #'gofmt-before-save)

;;;;;;;;;;;;;;;;;END;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 '(clojure-align-forms-automatically t)
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "hicv" "pubilc" "target"))))
