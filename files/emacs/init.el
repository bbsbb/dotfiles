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
    cider
    clojure-mode
    company
    elixir-mode
    exec-path-from-shell
    flycheck
    flymake-eslint
    geiser
    git-gutter
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
(setq use-package-always-ensure t)

;;START:  LSP + Go
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package company
  :diminish
  :config
  (global-company-mode 1)
  (setq ;; Only 2 letters required for completion to activate.
   company-minimum-prefix-length 2

   ;; Search other buffers for compleition candidates
   company-dabbrev-other-buffers t
   company-dabbrev-code-other-buffers t
   company-complete-number t
   company-dabbrev-downcase nil
   company-dabbrev-ignore-case t

   ;; Immediately activate completion.
   company-idle-delay 0)

  (define-key company-active-map (kbd "TAB") #'company-complete-selection)
  (define-key company-active-map (kbd "SPC") nil)
  ;; AC on everything please.
  (setq company-auto-complete-chars nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-tooltip ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white")))))

(global-git-gutter-mode t)

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
;;(setq js-indent-level 4)

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

(add-hook 'cider-mode-hook 'ac-flyspell-workaround)

(add-hook 'emacs-startup-hook 'setup-workspace)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'activate-paredit-mode-custom)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

;; lol mac os & racket
(exec-path-from-shell-initialize)
(setenv "PATH" (concat (getenv "PATH") ":/usr/racket/bin"))
(setq exec-path (append exec-path '("/usr/racket/bin")))

;; LSP + Clojure + CLJ-Kondo
(use-package lsp-mode
  :ensure t
  :hook ((clojure-mode . lsp))
  :commands lsp
  :custom ((lsp-clojure-server-command '("java" "-jar" "/home/zee/bin/clj-kondo-lsp")))
  :config (dolist (m '(clojure-mode
                       clojurescript-mode))
            (add-to-list 'lsp-language-id-configuration `(,m . "clojure"))))

;;;;;;;;;;;;;;;;;END;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(clojure-align-forms-automatically t)
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "hicv" "pubilc" "target"))
 '(package-selected-packages
   '(json-mode flycheck-clj-kondo use-package lsp-mode rjsx-mode graphql-mode racket-mode geiser flymake-eslint tide tuareg exec-path-from-shell yaml-mode smex sass-mode rainbow-mode rainbow-delimiters php-mode paredit org markdown-mode magit js2-mode jinja2-mode helm-projectile helm-ag groovy-mode go-mode git-gutter flycheck elixir-mode ansible)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
