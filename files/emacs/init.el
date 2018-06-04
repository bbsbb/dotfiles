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
    org
    paredit
    php-mode
    rainbow-delimiters
    rainbow-mode
    sass-mode
    smex
    terraform-mode
    tuareg
    web-mode
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
;;;;;;;;;;;;;;;;;;ORG MOD;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-directory "~/Dropbox/org")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-mobile-inbox-for-pull "~/Dropbox/org/inbox.org")
(setq org-mobile-files '("~/Dropbox/org"))
;;;;;;;;;;;;;;;;;;END;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;Enable paredit for some modes;;
(defun activate-paredit-mode-custom ()
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (global-set-key (kbd "C-l") 'paredit-forward-up)
  (paredit-mode 1)
  (show-paren-mode 1))
(defun jack-in-with-profile ()
  (interactive)
  (letrec ((profile (read-string "Profiles: "))
           (lein-params (concat "with-profile +" profile " repl :headless")))
    (set-variable 'cider-lein-parameters lein-params)
    (cider-jack-in)))
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
(global-prettify-symbols-mode 1)
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

;;Js dogshit.
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

(flycheck-add-mode 'javascript-eslint 'web-mode)
(setq-default flycheck-temp-prefix ".flycheck")
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
          '(json-jsonlist)))

(exec-path-from-shell-initialize)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  ;;(company-mode +1)
  )

;; aligns annotation to the right hand side
;; formats the buffer before saving
;;(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(add-to-list 'auto-mode-alist '("\\.tsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.test\\.tsx$" . web-mode))
(add-to-list 'ac-modes 'web-mode)
(add-to-list 'ac-modes 'typescript-mode)
;;(add-to-invisibility-spec 'ac-modes 'web-mode)
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)
;;end js dogshit.

(setq gofmt-command "goimports")
(add-hook 'before-save-hook #'gofmt-before-save)

;;;;;;;;;;;;;;;;;END;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
  '(clojure-align-forms-automatically t)
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "hicv" "pubilc")))
 '(package-selected-packages
   (quote
    (tide tuareg exec-path-from-shell esk yaml-mode smex sass-mode rainbow-mode rainbow-delimiters php-mode paredit org markdown-mode magit js2-mode jinja2-mode helm-projectile helm-ag groovy-mode golint go-mode git-gutter flycheck ensime elixir-mode ansible ac-cider))))
