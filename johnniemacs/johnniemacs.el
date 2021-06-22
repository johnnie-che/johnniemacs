(provide 'johnniemacs)

;; Add MELPA repo and refresh
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Install use-package
(condition-case nil
    (require 'use-package)
  (file-error
   (package-initialize)
   (package-refresh-contents)
   (package-install 'use-package)
   (require 'use-package)))

(require 'johnniemacs-powerline)
(johnniemacs/powerline)

(use-package restart-emacs :ensure t)

;;------
;; Evil
;;------
(use-package evil :ensure t)
(require 'evil)
(evil-mode 1)

;; Auto complete
;;(use-package auto-complete :ensure t)
;;(ac-config-default)

(use-package lsp-ui 
  :ensure t
  :config
  (progn
	(setq lsp-ui-doc-enable nil))
  :bind
  	(:map lsp-mode-map ("C-M-a" . lsp-ui-sideline-apply-code-actions)))
(use-package company :ensure t) ;; Auto-complete

;; Dart
(use-package lsp-mode :ensure t)
(use-package lsp-dart
  :ensure t
  :hook (dart-mode . lsp))

(use-package flutter 
  :ensure t
  :after dart-mode
  :bind (:map dart-mode-map ("C-M-r" . #'flutter-run-or-hot-reload)))

;; Yaml
(use-package yaml-mode :ensure t)

;;-------
;; Swift
;;-------

(use-package swift-mode
  :ensure t)
(use-package flycheck-swift
  :ensure t
  :after swift-mode)
(use-package swift-helpful
  :ensure t
  :after swift-mode)
(use-package lsp-sourcekit
  :ensure t)
(use-package company-sourcekit
  :ensure t)

;;---------
;; Treemacs
;;----------
(use-package treemacs 
  :ensure t
  :config
  (progn
	(setq treemacs-show-cursor nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;;(treemacs-modify-theme "Default"
;;  :config
;;  (progn
;;    (treemacs-create-icon :icon " " :fallback " 📄 " :extensions ("dart"))
;;    (treemacs-create-icon :icon ">" :fallback " 📁 " :extensions (dir-closed))
;;    (treemacs-create-icon :icon "v" :fallback " 📂 " :extensions (dir-open))))

(use-package lsp-treemacs 
  :ensure t)
(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

;;----------
;; PlantUML
;;----------

(use-package plantuml-mode
  :ensure t
  :config
  (progn
    (setq plantuml-executable-path "/opt/homebrew/bin/plantuml")
    (setq plantuml-default-exec-mode 'executable)))
(add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))

;;---------
;; Other
;;---------
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;;(unless (display-graphic-p) (setf ns-command-modifier 'super))
(setq mac-command-modifier 'super)

(menu-bar-mode -1)

