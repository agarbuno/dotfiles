;; NOTE: init.el is now generated from Emacs.org.  Please edit that file
;;       in Emacs and init.el will be generated automatically!

;; You will most likely need to adjust this font size for your system!
(defvar ag/default-font-size 150)
(defvar ag/default-variable-font-size 150)

;; Make frame transparency overridable
(defvar ag/frame-transparency '(90 . 90))

(setq gc-cons-threshold (* 2 1000 1000))

(defun ag/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'ag/display-startup-time)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

    ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
    (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose t)

(use-package no-littering)

(setq no-littering-etc-directory
      (expand-file-name "config/" user-emacs-directory))
(setq no-littering-var-directory
      (expand-file-name "data/" user-emacs-directory))
(require 'no-littering)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(defun ag/immortal-buffers ()
  (if (or (eq (current-buffer) (get-buffer "*scratch*"))
          (eq (current-buffer) (get-buffer "*Messages*")))
      (progn (bury-buffer)
             nil)
    t))

(add-hook 'kill-buffer-query-functions 'ag/immortal-buffers)

(setq save-interprogram-paste-before-kill t)
(setq delete-selection-mode 1)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil 'invert-face 'mode-line)))


(column-number-mode)
(global-display-line-numbers-mode t)

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha ag/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,ag/frame-transparency))
(setq mac-command-modifier 'super)
(setq mac-option-modifier  'meta)

;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized) 
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                deft-mode-hook
                shell-mode-hook
                reftex-select-bib-mode-hook
                pdf-outline-buffer-mode-hook
                org-agenda-mode-hook
                pdf-view-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0)))
  )

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  ;; (setq dashboard-page-separator "\n\f\n")
  (setq dashboard-startup-banner 'logo)  
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (dashboard-modify-heading-icons '((projects . "rocket")
                                    (agenda . "milestone")
                                    (recents . "history")
                                    (bookmarks . "bookmark")))
  (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
  (setq dashboard-items '(
                          (recents  . 10)
                          (projects . 5)
                          (bookmarks . 5)
                          (agenda . 10)
                          ))
  (setq dashboard-navigator-buttons
      `((;; Github
         (,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
          "Github"
          "Go to Github"
          (lambda (&rest _) (browse-url "https://github.com/agarbuno/")))
         ;; Perspectives
         (,(all-the-icons-octicon "history" :height 1.1 :v-adjust 0.0)
          "Restore"
          "Restore window configuration"
          (lambda (&rest _) (persp-state-load persp-state-default-file)))
         )))
  )

(set-face-attribute
 'default nil
 :font "Fira Code Retina"
 :height ag/default-font-size)

;; Set the fixed pitch face
(set-face-attribute
 'fixed-pitch nil
 :font "Fira Code Retina"
 :height ag/default-font-size)

;; Set the variable pitch face
(set-face-attribute
 'variable-pitch nil
 :font "Cantarell"
 :height ag/default-font-size
 :weight 'regular)

(use-package solaire-mode
  :config
  (solaire-global-mode 1)
  )

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; Adds accents in spanish
(global-set-key (kbd "M-a") '(lambda () (interactive) (insert "á")))
(global-set-key (kbd "M-e") '(lambda () (interactive) (insert "é")))
(global-set-key (kbd "M-i") '(lambda () (interactive) (insert "í")))
(global-set-key (kbd "M-o") '(lambda () (interactive) (insert "ó")))
(global-set-key (kbd "M-u") '(lambda () (interactive) (insert "ú")))
(global-set-key (kbd "M-y") '(lambda () (interactive) (insert "ü")))
(global-set-key (kbd "M-n") '(lambda () (interactive) (insert "ñ")))
(global-set-key (kbd "s-/") '(lambda () (interactive) (insert "¿")))
;; For macOS type of keybindings
(global-set-key (kbd "<s-up>")    'beginning-of-buffer)
(global-set-key (kbd "<s-down>")  'end-of-buffer)
(global-set-key (kbd "<s-left>")  'beginning-of-line)
(global-set-key (kbd "<s-right>") 'end-of-line)
;;
(global-set-key (kbd "s-u") 'revert-buffer)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-l") 'goto-line)
(global-set-key (kbd "s-s") 'save-buffer)

(use-package general
  :after evil
  :config
  (general-create-definer ag/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-c h")

  (ag/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    ;; Load emacs config 
    "fe" '(lambda () (interactive) (find-file (expand-file-name "~/.dotfiles/emacs.org")))  
    "fs" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/init.el")))
    ;; Visual toggle
    "v"  '(:ignore t :which-key "visual")
    "vl" '(org-toggle-link-display :which-key "toggle links")
    "vs" '(org-display-inline-images :which-key "toggle images")
    ))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-initial-state 'text-mode 'emacs)
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package command-log-mode
  :commands command-log-mode)

(defun ag/org-color-setup()
  (set-face-attribute 'org-block nil :background
                      (color-darken-name
                       (face-attribute 'default :background) 5))
  (set-face-attribute 'org-block-begin-line nil :background
                        (color-darken-name
                         (face-attribute 'default :background) -10))
  )


(use-package doom-themes
  ;; :init (load-theme 'doom-monokai-pro t))
  :init (load-theme 'doom-snazzy t))
;; :config (load-theme 'doom-nord t))

(use-package color
  :after org
  :config
  (set-face-attribute 'org-block nil :background
                      (color-darken-name
                       (face-attribute 'default :background) 5))
  (set-face-attribute 'org-block-begin-line nil :background
                      (color-darken-name
                       (face-attribute 'default :background) -10))
  )

(use-package all-the-icons)

(use-package doom-modeline
    :init (doom-modeline-mode 1)
    :config
    (setq doom-modeline-height 25)
    ;; (setq display-battery-mode t)
    (setq display-time-mode nil)
    (setq display-time-24hr-format 1)
    (setq display-time-day-and-date 1)
    (setq find-file-visit-truename t)
    (setq doom-modeline-project-detection 'project)
    (setq doom-modeline-project-detection 'ffip)
    )

(use-package minions
  :config 
  (setq doom-modeline-minor-modes t)
  (minions-mode 1)
  )

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)

         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  ;; (message "Ivy got loaded!")
  (ivy-mode 1))


(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

(use-package all-the-icons-ivy-rich
  :after ivy
  :init
  (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after all-the-icons-ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(ag/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1))

(setq prescient-sort-length-enable nil)
(setq ivy-prescient-retain-classic-highlighting t)

(use-package beacon
  :ensure t 
  :config
  (beacon-mode 1)
  (setq beacon-blink-when-focused 1)
  (setq beacon-size 70)
  )

(use-package emacs
  :config
  (setq-default indent-tabs-mode nil)
  (setq tab-width 4)
  (setq-default tab-always-indent 'complete)
)

(use-package perspective
  :ensure t
  :custom
  (persp-mode-prefix-key (kbd "C-x x"))
  :bind
  (("C-x k" . persp-kill-buffer*)
   ("C-x b" . persp-ivy-switch-buffer))
  :init
  (persp-mode)
  (setq persp-save-dir "~/.emacs.d/perspective/")
  (setq persp-state-default-file "~/.emacs.d/perspective/workflow.persp")
  (setq persp-sort 'created)
  )

(defun ag/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org-fancy-priorities ; priority icons
  :hook (org-mode . org-fancy-priorities-mode)
  :config (setq org-fancy-priorities-list '("⚑" "⬆" "⬇")))

(defun ag/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (ag/org-font-setup)
  (ag/org-color-setup)
  (setq fill-column 80))

(use-package org
  :commands (org-capture org-agenda)
  :hook ((org-mode . ag/org-mode-setup)
         (org-mode . ag/org-reveal))
  :config
  (setq org-ellipsis " ▾")
  (setq org-support-shift-select t)
  (setq org-latex-classes nil)

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
      '("~/orgfiles/agenda/tasks.org"
        "~/orgfiles/agenda/habits.org"))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAIT(w)" "|" "BACKLOG(b)" "CANCEL(c)"))
        )

  (setq org-refile-targets
        '(("archive.org" :maxlevel . 1)
          ("tasks.org" :maxlevel . 1)))

  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "hot pink" :weight bold))
          ("DONE" . (:foreground "#00e6ab" :weight bold))
          ("NEXT" . (:foreground "dark orange" :weight bold))
          ("WAIT" . (:foreground "#aeffff" :weight bold))
          ("CANCEL" . (:foreground "dark red" :weight bold :strike-through t))
          ("BACKLOG" . (:foreground "dark gray" :weight bold :underline t))
          ))

  (setq org-tag-alist
        '((:startgroup)
          ;; Put mutually exclusive tags here
          (:endgroup)
          ("admin" . ?a)
          ("book"  . ?b)
          ("course" . ?c)
          ("exam" . ?e)
          ("hw"   . ?h)
          ("papers" . ?p)
          ("read" . ?r)
          ("study"  . ?s)
          ("write"  . ?w)
          ))

;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)
(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ((agenda "/!-BACKLOG" (
                      (org-agenda-start-on-weekday 1)
                      (org-agenda-remove-tags t)
                      (org-agenda-show-inherited-tags nil)
                      (org-agenda-prefix-format "   %-2i ")
                      (org-deadline-warning-days 7)
                      ))
          (todo "NEXT"
                ((org-agenda-overriding-header "Ongoing Tasks")
                 (org-agenda-sorting-strategy '(priority-down))
                 (org-agenda-show-inherited-tags nil)
                 (org-agenda-prefix-format "   %-2i ")))
          (tags-todo "+research/!-NEXT"
                     ((org-agenda-overriding-header "1. Research")
                      (org-agenda-show-inherited-tags nil)
                      (org-agenda-prefix-format "   %-2i ")))
          (tags-todo "+teaching/!-NEXT"
                     ((org-agenda-overriding-header "2. Teaching")
                      (org-agenda-show-inherited-tags nil)
                      (org-agenda-prefix-format "   %-2i ")))            
          (tags-todo "+mcdatos/!-NEXT"
                     ((org-agenda-overriding-header "3. Maestria")
                      (org-agenda-show-inherited-tags nil)
                      (org-agenda-prefix-format "   %-2i ")))
          (tags-todo "+projects/!-NEXT"
                     ((org-agenda-overriding-header "4. Projects")
                      (org-agenda-show-inherited-tags nil)
                      (org-agenda-prefix-format "   %-2i ")))
          (todo "BACKLOG|BACK"
                ((org-agenda-overriding-header "5. Backlog")
                 (org-agenda-show-inherited-tags nil)
                 (org-agenda-prefix-format "   %-2i ")))

          (tags-todo "-research-teaching-mcdatos-projects/!-NEXT"
                     ((org-agenda-overriding-header "Unprocessed Inbox Tasks")
                      (org-agenda-show-inherited-tags nil)
                      (org-agenda-prefix-format "   %-2i ")
                      ;; (org-agenda-files "~/orgfiles/agenda/tasks.org")
                      (org-agenda-text-search-extra-files nil)
                      ))
          ))

        ("n" "Next Tasks"
         ((todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")))))

        ("W" "Work Tasks" tags-todo "+work-email")
        ))

(setq org-capture-templates
      `(
        ("m" "mcdatos") ;; ====================================================
          ("ma" "admin" entry
           (file+olp "~/orgfiles/agenda/tasks.org" "MCDATOS")
           "* TODO %? \t :admin:\n  %U\n  %a\n  %i" :empty-lines 1)
          ("ms" "students" entry
           (file+olp "~/orgfiles/agenda/tasks.org" "MCDATOS")
           "* TODO %? \t :students:\n  %U\n  %a\n  %i" :empty-lines 1)
          ("mp" "prospects" entry
           (file+olp "~/orgfiles/agenda/tasks.org" "MCDATOS")
           "* TODO %? \t :prospects:\n  %U\n  %a\n  %i" :empty-lines 1)
        ("r" "research") ;; ===================================================
          ("ra" "admin" entry
           (file+olp "~/orgfiles/agenda/tasks.org" "Research")
           "* TODO %? \t :admin:\n  %U\n  %a\n  %i" :empty-lines 1)
          ("ri" "ideas" entry 
           (file+olp "~/orgfiles/agenda/tasks.org" "Research")
           "* TODO %? \t :ideas:\n  %U\n  %a\n  %i" :empty-lines 1)
          ("rr" "read" entry
           (file+olp "~/orgfiles/agenda/tasks.org" "Research")
           "* TODO %? \t :read:\n  %U\n  %a\n  %i" :empty-lines 1)
          ("rs" "study" entry
           (file+olp "~/orgfiles/agenda/tasks.org" "Research")
           "* TODO %? \t :study:\n  %U\n  %a\n  %i" :empty-lines 1)
          ("rw" "write" entry
           (file+olp "~/orgfiles/agenda/tasks.org" "Research")
           "* TODO %? \t :write:\n  %U\n  %a\n  %i" :empty-lines 1)
        ("t" "teaching") ;; ===================================================
          ("ta" "admin" entry
           (file+olp "~/orgfiles/agenda/tasks.org" "Teaching")
           "* TODO %? \t :admin:\n  %U\n  %a\n  %i" :empty-lines 1)
          ("tc" "class" entry
           (file+olp "~/orgfiles/agenda/tasks.org" "Teaching")
           "* TODO %? \t :class:\n  %U\n  %a\n  %i" :empty-lines 1)
          ("tg" "grades" entry
           (file+olp "~/orgfiles/agenda/tasks.org" "Teaching")
           "* TODO %? \t :grades:\n  %U\n  %a\n  %i" :empty-lines 1)
          ("tt" "thesis" entry
           (file+olp "~/orgfiles/agenda/tasks.org" "Teaching")
           "* TODO %? \t :thesis:\n  %U\n  %a\n  %i" :empty-lines 1)
        ("p" "projects") ;; ===================================================
          ("pa" "admin" entry
           (file+olp "~/orgfiles/agenda/tasks.org" "Projects")
           "* TODO %? \t :admin:\n  %U\n  %a\n  %i" :empty-lines 1)
          ("pi" "ideas" entry
           (file+olp "~/orgfiles/agenda/tasks.org" "Projects")
           "* TODO %? \t :ideas:\n  %U\n  %a\n  %i" :empty-lines 1)
          ("pf" "followup" entry
           (file+olp "~/orgfiles/agenda/tasks.org" "Projects")
           "* TODO %? \t :followup:\n  %U\n  %a\n  %i" :empty-lines 1)
          ))

(define-key global-map (kbd "C-c t t")
  (lambda () (interactive) (org-capture nil "tt")))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c t c") 'org-capture)

(require 'org-habit)
(setq org-habit-show-all-today t)
(setq org-habit-today-glyph ?◌)
(setq org-habit-graph-column 40)
(setq org-habit-following-days 1)
(setq org-habit-show-habits t)
(setq org-habit-completed-glyph ?●)
(setq org-habit-preceding-days 10)
(setq org-habit-show-habits-only-for-today t)

) ;; Termina configuracion

(setq org-agenda-category-icon-alist
        `(;; Main categories =================================================
          ("research" ,(list (all-the-icons-faicon "rocket")) nil nil :ascent center)
          ("mcdatos" ,(list (all-the-icons-material "data_usage")) nil nil :ascent center)
          ("projects" ,(list (all-the-icons-material "group")) nil nil :ascent center)
          ("teaching" ,(list (all-the-icons-material "school")) nil nil :ascent center)
          ("email" ,(list (all-the-icons-material "email")) nil nil :ascent center)
          ;; Subcategories ===================================================
          ("paper" ,(list (all-the-icons-octicon "file-pdf")) nil nil :ascent center)
          ("book" ,(list (all-the-icons-faicon "book")) nil nil :ascent center)
          )
        )

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun ag/org-mode-visual-fill ()
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . ag/org-mode-visual-fill))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (latex . t)
   (R . t)
   (python . t)
   (sql . t)
   (shell . t)
   (octave . t)
   ))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("la" . "src latex"))
  (add-to-list 'org-structure-template-alist '("r" . "src R"))
  (add-to-list 'org-structure-template-alist '("co" . "src conf"))
  (add-to-list 'org-structure-template-alist '("p" . "src python"))
  (add-to-list 'org-structure-template-alist '("oct" . "src octave"))
  )

(push '("conf-unix" . conf-unix) org-src-lang-modes)
(setq org-confirm-babel-evaluate nil)
(setq org-src-window-setup 'split-window-below)
(add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
(setq org-src-tab-acts-natively t)

;; Automatically tangle our Emacs.org config file when we save it
(defun ag/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.dotfiles/emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'ag/org-babel-tangle-config)))

(setq org-hide-emphasis-markers t)
(use-package org-appear
      :hook (org-mode . org-appear-mode))

(use-package org-roam
  :init
   (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/orgfiles/"))
  (org-roam-completion-everywhere t)
  ;; (org-roam-completion-system 'default)
  ;; Capture templates
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "pages/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     )
   )
  ;; Dailies templates
  (org-roam-dailies-directory "journals/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "\n*  %?"
      :if-new (file+head
               "%<%Y-%m-%d>.org"
               "#+title: %<%Y-%m-%d %a>\n#+filetags: :journal:\n"))
     ("t" "talks" entry
      "\n* %^{Talk Title} by %^{Speaker} \t:talks: \n\n%?\n\n"
      :if-new (file+head+olp
               "%<%Y-%m-%d>.org"
               "#+title: %<%Y-%m-%d %a>\n#+filetags: :journal:\n"
               ("Talks")))
     ("m" "meeting" entry
      "\n*  %<%I:%M %p> - %^{Meeting Title} \t:meetings: \n\n%?\n\n"
      :if-new (file+head+olp
               "%<%Y-%m-%d>.org"
               "#+title: %<%Y-%m-%d %a>\n#+filetags: :journal:\n"
               ("Meetings")))
     ("s" "students" entry
      "\n*  %<%I:%M %p> - Monitoring development: %^{Student's Name} \t:students:\n\n%?\n\n"
      :if-new (file+head+olp
               "%<%Y-%m-%d>.org"
               "#+title: %<%Y-%m-%d %a>\n#+filetags: :journal:\n"
               ("Students")))
     ))

:bind (("C-c n b" . org-roam-buffer-toggle)
       ("C-c n f" . org-roam-node-find)
       ("C-c n g" . org-roam-graph)
       ("C-c n i" . org-roam-node-insert)
       ("C-c n c" . org-roam-capture)
       ("C-c n t" . org-roam-tag-add)
       ("C-c n r" . org-roam-tag-remove)
       ("C-c n k" . org-id-get-create)
       ;; Dailies
       ("C-c n m" . org-roam-dailies-capture-today)
       :map org-mode-map
       ("C-M-i"   . completion-at-point)
       )

:config
(org-roam-db-autosync-mode)
(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))

(cl-defmethod org-roam-node-filetitle ((node org-roam-node))
  "Return the file TITLE for the node."
  (org-collect-kewords "TITLE" (org-roam-node-file node))
  )

(cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
  (let* ((count (caar (org-roam-db-query
                       [:select (funcall count source)
                                :from links
                                :where (= dest $s1)
                                :and (= type "id")]
                       (org-roam-node-id node)))))
    (if (> count 0)
        (concat (propertize "=@=" 'display (all-the-icons-material "link" :face 'all-the-icons-dblue :height 0.9)) (format "%3d" count))
      (concat (propertize "=@=" 'display (all-the-icons-material "link" :face 'org-roam-dim :height 0.9))  "   ")
      ))
  )

(cl-defmethod org-roam-node-functiontag ((node org-roam-node))
  "The first tag of notes are used to denote note type"
  (let* ((specialtags ag/lit-categories)
         (tags (seq-filter (lambda (tag) (not (string= tag "ATTACH"))) (org-roam-node-tags node)))
         (functiontag (seq-intersection specialtags tags 'string=)))
    (concat
     (if functiontag
         (cond ((member "paper" functiontag)
                (propertize "=@=" 'display (all-the-icons-faicon "file-pdf-o" :face 'all-the-icons-dgreen :v-adjust 0.02 :height 0.8)))
               ((member "journal" functiontag)
                (propertize "=@=" 'display (all-the-icons-faicon "clock-o" :face 'all-the-icons-dgreen :v-adjust 0.02 :height 0.8)))
               ((member "thesis" functiontag)
                (propertize "=@=" 'display (all-the-icons-octicon "mortar-board" :face 'all-the-icons-dgreen :v-adjust 0.02 :height 0.8)))
             ((member "conferences" functiontag)
                  (propertize "=@=" 'display (all-the-icons-octicon "megaphone" :face 'all-the-icons-dgreen :v-adjust 0.02 :height 0.8)))
               ((member "book" functiontag)
                (propertize "=@=" 'display (all-the-icons-faicon "book" :face 'all-the-icons-dgreen :v-adjust 0.02 :height 0.8)))
               ((member "online" functiontag)
                (propertize "=@=" 'display (all-the-icons-faicon "globe" :face 'all-the-icons-dgreen :v-adjust 0.02 :height 0.8)))
               ((member "meetings" functiontag)
                (propertize "=@=" 'display (all-the-icons-octicon "broadcast" :face 'all-the-icons-dgreen :v-adjust 0.02 :height 0.8)))
               ((member "courses" functiontag)
                (propertize "=@=" 'display (all-the-icons-octicon "mortar-board" :face 'all-the-icons-dgreen :v-adjust 0.02 :height 0.8)))
               ((member "projects" functiontag)
                (propertize "=@=" 'display (all-the-icons-octicon "puzzle" :face 'all-the-icons-dgreen :v-adjust 0.02 :height 0.8)))
           )
       (propertize "=@=" 'display (all-the-icons-faicon "tags" :face 'all-the-icons-dgreen :v-adjust 0.02 :cache :height 0.7))
       (propertize "=@=" 'display (all-the-icons-faicon "tags" :face 'all-the-icons-dgreen :v-adjust 0.02 :height 0.7))
       )
     " "
     (propertize (string-join functiontag ", ") 'face 'all-the-icons-lblue)
     ))
  )

(cl-defmethod org-roam-node-othertags ((node org-roam-node))
  "Return the file TITLE for the node."
  (let* ((tags (seq-filter (lambda (tag) (not (string= tag "ATTACH"))) (org-roam-node-tags node)))
         (specialtags ag/lit-categories)
         (othertags (seq-difference tags specialtags 'string=))
         )
    (concat
     (if othertags
       (propertize "=@=" 'display "")
       (propertize "= =" 'display "")
       )
     (propertize (string-join othertags ", ") 'face 'all-the-icons-lorange))
    ))

(cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
  "Return the hierarchy for the node."
  (let* ((title (org-roam-node-title node))
         (olp (mapcar (lambda (s) (if (> (length s) 10) (concat (substring s 0 10)  "...") s)) (org-roam-node-olp node)))
         (level (org-roam-node-level node))
         (filetitle (org-roam-get-keyword "TITLE" (org-roam-node-file node)))
         (shortentitle (if (> (length filetitle) 20) (concat (substring filetitle 0 20)  "...") filetitle))
         (separator (concat " " (all-the-icons-material "chevron_right") " "))
         )
    (cond
     ((>= level 1) (concat (propertize (format "=level:%d=" level) 'display (all-the-icons-material "list" :face 'all-the-icons-blue))
                           " "
                           (propertize shortentitle 'face 'org-roam-dim)
                           (propertize separator 'face 'org-roam-dim)
                           title))
     (t (concat (propertize (format "=level:%d=" level) 'display (all-the-icons-material "insert_drive_file" :face 'all-the-icons-yellow))
                " "
                title))
     )
    ))

;; This closes the org-roam config
)

(setq ag/lit-categories
          '("book" "paper" "online" "journal" "thesis" "meetings" "courses" "projects" "conferences")
          )
(setq org-roam-node-display-template (concat " ${backlinkscount:8} " " ${functiontag:12} " " ${othertags:25} " " ${hierarchy:180} "))

(use-package deft
  :commands (deft)
  :bind (("C-c n d" . deft)
         ("C-c n u" . ag/pick-deft-dir))
  :config
  (setq  deft-directory org-roam-directory
         deft-extensions '("md" "org")
         deft-use-filename-as-title t)

  ;; Setup my list of deft directories
  (defvar ag/deft-dir-list '()
    "A list of deft directories to pick")

  (setq ag/deft-dir-list '("~/orgfiles/pages"
                           "~/orgfiles/journals"
                           "~/orgfiles/bibtex"
                           ))

  (defun ag/pick-deft-dir ()
    "Select directories from a list"
    (interactive)
    (setq deft-directory 
          (ido-completing-read "Select directory: " ag/deft-dir-list))
    (deft-refresh))

  (setq deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+\\#\\+title: ")
  (setq deft-strip-title-regexp
        (concat
         "\\(?:^%+\\|^#\\+TITLE: *\\|^[#* ]+\\|-\\*-[[:alpha:]]+-\\*-\\|^Title:[	 ]*\\|#+$\\)"
         ))

  )

(use-package org-ref
  :init
  (require 'bibtex)
  (require 'org-ref-ivy)
  ;; Following JKitchin config in scimax
  (require 'org-ref-arxiv)
  (require 'org-ref-scopus)
  (require 'org-ref-wos)
  (require 'doi-utils)
  (require 'org-ref-isbn)
  :config
  (setq org-ref-show-broken-links t)
  )

(use-package org-roam-bibtex
  :bind (("C-c b d" . doi-add-bibtex-entry)
         ("C-c b a" . arxiv-get-pdf-add-bibtex-entry)
         ("C-c b k" . org-ref-clean-bibtex-entry))
  :custom
  (org-roam-bibtex-mode 1)

  :config
  (require 'org-ref)

  (setq orb-preformat-keywords '("citekey" "author" "year" "title" "keywords" "file")
        orb-process-file-keyword t
        orb-file-field-extensions '("pdf"))
  )

(use-package pdf-tools
  :init
  (pdf-loader-install)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
  (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil
        display-line-numbers-mode 0
        pdf-view-resize-factor 1.1
        pdf-annot-activate-created-annotations t)
  )

(use-package org-noter
    :config
    (setq org-noter-always-create-frame nil
          org-noter-separate-notes-from-heading t
          org-noter-default-heading-title "Page $p$"
          org-noter-auto-save-last-location t
          org-noter-separate-notes-from-heading t
          org-noter-doc-property-in-notes t
          org-noter-hide-other t
          org-noter-doc-split-fraction '(.67 . .5)
          org-noter-notes-search-path '("~/orgfiles/bibtex/")
          )
    )

(add-to-list 'org-roam-capture-templates
             '("n" "references notes"  plain
               (file "~/.emacs.d/templates/org-capture/reference-noter")
               :if-new
               (file+head
                "bibtex/%<%Y%m%d>-${citekey}.org"
                "#+title: ${title}\n")
               :unnarrowed t)
             )

(add-to-list 'org-roam-capture-templates
             '("t" "thesis revision"  plain
               (file "~/.emacs.d/templates/org-capture/thesis-rev")
               :if-new
               (file+head
                "bibtex/%<%Y%m%d>-${citekey}.org"
                "#+title: ${title}\n")
               :unnarrowed t)
             )

(use-package org-sticky-header
  :hook (org-mode . org-sticky-header-mode)
  :config
  ;; Show full path in header
  (setq org-sticky-header-full-path 'full)
  ;; Use > instead of / as separator
  (setq org-sticky-header-outline-path-separator " > ")
  )

(use-package org-make-toc
  :hook (org-mode . org-make-toc-mode))

(use-package org-web-tools
  :after org
  :bind
  ("C-c n l" . org-web-tools-insert-link-for-url)
  )

(defun ag/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (setq lsp-pyls-disable-warning t)
  (lsp-headerline-breadcrumb-mode)
  )

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :hook (lsp-mode . ag/lsp-mode-setup)
  :config
  (setq lsp-enable-which-key-integration t)
  (setq lsp-auto-guess-root nil)
  (setq lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  (setq lsp-file-watch-threshold nil)
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-diagnostics-provider :none)
  (setq lsp-eldoc-hook nil)
  (setq flycheck-mode nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-delay .1)
  )

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy
  :after lsp)

(use-package company
  :after lsp-mode
  :hook ((lsp-mode . company-mode)
         (ess-r-mode . company-mode)
         (LaTeX-mode . company-mode))
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  (company-tooltip-align-annotations t)
  (company-selection-wrap-around t)
  ;; This might control my problems with R
  (company-tooltip-maximum-width 60)
  (company-tooltip-minimum-width 60)
  )

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs (append yas-snippet-dirs
                                 '("~/.emacs.d/templates/snippets/")))
  (yas-reload-all)
  (setq yas-triggers-in-field t)
  :init
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets
  :after yasnippet
  :config
  (yasnippet-snippets-initialize)
  )

(defun ag/org-latex-yas ()
  "Activate org and LaTeX yas expansion in org-mode buffers."
  (yas-minor-mode)
  (yas-activate-extra-mode 'latex-mode))

(add-hook 'org-mode-hook #'ag/org-latex-yas)

(use-package python-black
  :demand t
  :after python-mode
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(setenv "WORKON_HOME" "~/anaconda3/envs")

(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1)
  (pyvenv-activate "~/anaconda3"))

(use-package elpy
  :after python-mode
  :ensure t
  :config
  (setq elpy-shell-starting-directory 'current-directory
        python-shell-interpreter "python3"
        python-shell-interpreter-args "-i"
        elpy-rpc-virtualenv-path 'current)
   (add-to-list 'python-shell-completion-native-disabled-interpreters
                "python")

  :init
  (elpy-enable))

(use-package python-django
  :after python-mode)

(use-package poetry
  :after python-mode)

(use-package sphinx-doc
  :after python-mode
  :config (sphinx-doc-mode t))

(use-package python-mode
  :ensure t)

(defun ag/insert-r-pipe ()
  "R - %>% operator or 'then' pipe operator"
  (interactive)
  (just-one-space 1)
  (insert "|>")
  (reindent-then-newline-and-indent))

(use-package ess
  :defer t
  :commands R
  :bind (
         :map ess-mode-map
              ("C-<" . ess-insert-assign)
              ("C->" . ag/insert-r-pipe)
              :map inferior-ess-mode-map
              ("C-<" . ess-insert-assign)
              ("C->" . ag/insert-r-pipe)
              )
  :init
  (load "ess-site")
  :custom
  (setq ess-eval-visibly 'nowait)
  (setq ess-use-flymake nil)
  )

(use-package poly-R
  :config
  (defun ag/insert-rmd-chunk (language)
    "Insert an r-chunk in markdown mode. Necessary due to interactions between polymode and yasnippet"
    (interactive "sLanguage: ")
    (insert (concat "```{" language "}\n\n```"))
    (forward-line -1))
  (define-key poly-markdown+r-mode-map (kbd "M-n M-i") #'ag/insert-rmd-chunk)
  )

(defun ag/markdown-latex-yas ()
  "Activate org and LaTeX yas expansion in org-mode buffers."
  (yas-minor-mode)
  (yas-activate-extra-mode 'latex-mode))

(add-hook 'markdown-mode-hook #'ag/markdown-latex-yas)

(use-package stan-mode
  :mode ("\\.stan\\'" . stan-mode)
  :hook (stan-mode . stan-mode-setup)
  ;;
  :config
  ;; The officially recommended offset is 2.
  (setq stan-indentation-offset 2))

;;; company-stan.el
(use-package company-stan
  :hook (stan-mode . company-stan-setup)
  ;;
  :config
  ;; Whether to use fuzzy matching in `company-stan'
  (setq company-stan-fuzzy nil))

;;; eldoc-stan.el
(use-package eldoc-stan
  :hook (stan-mode . eldoc-stan-setup)
  ;;
  :config
  ;; No configuration options as of now.
  )

;;; flycheck-stan.el
(use-package flycheck-stan
  ;; Add a hook to setup `flycheck-stan' upon `stan-mode' entry
  :hook ((stan-mode . flycheck-stan-stanc2-setup)
         (stan-mode . flycheck-stan-stanc3-setup))
  :config
  ;; A string containing the name or the path of the stanc2 executable
  ;; If nil, defaults to `stanc2'
  (setq flycheck-stanc-executable nil)
  ;; A string containing the name or the path of the stanc2 executable
  ;; If nil, defaults to `stanc3'
  (setq flycheck-stanc3-executable nil))

;;; stan-snippets.el
(use-package stan-snippets
  :hook (stan-mode . stan-snippets-initialize)
  ;;
  :config
  ;; No configuration options as of now.
  )

(use-package yaml-mode
  :custom
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  )

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/github-repos")
    (setq projectile-project-search-path '("~/github-repos")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (treemacs-git-mode 'simple)
  )

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  )

(use-package treemacs-perspective
  :after (treemacs perspective) 
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (add-hook 'text-mode-hook 'disable-evil-mode)
  )

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t
  )

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
  :after magit)

(use-package git-timemachine
  :after magit
  :config
  (setq git-timemachine-abbreviation-length 4)
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package dockerfile-mode)

(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "zsh") ;; Change this to zsh, etc
  (setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :after evil-collection
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "powershell.exe")
  (setq explicit-powershell.exe-args '()))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single
  :after dired)

(use-package all-the-icons-dired
  :after dired
  :config
  (setq all-the-icons-dired-monochrome nil))

(use-package all-the-icons-dired
  :after dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :after dired
  :config
  ;; Doesn't work as expected!
  (add-to-list 'dired-open-functions #'dired-open-xdg t)
  ;; -- OR! --
  (setq dired-open-extensions '(("png" . "preview")
                                ("mkv" . "preview"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

;; For macOS type of keybindings
(global-set-key (kbd "<M-s-up>")    'evil-window-up)
(global-set-key (kbd "<M-s-down>")  'evil-window-down)
(global-set-key (kbd "<M-s-left>")  'evil-window-left)
(global-set-key (kbd "<M-s-right>") 'evil-window-right)

(use-package buffer-move
  :custom
  (buffer-move-stay-after-swap t)
  )

(global-set-key (kbd "<C-s-up>")     'buf-move-up)
(global-set-key (kbd "<C-s-down>")   'buf-move-down)
(global-set-key (kbd "<C-s-left>")   'buf-move-left)
(global-set-key (kbd "<C-s-right>")  'buf-move-right)

(use-package winner-mode
  :ensure nil
  :bind (:map evil-window-map
         ("u" . winner-undo)
         ("U" . winner-redo))
  :config
  (winner-mode))

(use-package winum
  :config
  (winum-mode))

(defun ag/latex-mode-visual-fill ()
  (setq fill-column 80)
  (display-fill-column-indicator-mode 1)
  )

;; (use-package auctex-latexmk
;;   :ensure t
;;   :config
;;   (auctex-latexmk-setup)
  ;; (setq auctex-latexmk-inherit-TeX-PDF-mode t))

(use-package company-auctex
  :ensure t
  :init (company-auctex-init))

(use-package latex
  :ensure auctex
  :hook (LaTeX-mode . ag/latex-mode-visual-fill)
  :custom
  (reftex-plug-into-AUCTeX t)
  (reftex-default-bibliography '("~/orgfiles/references/bibliography.bib"
                                 "~/orgfiles/references/bibliographypdfs.bib"))
  (LaTeX-indent-level 4
                      LaTeX-item-indent 0
                      TeX-brace-indent-level 4
                      TeX-newline-function 'newline-and-indent)
  :config
  (setq font-latex-match-reference-keywords
        '(
          ("cite" "[{")
          ("citep" "[{")
          ("cite*" "[{")
          )
        )
  (setq TeX-parse-self t
        TeX-auto-save t)
  ;; Prevent superscripts and subscripts from being displayed in a
  ;; different font size.
  (setq font-latex-fontify-script nil)
  ;; Prevent section headers from being displayed in different font
  ;; sizes.
  (setq font-latex-fontify-sectioning 1)
  ;; Don't be afraid to break inline math between lines.
  (setq LaTeX-fill-break-at-separators nil)
  (setq TeX-source-correlate-method 'synctex)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  ;; Use pdf-tools to open PDF files
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t)

  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  )

(use-package mic-paren
  :after latex
  :config
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (font-lock-add-keywords nil
                                      '(("\\\\alpha" 0 font-lock-warning-face t)))
              ))
)

(use-package smartparens
  :custom
  (smartparens-global-strict-mode nil)
  (smartparens-gobael-mode t)
  :config
  (sp-local-pair 'org-mode "$" "$")
  (sp-local-pair 'markdown-mode "$" "$")
  (sp-local-pair 'org-mode "~" "~")
 )

(add-hook 'org-mode-hook 'smartparens-mode)
(add-hook 'markdown-mode-hook 'smartparens-mode)

(use-package bibtex
  :defer t ; built-in with Emacs
  :bind (("C-c b s" . bibtex-sort-buffer))
  :custom
  (bibtex-autokey-titleword-length 0)
  (bibtex-autokey-titleword-separator "")
  (bibtex-autokey-titlewords 0)
  (bibtex-autokey-year-length 4)
  (bibtex-autokey-year-title-separator "")
  (bibtex-align-at-equal-sign t)
  (bibtex-autokey-name-case-convert-function 'capitalize)
  :config
  (defun ag/bibtex-generate-autokey ()
    (interactive)
    ;; first we delete the existing key
    (bibtex-beginning-of-entry)
    (re-search-forward bibtex-entry-maybe-empty-head)
    (if (match-beginning bibtex-key-in-head)
        (delete-region (match-beginning bibtex-key-in-head)
                       (match-end bibtex-key-in-head)))
    (let* ((names (bibtex-autokey-get-names))
           (year (bibtex-autokey-get-year))
           (existing-keys (bibtex-parse-keys))
           key)
      (setq key (format "%s%s" names year))
      (let ((ret key))
        (cl-loop for c
                 from ?b to ?z
                 while (assoc ret existing-keys)
                 do (setq ret (format "%s%c" key c)))
        ret)))
  (advice-add #'bibtex-generate-autokey :override #'ag/bibtex-generate-autokey)
  )

(use-package ivy-bibtex
  :bind (("C-c b f" . ivy-bibtex)
         ("C-c b n" . ivy-bibtex-with-notes)
         ("C-c b ]" . org-ref-cite-insert-ivy)
         ("C-c b h" . org-ref-bibtex-hydra/body)
         )
  :config
  (setq bibtex-completion-bibliography '("~/orgfiles/references/bibliography.bib"
                                         "~/orgfiles/references/bibliography-wpdfs.bib"
                                         "~/orgfiles/references/bibliography-arxiv.bib"))
  (setq  bibtex-completion-library-path "~/orgfiles/references/bibtex-pdfs/"
         bibtex-completion-notes-path   "~/orgfiles/bibtex/")

  (setq bibtex-completion-pdf-symbol "⌘")
  (setq bibtex-completion-notes-symbol "✎")
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)

  (setq bibtex-completion-display-formats
      '((t . "${=has-pdf=:1}${=has-note=:1} ${=type=:7} ${year:4} ${author:36} ${title:*} ${keywords:31}"))
    )

  (defun bibtex-completion-format-citation-org-cite-original (keys)
    "Format org-links using Org mode's own cite syntax."
    (format "citep:%s"
            (s-join ";"
                    (--map (format "%s" it) keys))))

  (setq bibtex-completion-format-citation-functions
        '((org-mode      . bibtex-completion-format-citation-org-cite-original)
          (latex-mode    . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (default       . bibtex-completion-format-citation-default)))
  )

(defun ag/ivy-toggle-mark ()
  "Toggle the mark"
  (interactive)
  (if (ivy--marked-p)
      (ivy-unmark)
    (ivy-mark))
  (ivy-previous-line))

(define-key ivy-minibuffer-map (kbd "M-TAB")
  #'ag/ivy-toggle-mark)

(use-package reftex
  :after auctex
  :config
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
  (setq reftex-save-parse-info t
        reftex-enable-partial-scans t
        reftex-use-multiple-selection-buffers t
        reftex-plug-into-AUCTeX t
        reftex-vref-is-default t
        reftex-cite-format
        '((?\C-m . "\\cite[]{%l}")
          (?t . "\\textcite{%l}")
          (?a . "\\autocite[]{%l}")
          (?p . "\\parencite{%l}")
          (?f . "\\footcite[][]{%l}")
          (?F . "\\fullcite[]{%l}")
          (?x . "[]{%l}")
          (?X . "{%l}"))

        font-latex-match-reference-keywords
        '(("cite" "[{")
          ("citep" "[{")
          ("cites" "[{}]")
          ("footcite" "[{")
          ("footcites" "[{")
          ("parencite" "[{")
          ("textcite" "[{")
          ("fullcite" "[{")
          ("citetitle" "[{")
          ("citetitles" "[{")
          ("headlessfullcite" "[{"))

        reftex-cite-prompt-optional-args nil
        reftex-cite-cleanup-optional-args t))

(use-package lsp-latex
  :after auctex
  :config
  (add-hook 'TeX-mode-hook 'lsp)
  (add-hook 'LaTeX-mode-hook 'lsp)
  (add-hook 'bibtex-mode-hook 'lsp)
  )

(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.1))
(use-package org-fragtog
  :config (add-hook 'org-mode-hook 'org-fragtog-mode))

(setenv "TEXMFHOME" "~/.texmf")

(add-to-list 'org-latex-classes
             '("custom"
               "\\documentclass[stslayout, reqno, noinfoline, preprint]{imsart}
\\usepackage[hmarginratio=1:1,top=32mm,columnsep=20pt]{geometry}
\\geometry{left=30mm,right=30mm}
\\usepackage[utf8]{inputenc}
\\usepackage{amsthm, amssymb, amsmath}
\\usepackage{graphicx}
\\usepackage{grffile}
\\usepackage{longtable}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{amssymb}
\\usepackage{capt-of}
\\usepackage[pagebackref=true,colorlinks=true,urlcolor=blue,pdfborder={0 0 0}]{hyperref}
      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]"
               ("\\section{%s}" . "\n\\section{%s}")
               ("\\subsection{%s}" . "\n\\subsection{%s}")
               ("\\subsubsection{%s}" . "\n\\subsubsection{%s}")
               ("\\paragraph{%s}" . "\n\\paragraph{%s}")
               ("\\subparagraph{%s}" . "\n\\subparagraph{%s}")
               )           
             )

(add-to-list 'org-latex-classes
               '("handout"
                 "\\documentclass[stslayout, reqno, noinfoline, preprint]{imsart}
  \\usepackage[hmarginratio=1:1,top=25mm,columnsep=20pt,footskip=15mm]{geometry}
  \\geometry{left=30mm,right=30mm,bottom=20mm}
  \\usepackage[utf8]{inputenc}
  \\usepackage{amsthm, amssymb, amsmath}
  \\usepackage{graphicx}
  \\usepackage{grffile}
  \\usepackage{longtable}
  \\usepackage{wrapfig}
  \\usepackage{rotating}
  \\usepackage[normalem]{ulem}
  \\usepackage{amsmath}
  \\usepackage{textcomp}
  \\usepackage{amssymb}
  \\usepackage{capt-of}
  \\usepackage[pagebackref=true,%
               colorlinks=true,%
               urlcolor=blue,%
               pdfborder={0 0 0}]{hyperref}
  \\usepackage{fancyhdr}
  \\renewcommand{\\textfraction}{0.05}
  \\renewcommand{\\topfraction}{0.8}
  \\renewcommand{\\bottomfraction}{0.8}
  \\renewcommand{\\floatpagefraction}{0.75}
  \\fancyfoot{}%
  \\renewcommand{\\footrulewidth}{0.4pt}%
  \\fancyfoot[C]{\\includegraphics[height=1cm]{%
                 ~/.dotfiles/assets/itam-footer.png}%
                 \\vspace{-1cm}}%
  \\fancyfoot[RE,LO]{\\vspace{-1cm}\\thepage}
  \\pagestyle{fancy}
  \\fancypagestyle{plain}{%
  \\fancyhead{}%
  \\renewcommand{\\headrulewidth}{0pt}%
  \\pagestyle{fancy}}
  \\fancypagestyle{toc}{%
  \\fancyhf{}%
  \\fancyhead[L]{\\nouppercase{\\rightmark}}
  \\fancyhead[R]{\\nouppercase{\\leftmark}}
  \\fancyfoot[C]{\\thepage}}
  \\newcommand{\\chaptermark}[1]{%
  \\markboth{\\chaptername\\ \\thechapter.\\ #1}{}}
        [NO-DEFAULT-PACKAGES]
        [NO-PACKAGES]"
                 ("\\section{%s}" . "\n\\section{%s}")
                 ("\\subsection{%s}" . "\n\\subsection{%s}")
                 ("\\subsubsection{%s}" . "\n\\subsubsection{%s}")
                 ("\\paragraph{%s}" . "\n\\paragraph{%s}")
                 ("\\subparagraph{%s}" . "\n\\subparagraph{%s}")
                 ))

(add-to-list 'org-latex-classes        
             '("header"
               "\\documentclass[reqno, noinfoline, preprint, oneside]{article}
\\usepackage[hmarginratio=1:1,top=16mm,columnsep=20pt]{geometry}
\\geometry{left=30mm,right=30mm}
\\usepackage[utf8]{inputenc}
\\usepackage{amsthm, amssymb, amsmath}
\\usepackage{graphicx}
\\usepackage{grffile}
\\usepackage{longtable}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{amssymb}
\\usepackage{capt-of}
\\definecolor{myGreen}{HTML}{006953}
\\usepackage[pagebackref=true,colorlinks=true,pdfborder={0 0 0}]{hyperref}
\\usepackage{fancyhdr} \\fancyhead{}
\\fancyhead[L]{\\includegraphics[height=3cm]{~/.dotfiles/assets/mcd-header.png} \\vspace{-.7cm}}
\\fancyhead[R]{{\\parbox[b][10mm][t]{0.5\\textwidth}{\\raggedleft{Departamento
      Académico de Estadística.\\ Campus Río Hondo.\\ Ciudad de
      México.}}}\\vspace{.3cm}}
\\renewcommand{\\headrulewidth}{.5pt}
\\pagestyle{fancy}
      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]"
               ("\\section{%s}" . "\n\\section{%s}")
               ("\\subsection{%s}" . "\n\\subsection{%s}")
               ("\\subsubsection{%s}" . "\n\\subsubsection{%s}")
               ("\\paragraph{%s}" . "\n\\paragraph{%s}")
               ("\\subparagraph{%s}" . "\n\\subparagraph{%s}")
               )
             )

(setq org-latex-default-class "custom")

(setq org-latex-pdf-process '("latexmk -f -pdf %f -shell-escape -interaction=nonstopmode -output-directory=%o"))

(setq org-latex-listings 'listings)
(setq org-latex-custom-lang-environments
      '((r "r")))
(setq org-latex-listings-options
      '(("frame" "single")
        ("backgroundcolor" "\\color{backcolour}")
        ("basicstyle" "\\ttfamily\\footnotesize")
        ("stringstyle" "\\ttfamily")
        ("numbers" "left")
        ("numberstyle" "\\tiny\\color{codegray}")
        ("rulecolor" "\\color{white}")
        ("commentsyle" "\\color{codegreen}")
        ))
(org-add-link-type
 "latex" nil
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "<span class=\"%s\">%s</span>" path desc))
    ((eq format 'latex)
     (format "\\%s{%s}" path desc)))))

(defun org-latex-ref-to-cref (text backend info)
  "Use \\cref instead of \\ref in latex export."
  (when (org-export-derived-backend-p backend 'latex)
    (replace-regexp-in-string "\\\\ref{" "\\\\cref{" text)))

(add-to-list 'org-export-filter-final-output-functions
             'org-latex-ref-to-cref)

(use-package cdlatex
  :after (tex)
  :config
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex))
  ;; (add-hook 'org-mode-hook 'turn-on-org-cdlatex)

(use-package org-download
    :after org
    :defer nil
    :custom
    (org-download-method 'directory)
    (org-download-image-dir "images")
    (org-download-heading-lvl nil)
    (org-download-timestamp "%Y%m%d-%H%M%S_")
    (org-download-image-attr-list '("#+attr_html: :width 700 :align center"))
    (org-download-screenshot-method "/usr/local/bin/pngpaste %s")
    :bind
    ("C-c n s" . org-download-screenshot)
    :config
    (require 'org-download))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper))
  )

(use-package flyspell-correct-ivy
  :after flyspell-correct)

(add-hook 'text-mode-hook 'flyspell-mode)

(setq pdf-view-use-scaling t
       pdf-view-use-imagemagick nil)

(use-package arxiv-mode
  :ensure t
  :config
  (setq arxiv-default-bibliography "~/orgfiles/references/bibliography-arxiv.bib"
        arxiv-default-download-folder "~/orgfiles/references/arxiv-pdfs/"
        arxiv-default-category "stat"
        arxiv-startup-with-abstract-window t)     
  )

(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/orgfiles/feeds/rss-list.org"))
  )

(defun ag/org-start-presentation ()
  (interactive)
  (org-tree-slide-mode 1)
  (org-sticky-header-mode 0)
  (setq text-scale-mode-amount 10.5)
  (text-scale-mode 1)
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.5) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     (org-code (:height 1.20) org-code)
                                     (org-verbatim (:height 1.20) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  (setq-local org-format-latex-options (plist-put org-format-latex-options :scale 1.7))
  (setq-local visual-fill-column-width 50
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1)
  (setq org-src-window-setup 'current-window)
  )

(defun ag/org-end-presentation ()
  (interactive)
  (text-scale-mode 0)
  (org-sticky-header-mode 1)
  (org-tree-slide-mode 0)
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq-local org-format-latex-options (plist-put org-format-latex-options :scale 1.2))
  (visual-fill-column-mode 1)
  (setq org-src-window-setup 'split-window-below)
  (org-mode-restart)
  )

(use-package hide-lines
  :after org
  )

(use-package org-tree-slide
  :defer t
  :after org
  :commands org-tree-slide-mode
  :config
    (evil-define-key 'normal org-tree-slide-mode-map
      (kbd "q") 'ag/org-end-presentation
      (kbd "C-j") 'org-tree-slide-move-next-tree
      (kbd "C-k") 'org-tree-slide-move-previous-tree)
    (setq org-tree-slide-slide-in-effect nil
          org-tree-slide-activate-message "Presentation started."
          org-tree-slide-deactivate-message "Presentation ended."
          org-tree-slide-header t)

    (when (require 'hide-lines nil t)
      (defvar ag/org-src-block-faces nil)
      (defun ag/show-headers ()
        (setq org-src-block-faces 'ag/org-src-block-faces)
        (hide-lines-show-all))
      (defun ag/hide-headers ()
        (setq ag/org-src-block-faces 'org-src-block-faces)
        ;; (setq-local org-src-block-faces
              ;; '(("emacs-lisp" (:background "cornsilk"))))
        (hide-lines-matching "#\\+BEGIN_SRC")
        ;; (hide-lines-matching "#\\+ATTR_HTML")
        (hide-lines-matching "#\\+END_SRC"))
        ;; (hide-lines-matching "#\\+ATTR_\\(HTML\\|EXAMPLE\\|VERSE\\|QUOTE\\)")
      (add-hook 'org-tree-slide-play-hook 'ag/hide-headers)
      (add-hook 'org-tree-slide-stop-hook 'ag/show-headers)

      (defun advice:org-edit-src-code (&optional code edit-buffer-name)
        (interactive)
        (ag/show-headers))
      (advice-add 'org-edit-src-code :before #'advice:org-edit-src-code)
      (defun advice:org-edit-src-exit ()
        (interactive)
        (ag/hide-headers))
      (advice-add 'org-edit-src-exit :after #'advice:org-edit-src-exit))

    :custom
    (org-tree-slide-breadcrumbs " > ")
    (org-image-actual-width nil)
    )

(use-package org-re-reveal-citeproc
  :config
  (require 'org-re-reveal)
  )
(add-to-list 'org-export-filter-paragraph-functions
             #'org-re-reveal-citeproc-filter-cite)

(defun ag/org-reveal ()  
  (use-package ox-reveal
    :custom
    (org-reveal-note-key-char nil)
    ;; (org-reveal-root "/Users/agarbuno/software/reveal.js")
    (org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
    (setq org-reveal-mathjax t)
    )
  (use-package htmlize)
  (require 'ox-reveal)
  )
