;; NOTE: init.el is now generated from Emacs.org.  Please edit that file
;;       in Emacs and init.el will be generated automatically!

;; You will most likely need to adjust this font size for your system!
(defvar ag/default-font-size 150)
(defvar ag/default-variable-font-size 150)

;; Make frame transparency overridable
(defvar ag/frame-transparency '(95 . 95))

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

(set-face-attribute 'default nil :font "Fira Code Retina" :height ag/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height ag/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height ag/default-font-size :weight 'regular)

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
    :global-prefix "C-SPC")

  (ag/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "fde" '(lambda () (interactive) (find-file (expand-file-name "~/github-repos/dotfiles/emacs.org")))
    "fds" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/init.el")))
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

(use-package doom-themes
  :init (load-theme 'doom-monokai-pro t))
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
  :bind
  (("C-x k" . persp-kill-buffer*)
   ("C-x b" . persp-ivy-switch-buffer))
  :init
  (persp-mode))

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

(defun ag/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (setq fill-column 80))

(use-package org
  :commands (org-capture org-agenda)
  :hook (org-mode . ag/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-support-shift-select t)

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
      '("~/Google Drive/orgfiles/agenda/tasks.org"
        "~/Google Drive/orgfiles/agenda/habits.org"))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAIT(w)" "READ(r)" "VIEW(v)" "|" ))
        )

  (setq org-refile-targets
        '(("archive.org" :maxlevel . 1)
          ("tasks.org" :maxlevel . 1)))

  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "hot pink" :weight bold))
          ("DONE" . (:foreground "#00e6ab" :weight bold))
          ("NEXT" . (:foreground "dark orange" :weight bold))
          ("WAIT" . (:foreground "#aeffff" :weight bold))
          ("READ" . (:foreground "#ffcc66" :weight bold))
          ("VIEW" . (:foreground "#8787ff" :weight bold))
          ))

  (setq org-tag-alist
        '((:startgroup)
          ;; Put mutually exclusive tags here
          (:endgroup)
          ("research" . ?r)
          ("maestria" . ?m)
          ("teaching" . ?t)
          ("paper"    . ?p)
          ("book"     . ?b)
          ("idea" . ?i)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Ongoing Tasks")))
            (tags-todo "+research/!-NEXT" ((org-agenda-overriding-header "Research Projects")))
            (tags-todo "+teaching/!-NEXT" ((org-agenda-overriding-header "Teaching Tasks")))
            (tags-todo "+maestria/!-NEXT" ((org-agenda-overriding-header "McDatos Tasks")))

            (tags-todo "-research-teaching-maestria/!-NEXT"
                       ((org-agenda-overriding-header "Unprocessed Inbox Tasks")
                        ;; (org-agenda-files "~/Google Drive/orgfiles/agenda/tasks.org")
                        (org-agenda-text-search-extra-files nil)
                        ))
            ))

          ("n" "Next Tasks"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))

          ("W" "Work Tasks" tags-todo "+work-email")
          ))

  (setq org-capture-templates
        `(("t" "Tasks/Projects ")
          ("tt" "Task" entry
           (file+olp "~/Google Drive/orgfiles/agenda/tasks.org" "Active")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
          ("tr" "Research Tasks" entry
           (file+olp "~/Google Drive/orgfiles/agenda/tasks.org" "Research")
           "* TODO %?  :research:\nLink: %a")
          ("tp" "Reading Reminder" entry
           (file+olp "~/Google Drive/orgfiles/agenda/tasks.org" "Reading")
           "* READ %?  \nLink: %a")
          ("tv" "Talk or Video" entry
           (file+olp "~/Google Drive/orgfiles/agenda/tasks.org" "Tutorial")
           "* VIEW %?  \nLink: %a")
          ("j" "Journal" entry
           (file+datetree "~/Google Drive/orgfiles/agenda/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")
          ("h" "Habit" entry
           (file+olp "~/Google Drive/orgfiles/agenda/habits.org" "Work")
           "* TODO %?")
          )
        )

  (define-key global-map (kbd "C-c t t")
    (lambda () (interactive) (org-capture nil "tt")))

  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c t c") 'org-capture)

  (require 'org-habit)
  (setq org-habit-show-all-today t) 
  (setq org-habit-graph-column 60)

  (ag/org-font-setup))

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
   (python . t)))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("la" . "src latex"))
  (add-to-list 'org-structure-template-alist '("r" . "src R"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

(push '("conf-unix" . conf-unix) org-src-lang-modes)
(setq org-confirm-babel-evaluate nil)
(setq org-src-window-setup 'split-window-right)
(add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))

;; Automatically tangle our Emacs.org config file when we save it
(defun ag/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/github-repos/dotfiles/emacs.org"))
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
  (org-roam-directory (file-truename "~/Google Drive/orgfiles/notes/"))
  (org-roam-completion-everywhere t)
  ;; (org-roam-completion-system 'default)
  ;; Capture templates
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("r" "reference" plain
      "%? %^{author} - %^{year}:"
      :if-new
      (file+head
       "References/${citekey}.org"
       "#+title: ${title}\n")
      :unnarrowed t)
     )
   )
  ;; Dailies templates
  (org-roam-dailies-directory "Journal/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "\n*  %?"
      :if-new (file+head
               "%<%Y-%m-%d>.org"
               "#+title: %<%Y-%m-%d %a>\n"))
     ("t" "talks" entry
      "\n*  %<%I:%M %p> - %^{Talk Title} by %^{Speaker} \n\n%?\n\n"
      :if-new (file+head+olp
               "%<%Y-%m-%d>.org"
               "#+title: %<%Y-%m-%d %a>\n\n"
               ("Talks")))
     ("m" "meeting" entry
      "\n*  %<%I:%M %p> - %^{Meeting Title}\n\n%?\n\n"
      :if-new (file+head+olp
               "%<%Y-%m-%d>.org"
               "#+title: %<%Y-%m-%d %a>\n#+filetags: :meetings:\n"
               ("Meetings")))
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
        (concat (propertize "" 'display (all-the-icons-material "link" :face 'all-the-icons-dblue :height 0.9)) (format "%3d" count))
      (concat (propertize "" 'display (all-the-icons-material "link" :face 'org-roam-dim :height 0.9))  "   ")
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
                (propertize "" 'display (all-the-icons-faicon "file-pdf-o" :face 'all-the-icons-dgreen :v-adjust 0.02 :height 0.8)))
               ((member "thesis" functiontag)
                (propertize "" 'display (all-the-icons-octicon "book" :face 'all-the-icons-dgreen :v-adjust 0.02 :height 0.8)))
               ((member "book" functiontag)
                (propertize "" 'display (all-the-icons-faicon "book" :face 'all-the-icons-dgreen :v-adjust 0.02 :height 0.8)))
               ((member "online" functiontag)
                (propertize "" 'display (all-the-icons-faicon "globe" :face 'all-the-icons-dgreen :v-adjust 0.02 :height 0.8)))
               ((member "meetings" functiontag)
                (propertize "" 'display (all-the-icons-octicon "broadcast" :face 'all-the-icons-dgreen :v-adjust 0.02 :height 0.8)))
               ((member "courses" functiontag)
                (propertize "" 'display (all-the-icons-octicon "mortar-board" :face 'all-the-icons-dgreen :v-adjust 0.02 :height 0.8)))
               ((member "projects" functiontag)
                (propertize "" 'display (all-the-icons-octicon "puzzle" :face 'all-the-icons-dgreen :v-adjust 0.02 :height 0.8)))
           )
       (propertize "" 'display (all-the-icons-faicon "tags" :face 'all-the-icons-dgreen :v-adjust 0.02 :cache :height 0.7))
       (propertize "" 'display (all-the-icons-faicon "tags" :face 'all-the-icons-dgreen :v-adjust 0.02 :height 0.7))
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
     ;; (if othertags
     ;;   (propertize "=@=" 'display "")
     ;;   (propertize "= =" 'display "")
     ;;   )
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
     ((>= level 1) (concat (propertize (format "" level) 'display (all-the-icons-material "list" :face 'all-the-icons-blue))
                           " "
                           (propertize shortentitle 'face 'org-roam-dim)
                           (propertize separator 'face 'org-roam-dim)
                           title))
     (t (concat (propertize (format "" level) 'display (all-the-icons-material "insert_drive_file" :face 'all-the-icons-yellow))
                " "
                title))
     )
    ))

;; This closes the org-roam config
)

(setq ag/lit-categories
          '("book" "paper" "online" "journal" "thesis" "meetings" "courses" "projects")
          )
(setq org-roam-node-display-template (concat " ${backlinkscount:5} " " ${functiontag:8} " " ${othertags:25} " " ${hierarchy:*} "))

(use-package deft
  :commands (deft)
  :bind (("C-c n d" . deft)
         ("C-c n u" . ag/pick-deft-dir))
  :config
  (setq  deft-directory "~/Google Drive/orgfiles/notes/"
         deft-extensions '("md" "org"))

  ;; Setup my list of deft directories
  (defvar ag/deft-dir-list '()
    "A list of deft directories to pick")

  (setq ag/deft-dir-list '("/Users/agarbuno/Google Drive/orgfiles/notes"
                           "/Users/agarbuno/Google Drive/orgfiles/notes/Journal"
                           "/Users/agarbuno/Google Drive/orgfiles/notes/References"
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

(use-package org-roam-bibtex
  :bind (("C-c b d" . doi-add-bibtex-entry)
         ("C-c b a" . arxiv-get-pdf-add-bibtex-entry)
         ("C-c b k" . org-ref-clean-bibtex-entry))
  :custom
  (org-roam-bibtex-mode 1)
  :config
  (require 'org-ref)
  (setq reftex-default-bibliography '("~/Google Drive/orgfiles/references/bibliography.bib"
                                      "~/Google Drive/orgfiles/references/bibliographypdfs.bib"))

  (setq org-ref-completion-library 'org-ref-ivy-cite)
  (require 'org-ref-ivy-cite)

  ;; see org-ref for use of these variables
  (setq org-ref-bibliography-notes "~/Google Drive/orgfiles/references/notes.org"
        org-ref-default-bibliography '("~/Google Drive/orgfiles/references/bibliography.bib"
                                       "~/Google Drive/orgfiles/references/bibliographypdfs.bib")
        org-ref-pdf-directory "~/Google Drive/orgfiles/references/bibtex-pdfs/"
        org-ref-notes-directory "~/Google Drive/orgfiles/references/bibtex-notes/"
        org-ref-show-broken-links t)

  (setq orb-preformat-keywords '("citekey" "author" "year" "title" "keywords" "file")
        orb-process-file-keyword t
        orb-file-field-extensions '("pdf"))

  (add-to-list 'org-roam-capture-templates
               '("r" "reference" plain
                 "%? %^{author} - %^{year}:"
                 :if-new
                 (file+head
                  "References/${citekey}.org"
                  "#+title: ${title}\n")
                 :unnarrowed t)
               )
  (add-to-list 'org-roam-capture-templates
               '("n" "references notes"  plain
                 (file "~/.emacs.d/templates/org-capture/reference-noter")
                 :if-new
                 (file+head
                  "References/${citekey}.org"
                  "#+title: ${title}\n")
                 :unnarrowed t)
               )
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
          org-noter-notes-search-path '("~/Google Drive/orgfiles/notes/References/"
                                        "~/Google Drive/orgfiles/references/bibtex-notes/")
          )
    )

(add-to-list 'org-roam-capture-templates
             '("n" "references notes"  plain
               (file "~/.emacs.d/templates/org-capture/reference-noter")
               :if-new
               (file+head
                "References/${citekey}.org"
                "#+title: ${title}\n")
               :unnarrowed t)
             )

(add-to-list 'org-roam-capture-templates
             '("t" "thesis revision"  plain
               (file "~/.emacs.d/templates/org-capture/thesis-rev")
               :if-new
               (file+head
                "References/${citekey}.org"
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
  (yas-reload-all)
  :init
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets
  :after yasnippet
  :config
  (yasnippet-snippets-initialize)
  )

(setq yas-snippet-dirs (append yas-snippet-dirs
                               '("/Users/agarbuno/.emacs.d/template/snippets/")))

(defun ag/org-latex-yas ()
  "Activate org and LaTeX yas expansion in org-mode buffers."
  (yas-minor-mode)
  (yas-activate-extra-mode 'latex-mode))

(use-package dap-mode)

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(defun ag/insert-r-pipe ()
  "R - %>% operator or 'then' pipe operator"
  (interactive)
  (just-one-space 1)
  (insert "%>%")
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

(use-package auctex-latexmk
  :ensure t
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

(use-package company-auctex
  :ensure t
  :init (company-auctex-init))

(use-package latex
  :ensure auctex
  :hook (LaTeX-mode . ag/latex-mode-visual-fill)
  :custom
  (reftex-plug-into-AUCTeX t)
  (reftex-default-bibliography '("~/Google Drive/orgfiles/references/bibliography.bib"
                                 "~/Google Drive/orgfiles/references/bibliographypdfs.bib"))
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
 )

(use-package pdf-tools
    :after latex)

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
         ("C-c b n" . ivy-bibtex-with-notes))
  :config
  (setq bibtex-completion-bibliography '("~/Google Drive/orgfiles/references/bibliography.bib"
                                         "~/Google Drive/orgfiles/references/bibliographypdfs.bib"))
  (setq  bibtex-completion-library-path "~/Google Drive/orgfiles/references/bibtex-pdfs"
         bibtex-completion-notes-path   "~/Google Drive/orgfiles/references/bibtex-notes")

  (setq bibtex-completion-pdf-symbol "⌘")
  (setq bibtex-completion-notes-symbol "✎")
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)

  (setq bibtex-completion-display-formats
      '((t . "${=has-pdf=:1}${=has-note=:1} ${=type=:7} ${year:4} ${author:36} ${title:*} ${keywords:31}"))
    )

  )

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

(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2))
(use-package org-fragtog
  :config (add-hook 'org-mode-hook 'org-fragtog-mode))

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
))

(setq org-latex-default-class "custom")

(setq org-latex-pdf-process '("latexmk -f -pdf -shell-escape -interaction=nonstopmode -output-directory=%o %f"))

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

(defun ag/org-start-presentation ()
    (interactive)
    (org-tree-slide-mode 1)
    (org-sticky-header-mode 0)
    (setq text-scale-mode-amount 3)
    (text-scale-mode 1)
    (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                       (header-line (:height 4.5) variable-pitch)
                                       (org-document-title (:height 1.75) org-document-title)
                                       (org-code (:height 1.55) org-code)
                                       (org-verbatim (:height 1.55) org-verbatim)
                                       (org-block (:height 1.25) org-block)
                                       (org-block-begin-line (:height 0.7) org-block)))
    (setq-local org-format-latex-options (plist-put org-format-latex-options :scale 1.7))
    (setq-local visual-fill-column-width 60
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1)
    )

(defun ag/org-end-presentation ()
  (interactive)
  (text-scale-mode 0)
  (org-sticky-header-mode 1)
  (org-tree-slide-mode 0)
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq-local org-format-latex-options (plist-put org-format-latex-options :scale 1.2))
  (visual-fill-column-mode 1)
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
