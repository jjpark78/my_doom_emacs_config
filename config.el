(load! "+custom_funcs")
(load! "+private")
;; prodigy를 사용해서 빌드 명령어를 돌릴려고 했는데 버퍼 출력이 어마어마해져서 그냥 터미널에서 하는게 속 편하다는 걸 알았다.
;; (load! "+startup")
;; (add-to-list 'load-path "~/.doom.d/legacy/org-weather")

;; 사용자 이름 설정
(setq user-full-name "Jaejin Park"
      user-mail-address "jjpark78@outlook.com")
(setq-default frame-title-format '("DOOM EMACS"))

;; 바쁘게 버퍼전환 하는 와중에 화면이 울렁거리는게 멀미날것 같아서 프리뷰 옵션을 껐다
(setq +ivy-buffer-preview nil)
;; (setq ivy-tab-space t)
(setq avy-all-windows t)
(setq ivy-read-action-function #'ivy-hydra-read-action)
;; dired를 두개 열어 놓고 왔다 갔다하며 복사 붙여넣기 할때 편하다
(setq dired-dwim-target t)

(setq which-key-idle-delay 0.5)
(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))

;; Restore Frame size and location, if we are using gui emacs
;; (if window-system
;;   (progn
;;     (add-hook 'after-init-hook 'load-framegeometry)
;;     (add-hook 'kill-emacs-hook 'save-framegeometry))
;; )

;; 수동으로 직접 프레임 위치를 지정해줘 본다.
(setq initial-frame-alist '((top . 23) (left . 1147) (width . 130) (height . 80)))

;; 좌우로 여백을 활성화 시킨다.
(defun my-fringe-mode-hook ()
   (fringe-mode '(15 . 15)))

(add-hook 'prog-mode-hook 'my-fringe-mode-hook)
(add-hook 'gfm-mode-hook  'my-fringe-mode-hook)
(add-hook 'org-mode-hook  'my-fringe-mode-hook)
(global-evil-matchit-mode)
;; make open url function to use webkit
;; (setq browse-url-browser-function 'xwidget-webkit-browse-url)

;; 스나이프를 화면 보이는 영역으로 제한한다.
(setq evil-snipe-scope 'whole-visible)
(setq evil-snipe-repeat-scope 'whole-visible)

;; 와카타임이라고 본인이 얼마나 열심히 일했는지
;; 하루 일과를 잘 보여준다.
;; 개인으로만 쓰면 공짜인것도 매력임.
(global-wakatime-mode)

;; (defadvice! message-with-timestamp (args)
;;   :filter-args #'message
;;   (setcar args (format "%s %s"
;;                        (format-time-string "[%F %T.%3N %Z]")
;;                        (car args)))
;;   args)

 ;; Auto refresh buffers
(global-auto-revert-mode 1)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; 한글 입력기 on
(setq default-input-method "korean-hangul")
(set-language-environment "Korean")
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)               ; Default to utf-8 encoding

;; 둠의 기본 테마 설정
;; 이것 저것 많이 바꿔봤지만 역시 기본 테마가 젤 이쁘다
(setq doom-theme 'doom-one)

(setq all-the-icons-scale-factor 1.0)
;; (setq all-the-icons-rvy-rich-mode 1)
;; (ivy-rich-mode 1)

;; 노안이 왔는지 이제는 이정도 폰트 크기는 되어야 잘 보임
(setq doom-font (font-spec :family "Monaco" :size 13))

;; 한글 관련 폰트미치 스케일링 설정
(set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))
(setq face-font-rescale-alist
      '(("NanumGothicCoding" . 1.2307692307692308)))

;;고양이를 켜서 그나마 좀 재미나게 바꿔본다.
(nyan-mode)
(nyan-start-animation)
;; add icons to ivy
;; 아이비 메뉴에 아이콘이 들어가면 호박에 줄그어서 수박이 되는 경험을 할 수 있다.
;; (add-hook 'after-init-hook 'all-the-icons-ivy-setup)

(after! doom-modeline
  (setq lsp-modeline-diagnostics-scope ':buffer)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-persp-name t)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-buffer-state-icon nil)
  (setq doom-modeline-persp-icon nil)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-buffer-file-name-style 'file-name))

;; 라인 넘버표시 하지 않는게 더 빠르다
;; 이유는 모름.
;; (setq display-line-numbers-type 'relative)
(setq display-line-numbers-type nil)

;; 더블버퍼링이 동작하도록 설정한다.
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(setq scroll-conservatively 101)

;; lsp 관련 설정 메뉴들
;; 이맥스를 느리게 만드는 범인중 십중팔구 LSP가 관련되어져 있다고 함.
;; 해당 튜닝도 구글링을 통해서 찾았다.
;; (setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; 스프릿된 화면들을 넘어다닐때 아주 유용하다.
(map! "C-h" #'tmux-pane-omni-window-left)
(map! "C-j" #'tmux-pane-omni-window-down)
(map! "C-k" #'tmux-pane-omni-window-up)
(map! "C-l" #'tmux-pane-omni-window-right)

(map! :leader :prefix "g" :desc "ediff style diff from working-tree" "d" #'magit-ediff-show-working-tree)

;; go back, go references
(map! :n "gb" #'evil-jump-backward)
(map! :n "gr" #'+lookup/references)
;; 검색할때 브라우저를 찾는 수고를 줄여 준다.
(map! :leader :prefix "s" :desc "Search Google.com" "g" #'google-search)
(map! :leader :prefix "s" :desc "Search StackOverFlow" "v" #'stackoverflow-search)
(map! :leader :prefix "s" :desc "Search Buffers" "b" #'swiper-all)
(map! :leader :prefix "s" :desc "new project search file" "p" #'rg-project)
;; 가끔씩 즐겨보는 블로그들의 rss를 피드로 받아와서 읽을때 사용한다.
(map! :leader :prefix "o" :desc "Open news form RSS with ELfeed" "n" #'elfeed)
;; 버퍼끼리 화면 전환할때 프로젝트를 벗어 나지 않도록 강제한다.
(map! :leader :desc "workspace buffer list" "," #'+ivy/switch-workspace-buffer)
;; ORG 모드에서 쓰는 단축키들
(map! :leader :desc "Tangle Export" "ee" #'org-babel-tangle)
;; 커스텀 함수로 정의해둔 쉘 설정 파일불러오는 함수에게 단축기를 할당했음.
;; 자주 쓰지는 않는데 있어보이는 척 할때 아주 좋다.
(map! :leader :prefix "f" :desc "Open Shell init file on other windows" "i" #'er-find-shell-init-file)
;; 코드를 입력받아서 이쁜 화면으로 만들어주는 패키지에 단축기를 할당했다.
(map! :leader :prefix "t" :desc "Capture Code with Carbon now" "t" #'carbon-now-sh)

;; change window split mode
;; 이맥스를 넓게 쓰다가 길게 쓰다가 할때마다 자주 쓰이는 레이아웃 번경 맛집 함수
(map! :leader :prefix "t" :desc "Toggle Window Split Style" "s" #'toggle-window-split)

;; evil 에서 라인 처음과 마지막으로 더 빨리 점프할 수 있도록 한다.
(define-key evil-visual-state-map (kbd "H") 'beginning-of-line-text)
(define-key evil-visual-state-map (kbd "L") 'evil-end-of-line)
(define-key evil-normal-state-map (kbd "H") 'beginning-of-line-text)
(define-key evil-normal-state-map (kbd "L") 'evil-end-of-line)
;; evil multi edit recommanded setting
(define-key evil-visual-state-map (kbd "M-s-m") 'evil-multiedit-match-all)
(define-key evil-normal-state-map (kbd "M-s-m") 'evil-multiedit-match-all)
(define-key evil-insert-state-map (kbd "M-s-m") 'evil-multiedit-match-all)
;; 블럭 단위로 한번에 선택하고 싶을때 사용하면 좋다.
;; 기본 단축키가 너무 불편해서 변경했다.
(define-key evil-normal-state-map (kbd "M-s-k") #'er/expand-region)
(define-key evil-normal-state-map (kbd "M-s-j") #'er/contract-region)
(define-key evil-insert-state-map (kbd "M-s-k") #'er/expand-region)
(define-key evil-insert-state-map (kbd "M-s-j") #'er/contract-region)
;;ivy 미니 버퍼에서 컨트롤 키로 아이템을 선택하는건 새끼손가락에 죄를 짓는 일이다.
(map! :after ivy :map ivy-minibuffer-map "TAB" 'next-line)

;; ORG 모드에서 헤더 레벨 설정할때 쓰기 편한 단축키
(map! :after org-mode :map org-mode-map ">" 'org-cyclt-level)

;; 둠 이맥스 디스코드 채널에서 고수가 제안한 새로운 바인딩
;; https://discord.com/channels/406534637242810369/695450585758957609/759868990909841438
(after! evil
  (require 'evil-textobj-anyblock)
  (evil-define-text-object my-evil-textobj-anyblock-inner-quote
    (count &optional beg end type)
    "Select the closest outer quote."
    (let ((evil-textobj-anyblock-blocks
           '(("'" . "'")
             ("\"" . "\"")
             ("`" . "`")
             ("“" . "”"))))
      (evil-textobj-anyblock--make-textobj beg end type count nil)))

  (evil-define-text-object my-evil-textobj-anyblock-a-quote
    (count &optional beg end type)
    "Select the closest outer quote."
    (let ((evil-textobj-anyblock-blocks
           '(("'" . "'")
             ("\"" . "\"")
             ("`" . "`")
             ("“" . "”"))))
      (evil-textobj-anyblock--make-textobj beg end type count t)))

  (define-key evil-inner-text-objects-map "q" 'my-evil-textobj-anyblock-inner-quote)
  (define-key evil-outer-text-objects-map "q" 'my-evil-textobj-anyblock-a-quote)
  )

;; 다국어를 사용할때 느낀 불편함을 해소하는 것들
;; 하나의 단축기로 등록된 여러개의 입력기를 순환하며 선택한다.
(add-hook 'evil-normal-state-entry-hook 'custom-input-method-hook)
(define-key evil-normal-state-map (kbd "C-\\") #'choose-input-from-rotated-list)
(define-key evil-insert-state-map (kbd "C-\\") #'choose-input-from-rotated-list)

;; 뷰모드가 느리게 동작하고 아직 버그가 많아서 웹 모드로 바꾼다.
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.env\\'" . dotenv-mode))

;; disable CamelCase syntax
(global-subword-mode nil)

(add-hook 'web-mode-hook 'custom-vue-mode)
(add-hook 'typescript-mode-hook 'custom-ts-mode)

(after! web-mode
  (set-company-backend! 'web-mode '(company-capf company-yasnippet)))

(after! typescript-mode
  (set-company-backend! 'typescript-mode '(company-tabnine company-capf company-yasnippet)))

(setq flycheck-global-modes '(not gfm-mode forge-post-mode gitlab-ci-mode dockerfile-mode Org-mode org-mode))
;; all-the-icons에 아이콘 색깔을 바꾸기 위해서 수동으로 설정한다.
(add-hook 'company-mode-hook 'company-box-mode)
(setq company-box-icons-alist 'company-box-icons-idea)

(setq company-tooltip-minimum-width 60)
(setq company-tooltip-maximum-width 60)
(setq company-box-doc-enable nil)

;; 린트 에러 버퍼를 오픈하면 포커스가 자동으로 이동하지 않는다.
;; 이거 없으면 생각보다 귀찮아진다.
(add-hook 'flycheck-error-list-mode-hook (lambda () (switch-to-buffer-other-window "*Flycheck errors*")))

(add-hook! 'lsp-completion-mode-hook
  (defun init-company-tabnine-h ()
    (when lsp-completion-mode
      (setq-local company-backends (cons 'company-tabnine company-backends)))))

;; dash docs setup
;; 무슨 이유인지 모르겠으나 dash docs를 자동 인식 못함. 수동으로 추가해줌
(setq dash-docs-docsets (list "VueJS" "TypeScript" "MomentJS" "NodeJS" "Stylus" "Pug" "HTML" "CSS"
                              "Express" "Mongoose" "ElasticSearch" "Docker" "Jekyll" "Mocha"
                              "Bootstrap_4" "Bootstrap_5" "jQuery" "React" "Dart"))

;; 1초라도 빨리 팝업 띄우고 싶어서
;; 그러나 실제 체감속도 향상은 없음
(setq company-idle-delay 0.0)

;; elfeed 관련 목록 커스텀 컬럼 설정
(after! elfeed
  (setq elfeed-search-print-entry-function #'feed-reader/search-print))

;; lsp 설정 이후에 불필요한 옵션들은 전부다 끈다.
(after! lsp
  ;; These take up a lot of space on my big font size
  (setq lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-signature-render-all nil))

;; vc & magit 관련 설정
(setq vc-follow-symlinks t)
(setq find-file-visit-truename t)
(setq magit-refresh-status-buffer 'switch-to-buffer)
(setq magit-rewrite-inclusive 'ask)
(setq magit-save-some-buffers t)
(setq magit-set-upstream-on-push 'askifnotset)
(setq magit-diff-refine-hunk 'all)

(magit-delta-mode)

(setq ghub-use-workaround-for-emacs-bug 'force)
(setq forge-topic-list-limit '(200 . 10))

;; ediff를 닫을때 항상 물어보는 거 금지!!
(defadvice! shut-up-ediff-quit (orig-fn &rest args)
  :around #'ediff-quit
  (letf! (defun y-or-n-p (&rest _) t)
    (apply orig-fn args)))

(after! forge
  (setq auth-sources '("~/.authinfo"))
  (add-to-list 'forge-alist '("gitlab.jjsoft.kr" "gitlab.jjsoft.kr/api/v4" "gitlab.jjsoft.kr" forge-gitlab-repository))
  ;; O-T (Open This)바인딩으로 브라우저에서 링크를 열 수 있도록 지원한다.
  (define-key forge-topic-title-section-map (kbd "ot") 'forge-custom-open-url)
  (define-key forge-topic-marks-section-map (kbd "ot") 'forge-custom-open-url)
  (define-key forge-topic-state-section-map (kbd "ot") 'forge-custom-open-url)
  (define-key forge-topic-labels-section-map (kbd "ot") 'forge-custom-open-url)
  (define-key forge-topic-milestone-section-map (kbd "ot") 'forge-custom-open-url)
  (define-key forge-topic-assignees-section-map (kbd "ot") 'forge-custom-open-url)
  (define-key forge-post-section-map (kbd "ot") 'forge-custom-open-url)
  ;; Y-T (Yank This)바인딩으로 이슈와 커멘트들의 링크를 복사한다.
  (define-key forge-topic-title-section-map (kbd "yt") 'forge-copy-url-at-point-as-kill)
  (define-key forge-topic-marks-section-map (kbd "yt") 'forge-copy-url-at-point-as-kill)
  (define-key forge-topic-state-section-map (kbd "yt") 'forge-copy-url-at-point-as-kill)
  (define-key forge-topic-labels-section-map (kbd "yt") 'forge-copy-url-at-point-as-kill)
  (define-key forge-topic-milestone-section-map (kbd "yt") 'forge-copy-url-at-point-as-kill)
  (define-key forge-topic-assignees-section-map (kbd "yt") 'forge-copy-url-at-point-as-kill)
  (define-key forge-post-section-map (kbd "yt") 'forge-copy-url-at-point-as-kill)
  ;; E-T i(Edit This)바인딩으로 간편하게 모든걸 수정하자
  (define-key forge-topic-title-section-map (kbd "et") 'forge-edit-topic-title)
  (define-key forge-topic-marks-section-map (kbd "et") 'forge-edit-topic-marks)
  (define-key forge-topic-state-section-map (kbd "et") 'forge-edit-topic-state)
  (define-key forge-topic-labels-section-map (kbd "et") 'forge-edit-topic-labels)
  (define-key forge-topic-milestone-section-map (kbd "et") 'forge-edit-topic-milestone)
  (define-key forge-topic-assignees-section-map (kbd "et") 'forge-edit-topic-assignees)
  (define-key forge-post-section-map (kbd "et") 'forge-edit-post)
  (define-key forge-post-section-map (kbd "dt") 'forge-delete-comment)
  (define-key forge-topic-mode-map (kbd "ar") 'forge-create-post)
  ;; 팝업을 별도의 버퍼로 띄우도록 한다.
  ;; (setq magit-display-buffer-function #'+magit-my-display-buffer-fn)
  (setq markdown-display-remote-images t)
  )

;; start my org settings

;; config some hooks
(add-hook 'org-mode-hook 'my-org-config/after-org-mode-load)

;;basic org mode config
(setq
  org-hide-emphasis-markers t
  org-log-state-notes-into-drawer t
  org-directory "~/org/"
  org-agenda-skip-scheduled-if-done t
  org-ellipsis " ▾ "
  org-tags-column -80
  org-agenda-span 30
  org-agenda-files '("~/org")
  org-log-done 'time
  org-refile-targets (quote ((nil :maxlevel . 1)))
  ;; org-capture-templates '(("x" "JW.ORG" entry
  ;;                         (file+olp+datetree "jw.org")
  ;;                         "**** [ ] %U %?" :prepend t :kill-buffer t)
  ;;                         ("t" "JJSOFT" entry
  ;;                         (file+headline "jjsoft.org")
  ;;                         "* [ ] %?\n%i" :prepend t :ill-buffer t))
  +doom-dashboard-banner-file (expand-file-name "logo.png" doom-private-dir)
  +org-capture-todo-file "tasks.org")

;; config org-super-agenda
(setq org-super-agenda-mode t)
(setq org-super-agenda-header-map nil)
(setq org-deadline-warning-days 7)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-block-separator 9472)
(setq org-agenda-start-on-weekday nil)
(setq org-super-agenda-groups '((:name "Today"
                                   :time-grid t
                                   :scheduled today)
                                (:name "Due today"
                                    :deadline today)
                                (:name "Important"
                                    :priority "A")
                                (:name "Overdue"
                                    :deadline past)
                                (:name "Due soon"
                                    :deadline future)))

(after! org
  (set-face-attribute 'org-link nil :weight 'normal :background nil)
  (set-face-attribute 'org-code nil :foreground "#a9a1e1" :background nil)
  (set-face-attribute 'org-date nil :foreground "#5B6268" :background nil)
  (set-face-attribute 'org-level-1 nil :foreground "steelblue2" :background nil :height 1.2 :weight 'normal)
  (set-face-attribute 'org-level-2 nil :foreground "slategray2" :background nil :height 1.0 :weight 'normal)
  (set-face-attribute 'org-level-3 nil :foreground "SkyBlue2" :background nil :height 1.0 :weight 'normal)
  (set-face-attribute 'org-level-4 nil :foreground "DodgerBlue2" :background nil :height 1.0 :weight 'normal)
  (set-face-attribute 'org-level-5 nil :weight 'normal)
  (set-face-attribute 'org-level-6 nil :weight 'normal)
  (set-face-attribute 'org-document-title nil :foreground "SlateGray1" :background nil :height 1.75 :weight 'bold)
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕"))
  (setq org-fontify-quote-and-verse-blocks nil
        org-fontify-whole-heading-line nil
        org-hide-leading-stars nil
        org-startup-indented nil)
  (set-face-attribute 'org-document-title nil
                      :foreground "White"
                      :height 1.2
                      :weight 'bold))

;; 기본 단추들이 맘에 안들어서 커보이는 것들 순으로 다시 조정했다.
(use-package org-bullets
  :init
  (setq org-bullets-bullet-list '("✸" "✸" "✸" "✸" "✸"))
  :config
  (add-hook 'org-mode-hook
            (lambda () (org-bullets-mode 1)))
)

(setq org-edit-src-content-indentation 0)
(setq org-src-tab-acts-natively t)
(setq org-src-preserve-indentation t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ts . t)
   (js . t)
   (emacs-lisp . t)
   (python . t)
   (shell . t)
   ))

(cl-defmacro lsp-org-babel-enable (lang)
  "Support LANG in org source code block."
  (setq centaur-lsp 'lsp-mode)
  (cl-check-type lang stringp)
  (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
         (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
    `(progn
       (defun ,intern-pre (info)
         (let ((file-name (->> info caddr (alist-get :file))))
           (unless file-name
             (setq file-name (make-temp-file "babel-lsp-")))
           (setq buffer-file-name file-name)
           (lsp-deferred)))
       (put ',intern-pre 'function-documentation
            (format "Enable lsp-mode in the buffer of org source block (%s)."
                    (upcase ,lang)))
       (if (fboundp ',edit-pre)
           (advice-add ',edit-pre :after ',intern-pre)
         (progn
           (defun ,edit-pre (info)
             (,intern-pre info))
           (put ',edit-pre 'function-documentation
                (format "Prepare local buffer environment for org source block (%s)."
                        (upcase ,lang))))))))
(defvar org-babel-lang-list
  '("emacs-lisp" "python" "javascript" "bash" "sh" "vue"))
(dolist (lang org-babel-lang-list)
  (eval `(lsp-org-babel-enable ,lang)))

(use-package! org-mac-link
  :after org
  :config
  (setq org-mac-grab-Acrobat-app-p nil)
  (setq org-mac-grab-devonthink-app-p nil)
  (map! :leader
        :map org-mode-map
        :desc "link from mac apps"
        "mlm"  #'org-mac-grab-link))
(setq org-html-htmlize-output-type 'css)
(setq org-download-method 'attach)
(setq global-org-pretty-table-mode t)
;; (use-package org-weather)
;; (setq org-weather-location "Daejoen,KR")
;; (org-weather-refresh)

(setq elfeed-feeds '(
"http://www.bloter.net/feed"
"https://blog.elementary.io/feed.xml"
"https://d2.naver.com/d2.atom"
"https://engineering.linecorp.com/ko/feed/"
"https://tech.lezhin.com/rss/"
"https://emacsredux.com/atom.xml"
"http://sachachua.com/blog/category/emacs/feed"
"https://planet.emacslife.com/atom.xml"
"https://www.emacswiki.org/emacs?action=rss;match=%5E%5Cd%5Cd%5Cd%5Cd-%5Cd%5Cd-%5Cd%5Cd"
"https://feeds.feedburner.com/zdkorea"
"https://www.producthunt.com/feed?category=undefined"
"https://www.reddit.com/r/linux.rss"
"https://www.gamingonlinux.com/article_rss.php"
"https://hackaday.com/blog/feed/"
"https://opensource.com/feed"
"https://linux.softpedia.com/backend.xml"
"https://itsfoss.com/feed/"
"https://www.zdnet.com/topic/linux/rss.xml"
"https://www.phoronix.com/rss.php"
"http://feeds.feedburner.com/d0od"
"https://www.computerworld.com/index.rss"
"https://www.networkworld.com/category/linux/index.rss"
"https://www.techrepublic.com/rssfeeds/topic/open-source/"
"https://betanews.com/feed"
"http://lxer.com/module/newswire/headlines.rss"
"https://distrowatch.com/news/dwd.xml"
))

(use-package rg
  :config
  (setq rg-group-result t)
  (setq rg-hide-command t)
  (setq rg-show-columns nil)
  (setq rg-show-header t)
  (setq rg-custom-type-aliases nil)
  (setq rg-default-alias-fallback "all"))

;; 버퍼가 열리면 포커스를 그쪽으로 이동시킨다.
;; 이거 없으면 생각보다 귀찮아진다.
(add-hook 'rg-mode-hook (lambda () (switch-to-buffer-other-window "*rg*")))

(add-to-list 'load-path "/usr/local/Cellar/mu/1.4.13/share/emacs/site-lisp/mu/mu4e")
(use-package! mu4e)
(after! mu4e
  (message "init mu4e variables")
  (setq mu4e-attachment-dir "~/Downloads"
        mu4e-compose-signature-auto-include t
        mu4e-get-mail-command "mbsync -a"
        mu4e-maildir "~/Mailbox"
        mu4e-update-interval 60
        mu4e-use-fancy-chars t
        mu4e-view-show-addresses t
        mu4e-view-show-images t
        mu4e-index-update-in-background t
        mu4e-compose-signature-auto-include t
        mu4e-compose-format-flowed t
        ;; +mu4e-min-header-frame-width 142
        mu4e-headers-date-format "%y/%m/%d"
        mu4e-headers-time-format "%H:%M:%S"
        mu4e-index-cleanup t)
  ;; 메일 목록 화면에서 컬럼 사이즈를 재조정한다.
  (setq mu4e-headers-fields '((:human-date    . 12)
                              (:flags         . 6)
                              ;; (:acctshortname . 4)
                              ;; (:foldername    . 25)
                              (:from-or-to    . 30)
                              ;(:size          . 6)
                              (:subject       . nil)))
  ;;메일 폴더를 빠르게 선택할 수 있는 단축키도 지정한다.
  (setq mu4e-maildir-shortcuts '((:maildir "/jjpark78@outlook.com/inbox"   :key ?i)
                                 (:maildir "/jjpark78@outlook.com/sent"    :key ?s)
                                 ))
)

(set-email-account! "Outlook"
                    '((user-full-name         . "Jaejin Park")
                      (smtpmail-smtp-server   . "smtp.office365.com")
                      (smtpmail-smtp-service  . 587)
                      (smtpmail-stream-type   . starttls)
                      (smtpmail-debug-info    . t)
                      (mu4e-drafts-folder     . "/Drafts")
                      (mu4e-refile-folder     . "/Archive")
                      (mu4e-sent-folder       . "/Sent Items")
                      (mu4e-trash-folder      . "/Deleted Items")
                      ;(mu4e-sent-messages-behavior . 'delete)
                      )
                    nil)

(use-package! mu4e-views
  :after mu4e
  :defer nil
  :bind (:map mu4e-headers-mode-map
	    ("v" . mu4e-views-mu4e-select-view-msg-method) ;; select viewing method
	    ("M-n" . mu4e-views-cursor-msg-view-window-down) ;; from headers window scroll the email view
	    ("M-p" . mu4e-views-cursor-msg-view-window-up) ;; from headers window scroll the email view
	    )
  :config
  (setq mu4e-views-completion-method 'ivy) ;; use ivy for completion
  (setq mu4e-views-default-view-method "html") ;; make xwidgets default
  (mu4e-views-mu4e-use-view-msg-method "html") ;; select the default
  (setq mu4e-views-next-previous-message-behaviour 'always-switch-to-view))

(use-package mu4e-alert
  :config
  (message "loaded mu4e-alert")
  (mu4e-alert-set-default-style 'notifier)
  (mu4e-alert-enable-notifications)
  (mu4e-alert-enable-mode-line-display)
)
