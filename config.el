(load! "+custom_funcs")
(load! "+private")

;; 사용자 이름 설정
(setq user-full-name "Jaejin Park"
      user-mail-address "jjpark78@outlook.com")
(setq-default frame-title-format '("DOOM EMACS"))

;; 바쁘게 버퍼전환 하는 와중에 화면이 울렁거리는게 멀미날것 같아서 프리뷰 옵션을 껐다
(setq +ivy-buffer-preview nil)
(setq avy-all-windows t)
(setq ivy-read-action-function #'ivy-hydra-read-action)
;; dired를 두개 열어 놓고 왔다 갔다하며 복사 붙여넣기 할때 편하다
;; (setq dired-dwim-target t)

(setq confirm-kill-emacs nil)
(setq which-key-idle-delay 0.5)
(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))

;; 수동으로 직접 프레임 위치를 지정해줘 본다.
;; (setq initial-frame-alist '((top . 23) (left . 1175) (width . 125) (height . 80)))

(global-evil-matchit-mode)

;; 스나이프를 화면 보이는 영역으로 제한한다.
(setq evil-snipe-scope 'whole-visible)
(setq evil-snipe-repeat-scope 'whole-visible)

;; 와카타임이라고 본인이 얼마나 열심히 일했는지
;; 하루 일과를 잘 보여준다.
;; 개인으로만 쓰면 공짜인것도 매력임.
(global-wakatime-mode)

;; 한글 입력기 on
(setq default-input-method "korean-hangul")
(set-language-environment "Korean")
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;; (set-default-coding-systems 'utf-8)               ; Default to utf-8 encoding
(require 'ucs-normalize)
(set-file-name-coding-system 'utf-8-hfs)

;; 이것 저것 많이 바꿔봤지만 역시 기본 테마가 젤 이쁘다
(setq doom-theme 'doom-nord)

;; 노안이 왔는지 이제는 이정도 폰트 크기는 되어야 잘 보임
(setq doom-font (font-spec :family "monaco" :size 14))

(set-fringe-style '(nil . 0))

;; 한글 관련 폰트 스케일링 설정
;; (set-face-attribute 'default nil :height 130)
(set-fontset-font t 'hangul (font-spec :name "AppleGothic"))
(set-fontset-font t 'japanese-jisx0213.2004-1 (font-spec :name "AppleGothic"))
(set-fontset-font t 'katakana-jisx0201 (font-spec :name "AppleGothic"))

(setq face-font-rescale-alist
      '(("NanumGothicCoding" . 1.2307692307692308)
        ("AppleGothic" . 1.2307692307692308)
        ))

(after! doom-modeline
  (setq
    doom-modeline-major-mode-icon t
    doom-modeline-buffer-encoding nil
    doom-modeline-mu4e t
    doom-modeline-buffer-file-name-style 'truncate-with-project))

;; 라인 넘버표시 하지 않는게 더 빠르다
;; 이유는 모름.
;; (setq display-line-numbers-type 'relative)
;; (setq display-line-numbers-type t)
(setq display-line-numbers-type nil)

;; 필요없는 부분은 동작하지 않는 특수 모드 활성화
(global-so-long-mode 1)

;; 더블버퍼링이 동작하도록 설정한다.
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(setq scroll-conservatively 101)

;; lsp 관련 설정 메뉴들
;; 이맥스를 느리게 만드는 범인중 십중팔구 LSP가 관련되어져 있다고 함.
;; 해당 튜닝도 구글링을 통해서 찾았다.
(setq gc-cons-threshold 1000000000)
(setq read-process-output-max (* 1024 1024))

(map! "C-h" #'tmux-pane-omni-window-left)
(map! "C-j" #'tmux-pane-omni-window-down)
(map! "C-k" #'tmux-pane-omni-window-up)
(map! "C-l" #'tmux-pane-omni-window-right)
;; (map! "C-h" #'evil-window-left)
;; (map! "C-j" #'evil-window-down)
;; (map! "C-k" #'evil-window-up)
;; (map! "C-l" #'evil-window-right)
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
(map! :leader :prefix "o" :desc "Open mu4e to current window" "m" #'mu4e)
;; 버퍼끼리 화면 전환할때 프로젝트를 벗어 나지 않도록 강제한다.
(map! :leader :desc "workspace buffer list" "," #'+ivy/switch-workspace-buffer)
;; ORG 모드에서 쓰는 단축키들
(map! :leader :desc "Tangle Export" "ee" #'org-babel-tangle)
;; 커스텀 함수로 정의해둔 쉘 설정 파일불러오는 함수에게 단축기를 할당했음.
;; 자주 쓰지는 않는데 있어보이는 척 할때 아주 좋다.
(map! :leader :prefix "f" :desc "Open Shell init file on other windows" "i" #'er-find-shell-init-file)
(map! :leader :prefix "f" :desc "Open alacritty init file on other windows" "a" #'er-find-alacritty-init-file)
;; 코드를 입력받아서 이쁜 화면으로 만들어주는 패키지에 단축기를 할당했다.
(map! :leader :prefix "t" :desc "Capture Code with Carbon now" "t" #'carbon-now-sh)
;; change window split mode
;; 이맥스를 넓게 쓰다가 길게 쓰다가 할때마다 자주 쓰이는 레이아웃 번경 맛집 함수
(map! :leader :prefix "t" :desc "Toggle Window Split Style" "s" #'toggle-window-split)
;; ace-window
(map! :leader :prefix "w" :desc "open ace window to select window" "a" #'ace-window)
;; evil 에서 라인 처음과 마지막으로 더 빨리 점프할 수 있도록 한다.
(map! :leader :prefix "c" :desc "run npm script" "n" #'npm-mode-npm-run)
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
;; <SPC> w C-o 는 너무 누르기 힘들지만 이게 의외로 많이 쓰인다. 쓰이지 않는 키 바인딩에 할당해서 더 간단히 만든다.
(map! :leader :prefix "w" :desc "Close Other Windows Fast Binding" "O" 'delete-other-windows)
(map! :leader :n "," 'switch-to-buffer)

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

;; 뷰모드가 느리게 동작하고 아직 버그가 많아서 웹 모드로 바꾼다.
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.env\\'" . dotenv-mode))

;; disable CamelCase syntax
(global-subword-mode nil)

;; disable lsp-formating
(setq +format-with-lsp nil)

(add-hook 'web-mode-hook 'custom-vue-mode)
(add-hook 'typescript-mode-hook 'custom-ts-mode)
(add-hook 'cc-mode-hook 'custom-cc-mode)

;; (after! typescript-mode
;;   (set-company-backend! 'typescript-mode '(company-tabnine company-capf company-yasnippet)))
(setq flycheck-global-modes '(not conf-colon-mode gfm-mode forge-post-mode gitlab-ci-mode dockerfile-mode Org-mode org-mode))

;; all-the-icons에 아이콘 색깔을 바꾸기 위해서 수동으로 설정한다.
;; (add-hook 'company-mode-hook 'company-box-mode)
;; (setq company-box-icons-alist 'company-box-icons-idea)
;; (setq company-tooltip-minimum-width 60)
;; (setq company-tooltip-maximum-width 60)
;; (setq company-box-doc-enable nil)

;; 린트 에러 버퍼를 오픈하면 포커스가 자동으로 이동하지 않는다.
;; 이거 없으면 생각보다 귀찮아진다.
(add-hook 'flycheck-error-list-mode-hook (lambda () (switch-to-buffer-other-window "*Flycheck errors*")))

(after! ccls
  (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
  (set-lsp-priority! 'ccls 2)) ; optional as ccls is the default in Doom

(use-package company-tabnine
  :defer 1
  :custom
  (company-tabnine-max-num-results 9)
  :hook
  (lsp-after-open . (lambda ()
                      (setq company-tabnine-max-num-results 5)
                      (add-to-list 'company-transformers 'company//sort-by-tabnine t)
                      (add-to-list 'company-backends '(company-capf :with company-tabnine :separate))))
  (kill-emacs . company-tabnine-kill-process)
  :config
  ;; Enable TabNine on default
  (add-to-list 'company-backends #'company-tabnine)

  ;; Integrate company-tabnine with lsp-mode
  (defun company//sort-by-tabnine (candidates)
    (if (or (functionp company-backend)
            (not (and (listp company-backend) (memq 'company-tabnine company-backends))))
        candidates
      (let ((candidates-table (make-hash-table :test #'equal))
            candidates-lsp
            candidates-tabnine)
        (dolist (candidate candidates)
          (if (eq (get-text-property 0 'company-backend candidate)
                  'company-tabnine)
              (unless (gethash candidate candidates-table)
                (push candidate candidates-tabnine))
            (push candidate candidates-lsp)
            (puthash candidate t candidates-table)))
        (setq candidates-lsp (nreverse candidates-lsp))
        (setq candidates-tabnine (nreverse candidates-tabnine))
        (nconc (seq-take candidates-tabnine 5)
               (seq-take candidates-lsp 6))))))

;; 1초라도 빨리 팝업 띄우고 싶어서
;; 그러나 실제 체감속도 향상은 없음
(setq company-idle-delay 0.0)

;; Dash Documents랑 연동이 되도록 각각 메이저에 관련 정보들을 추가한다.
(set-docsets! 'c++-mode "Qt" "C++" "C")
(set-docsets! 'cc-mode "Qt" "C++" "C")
(set-docsets! 'web-mode   "TypeScript" "NodeJS" "HTML" "CSS" "Pug" "Stylus" "VueJS")
(set-docsets! 'typescript "TypeScript" "NodeJS" "HTML" "CSS" "Pug" "Stylus" "VueJS")

;; lsp 설정 이후에 불필요한 옵션들은 전부다 끈다.
(after! lsp
  ;; These take up a lot of space on my big font size
  (setq lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-modeline-diagnostics-mode nil
        lsp-modeline-diagnostics-enable nil
        lsp-signature-render-all nil))

;; vc & magit 관련 설정
(setq vc-follow-symlinks t)
(setq find-file-visit-truename t)
(setq magit-refresh-status-buffer 'switch-to-buffer)
(setq magit-rewrite-inclusive 'ask)
(setq magit-save-some-buffers t)
(setq magit-set-upstream-on-push 'askifnotset)
(setq magit-diff-refine-hunk 'all)

;; (magit-delta-mode)
(magit-todos-mode)
;; (setq ghub-use-workaround-for-emacs-bug 'force)
(setq forge-topic-list-limit '(200 . 10))

;; ediff를 닫을때 항상 물어보는 거 금지!!
(defadvice! shut-up-ediff-quit (orig-fn &rest args)
  :around #'ediff-quit
  (letf! (defun y-or-n-p (&rest _) t)
    (apply orig-fn args)))

(after! git-link
  (setq git-link-default-remote "upstream"
        git-link-default-branch "develop"
        git-link-open-in-browser nil
  )
  (map! :leader :prefix "g" :desc "get remote link using git-link"  "k" #'git-link)
)

(after! forge
  (setq auth-sources '("~/.authinfo"))
  (add-to-list 'forge-alist '("gitlab.com" "gitlab.com/api/v4" "gitlab.com" forge-gitlab-repository))
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

  ;;section visibility
  (setq magit-section-initial-visibility-alist
        '((stashes . show)
          (untracked . show)
          (unstaged . show)
          (staged . show)
          (unpushed . show)
          (todos . show)
          (issues . show)
          (pullreqs . show)))
  )

(add-to-list 'load-path "/usr/local/Cellar/mu/1.4.13/share/emacs/site-lisp/mu/mu4e")
(use-package! mu4e)
(after! mu4e
  (message "init mu4e variables")
  (setq mu4e-attachment-dir "~/Downloads"
        mu4e-compose-signature-auto-include t
        mu4e-get-mail-command "true"
        mu4e-maildir "~/Mailbox"
        mu4e-update-interval nil
        mu4e-use-fancy-chars t
        mu4e-view-show-addresses t
        mu4e-view-show-images t
        mu4e-index-update-in-background t
        mu4e-index-update-error-warning nil
        mu4e-confirm-quit nil
        mu4e-compose-format-flowed t
        ;; +mu4e-min-header-frame-width 142
        mu4e-headers-date-format "%y/%m/%d"
        mu4e-headers-time-format "%H:%M:%S"
        mu4e-index-cleanup t)

  ;; 메일 목록 화면에서 컬럼 사이즈를 재조정한다.
  (setq mu4e-headers-fields '((:human-date . 10)
                              (:subject    . nil)))
  ;;메일 폴더를 빠르게 선택할 수 있는 단축키도 지정한다.
  (setq mu4e-maildir-shortcuts '((:maildir "/jjpark78@gmail.com/inbox"   :key ?i)
                                 (:maildir "/jjpark78@gmail.com/sent"    :key ?s)
                                 ))
  ;;리플라이나 포워딩을 할때 원본 메세지의 받은 주소를 자동으로 보내는 사람 필드에 설정한다.
  (add-hook 'mu4e-compose-pre-hook
            (defun my-set-from-address ()
              "Set the From address based on the To address of the original."
              (let ((msg mu4e-compose-parent-message)) ;; msg is shorter...
                (when msg
                  (setq user-mail-address
                        (cond
                         ((mu4e-message-contact-field-matches msg :to "jjpark@jjsoft.kr") "jjpark@jjsoft.kr")
                         ((mu4e-message-contact-field-matches msg :to "jjpark78@outlook.com") "jjpark78@outlook.com")
                         ((mu4e-message-contact-field-matches msg :to "pjj78@naver.com") "pjj78@naver.com")
                         ((mu4e-message-contact-field-matches msg :to "admin@jjsoft.kr") "admin@jjsoft.kr")
                         (t "jjpark78@gmail.com")))))))
  )

(set-email-account! "Gmail"
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
  (setq mu4e-views-mu4e-html-email-header-style
          "<style type=\"text/css\">
  .mu4e-mu4e-views-mail-headers { font-family: sans-serif; font-size: 10pt; margin-bottom: 30px; padding-bottom: 10px; border-bottom: 1px solid #ccc; color: #000;}
  .mu4e-mu4e-views-header-row { display:block; padding: 1px 0 1px 0; }
  .mu4e-mu4e-views-mail-header { display: inline-block; text-transform: capitalize; font-weight: bold; }
  .mu4e-mu4e-views-header-content { display: inline-block; padding-right: 8px; }
  .mu4e-mu4e-views-email { display: inline-block; padding-right: 8px; }
  .mu4e-mu4e-views-attachment { display: inline-block; padding-right: 8px; }
  </style>")
  (setq mu4e-views-completion-method 'ivy) ;; use ivy for completion
  (setq mu4e-views-default-view-method "browser") ;; make xwidgets default
  (mu4e-views-mu4e-use-view-msg-method "browser") ;; select the default
  (setq mu4e-views-next-previous-message-behaviour 'stick-to-current-window)
  (map! :map mu4e-headers-mode-map
        :n "M-b" #'mu4e-views-cursor-msg-view-window-up
        :n "M-f" #'mu4e-views-cursor-msg-view-window-down
        :localleader
        :desc "Message action"        "a"   #'mu4e-views-mu4e-view-action
        :desc "Scoll message down"    "b"   #'mu4e-views-cursor-msg-view-window-up
        :desc "Scoll message up"      "f"   #'mu4e-views-cursor-msg-view-window-down
        :desc "Open attachment"       "o"   #'mu4e-views-mu4e-view-open-attachment
        :desc "Save attachment"       "s"   #'mu4e-views-mu4e-view-save-attachment
        :desc "Save all attachments"  "S"   #'mu4e-views-mu4e-view-save-all-attachments
        :desc "Set view method"       "v"   #'mu4e-views-mu4e-select-view-msg-method)) ;; select viewing method)

(use-package mu4e-alert
  :config
  (message "loaded mu4e-alert")
  (mu4e-alert-set-default-style 'notifier)
  (mu4e-alert-enable-notifications)
  )

(defun refresh-mu4e-alert-mode-line ()
  (interactive)
  (call-process-shell-command "~/.doom.d/update_mail.sh" nil 0)
  (mu4e-alert-enable-mode-line-display))

(run-with-timer 0 60 'refresh-mu4e-alert-mode-line)

;; start my org settings
;; config some hooks
(after! org
  (add-hook 'org-mode-hook 'my-org-config/after-org-mode-load)
  ;;basic org mode config
  (setq
   org-fontify-quote-and-verse-blocks nil
   org-fontify-whole-heading-line nil
   org-hide-leading-starts nil
   org-startup-indented nil
   org-hide-emphasis-markers t
   org-directory "~/org/"
   org-agenda-skip-scheduled-if-done t
   org-ellipsis " ▾ "
   org-tags-column -80
   org-agenda-span 30
   org-agenda-files '("~/org")
   org-log-done 'time
   org-refile-targets (quote ((nil :maxlevel . 1)))
   +org-capture-todo-file "tasks.org"
   org-edit-src-content-indentation 0
   org-src-tab-acts-natively t
   org-src-preserve-indentation t
   ;; config org-super-agenda
   org-super-agenda-mode t
   org-super-agenda-header-map nil
   org-deadline-warning-days 7
   org-agenda-skip-scheduled-if-done t
   org-agenda-block-separator 9472
   org-agenda-start-on-weekday nil
   org-super-agenda-groups '((:name "Today"
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
  ;; org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕"))
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
  (set-face-attribute 'org-document-title nil
                      :foreground "White"
                      :height 1.2
                      :weight 'bold)

  ;; 기본 단추들이 맘에 안들어서 커보이는 것들 순으로 다시 조정했다.
  (use-package org-bullets
    :init
    (setq org-bullets-bullet-list '("✸" "✸" "✸" "✸" "✸"))
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
  ;;기타 ORG모드 설정
  (use-package! org-mac-link
    :after org
    :config
    (setq
     org-mac-grab-Acrobat-app-p nil
     org-mac-grab-devonthink-app-p nil
     org-html-htmlize-output-type 'css
     org-download-method 'attach
     global-org-pretty-table-mode t)
    (map! :leader
          :map org-mode-map
          :desc "link from mac apps"
          "mlm"  #'org-mac-grab-link))
  )

(setq elfeed-feeds '(
    "http://www.bloter.net/feed"
    "https://d2.naver.com/d2.atom"
    "https://engineering.linecorp.com/ko/feed/"
    "https://tech.lezhin.com/rss/"
    "https://emacsredux.com/atom.xml"
    "http://sachachua.com/blog/category/emacs/feed"
    "https://planet.emacslife.com/atom.xml"
    "https://www.emacswiki.org/emacs?action=rss;match=%5E%5Cd%5Cd%5Cd%5Cd-%5Cd%5Cd-%5Cd%5Cd"
    "https://feeds.feedburner.com/zdkorea"
    "https://www.reddit.com/r/linux.rss"
))

(use-package rg
  :config
  (setq rg-group-result t
        rg-hide-command t
        rg-show-columns nil
        rg-show-header t
        rg-custom-type-aliases nil
        rg-default-alias-fallback "all")
  ;; 버퍼가 열리면 포커스를 그쪽으로 이동시킨다.
  ;; 이거 없으면 생각보다 귀찮아진다.
  (add-hook 'rg-mode-hook (lambda () (switch-to-buffer-other-window "*rg*"))))
