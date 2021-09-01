(defun setup-custom-jsts-mode ()
  ;; 기본 인덴테이션을 설정한다.
  (lsp)
  (setq typescript-indent-level 2)
  (setq emmet-indentation 2)
  (setq js-indent-level 2)
  ;; (setq global-git-gutter-mode t)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (flycheck-mode +1)
  (my/use-eslint-from-node-modules)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  (setq lsp-ui-peek-fontify 'always)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  )

(defun custom-ts-mode ()
  (if (not (equal buffer-file-name 'nil))
      (let ((extname (file-name-extension buffer-file-name)))
        (when (or (string-equal "tsx" extname)
                  (string-equal "ts" extname))
          (setup-custom-jsts-mode)
          (flycheck-select-checker 'javascript-eslint)))))

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-exists-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(defun custom-web-mode ()
  "Custom hooks for vue-mode"
  (if (not (equal buffer-file-name 'nil))
      (let ((extname (file-name-extension buffer-file-name)))
        (when (string-equal "vue" extname)
          (setup-custom-jsts-mode)
          (flycheck-select-checker 'javascript-eslint)
          ))))

(defun my-org-config/after-org-mode-load ()
  ;; (visual-line-mode)
  (require 'org-indent)
  (org-indent-mode)
  )

; 어쩌다가 한번 수행하는 함수, 아래의 파일로 만들어 관리하도록 하는데 실수로 저장된 파일을 날려먹거나 하면
; 이 함수를 수행해서 다시 값을 복구 한다.
(defun my/search-org-project-files()
  (interactive)
  (async-start
    ;;시간이 많이 걸리기 때문에 비동기 모드로 수행한다.
      (lambda ()
        (append (directory-files-recursively "~/develop/jltech/" "\\.org$")
                (directory-files-recursively "/mnt/c/Users/jaejinpark/Development" "\\.org$")
                (directory-files-recursively "~/org" "\\.org$")))
      (lambda (result)
        (setq org-agenda-files result))))

  (defvar org-agenda-list-save-path
    "~/.doom.d/org-agenda-list.el"
  "Path to save the list of files belonging to the agenda.")

  (defun org-agenda-save-file-list ()
    "Save list of desktops from file in org-agenda-list-save-path"
    (interactive)
    (save-excursion
      (let ((buf (find-file-noselect org-agenda-list-save-path)))
        (set-buffer buf)
        (erase-buffer)
        (print (list 'quote org-agenda-files) buf)
        (save-buffer)
        (kill-buffer)
        (message "org-agenda file list saved to: %s" org-agenda-list-save-path))))

  (defun org-agenda-load-file-list ()
    "Load list of desktops from file in org-agenda-list-save-path"
    (interactive)
    (save-excursion
      (let ((buf (find-file-noselect org-agenda-list-save-path)))
        (set-buffer buf)
        (setq org-agenda-files (eval (read (buffer-string))))
        (kill-buffer)
        (message "org-agenda file list loaded from: %s" org-agenda-list-save-path))))

(defun feed-reader/search-print (entry)
      "Print ENTRY to the buffer."
      (let* ((feed-width 16)
              (tags-width 8)
              (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
              (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
              (feed (elfeed-entry-feed entry))
              (feed-title
              (when feed
              (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
              (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
              (tags-str (concat "[" (mapconcat 'identity tags ",") "]"))
              (title-width (- (window-width) feed-width tags-width 4))
              (title-column (elfeed-format-column
                              title (elfeed-clamp
                              elfeed-search-title-min-width
                              title-width
                              elfeed-search-title-max-width)
                              :left))
              (tag-column (elfeed-format-column
                      tags-str (elfeed-clamp (length tags-str) tags-width tags-width)
                      :left))
              (feed-column (elfeed-format-column
                      feed-title (elfeed-clamp feed-width feed-width feed-width)
                      :left)))
      (insert (propertize feed-column 'face 'elfeed-search-feed-face) " ")
      (insert (propertize tag-column 'face 'elfeed-search-tag-face) " ")
      (insert (propertize title 'face title-faces 'kbd-help title))))

 (setq elfeed-search-print-entry-function #'feed-reader/search-print)

(defun stackoverflow-search ()
"search keyword in google code search and stackoverflow.com"
    (interactive)
    (require 'w3m)
    (let ((keyword (w3m-url-encode-string (read-string "Enter Search Text: "))))
      (execute-chrome-with-args (concat "https://www.google.com/search\\?q=" keyword "+site:stackoverflow.com")))
)

(defun google-search ()
"search word under cursor in google code search and google.com"
    (interactive)
    (require 'w3m)
    (let ((keyword (w3m-url-encode-string (read-string "Enter Search Text: "))))
      (execute-chrome-with-args (concat "https://www.google.com/search\\?q=" keyword "")))
)

(defun github-search ()
"search word under cursor in google code search and google.com"
    (interactive)
    (require 'w3m)
    (let ((keyword (w3m-url-encode-string (read-string "Enter Search Text: "))))
      (execute-chrome-with-args (concat "https://www.google.com/search\\?q=" keyword "+site:github.com")))
)

(defun forge-custom-open-url ()
  (interactive)
  (if-let ((url (forge-get-url (or (forge-post-at-point)
                                   (forge-current-topic)))))
      (progn
        (message "Open Url: %S" url)
        (execute-chrome-with-args url)))
  )

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun er-find-alacritty-init-file ()
  "Edit the shell init file in another window."
  (interactive)
    (find-file-other-window (expand-file-name ".config/alacritty/alacritty.yml" (getenv "HOME"))))

(defun er-find-tmuxconfig-file ()
  "Edit the shell init file in another window."
  (interactive)
    (find-file-other-window (expand-file-name ".tmux.conf" (getenv "HOME"))))

(defun er-find-tmuxinator-file ()
  "Brows tmuxinator session definition"
  (interactive)
  (find-file-other-window "~/.config/tmuxinator")
  )

(defun er-find-shell-init-file ()
  "Edit the shell init file in another window."
  (interactive)
  (let* ((shell (car (reverse (split-string (getenv "SHELL") "/"))))
         (shell-init-file (cond
                           ((string-equal "zsh" shell) ".zshrc")
                           ((string-equal "bash" shell) ".bashrc")
                           (t (error "Unknown shell")))))
    (find-file-other-window (expand-file-name shell-init-file (getenv "HOME")))))
