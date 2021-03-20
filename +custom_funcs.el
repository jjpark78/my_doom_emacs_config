(defun custom-cc-mode ()
  "Custom cc-mode make support qml, qmake etc."
  (interactive)
  (platformio-conditionally-enable)
  (setq lsp-prefer-flymake nil
        ccls-executable "/usr/local/bin/ccls"
        lsp-ui-peek-fontify 'always
        lsp-ui-doc-include-signature nil  ; don't include type signature in the child fram
        lsp-ui-sideline-show-symbol nil)  ; don't show symbol on the right of info
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc)))

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
  (setq lsp-ui-peek-fontify 'always)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  ;; (add-hook 'before-save-hook prettier-js nil 'local)
  ;; (prettier-js-mode)
  )

(defun custom-ts-mode ()
  (if (not (equal buffer-file-name 'nil))
      (let ((extname (file-name-extension buffer-file-name)))
        (when (or (string-equal "tsx" extname)
                  (string-equal "ts" extname))
          (setup-custom-jsts-mode)
          ;; (set-company-backend! 'prog-mode '(company-tabnine company-capf company-yasnippet))
          ;; Optional configuration that hides the background color for a highlighted block
          ;; I find it useful for debugging emacs, but when actually coding I dont want so much emphasis on submodes
          (flycheck-select-checker 'javascript-eslint)))))

(defun my/use-eslint-from-node-modules ()
  "Use local eslint from node_modules before global."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(defun custom-vue-mode ()
  "Custom hooks for vue-mode"
  (if (not (equal buffer-file-name 'nil))
      (let ((extname (file-name-extension buffer-file-name)))
        (when (string-equal "vue" extname)
          (setup-custom-jsts-mode)
          ;; (set-company-backend! 'prog-mode '(company-tabnine company-capf company-yasnippet))
          (flycheck-select-checker 'javascript-eslint)
          ))))

(defun my-org-config/after-org-mode-load ()
  ;; (visual-line-mode)
  (require 'org-indent)
  (org-indent-mode)
  )

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
      (browse-url (concat "http://www.google.com/search?hl=en&q=" keyword "+site:stackoverflow.com")))
)

(defun google-search ()
"search word under cursor in google code search and google.com"
    (interactive)
    (require 'w3m)
    (let ((keyword (w3m-url-encode-string (read-string "Enter Search Text: "))))
      (browse-url (concat "http://www.google.com/search?hl=en&q=" keyword )))
)

(defun forge-custom-open-url ()
  (interactive)
  (if-let ((url (forge-get-url (or (forge-post-at-point)
                                   (forge-current-topic)))))
      (progn
        (message "Open Url: %S" url)
        (browse-url-generic url)))
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

(defun er-find-shell-init-file ()
  "Edit the shell init file in another window."
  (interactive)
  (let* ((shell (car (reverse (split-string (getenv "SHELL") "/"))))
         (shell-init-file (cond
                           ((string-equal "zsh" shell) ".zshrc")
                           ((string-equal "bash" shell) ".bashrc")
                           (t (error "Unknown shell")))))
    (find-file-other-window (expand-file-name shell-init-file (getenv "HOME")))))
