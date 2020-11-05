(defun save-framegeometry ()
  "Gets the current frame's geometry and saves to ~/.emacs.d/framegeometry."
  (let (
        (framegeometry-left (frame-parameter (selected-frame) 'left))
        (framegeometry-top (frame-parameter (selected-frame) 'top))
        (framegeometry-width (frame-parameter (selected-frame) 'width))
        (framegeometry-height (frame-parameter (selected-frame) 'height))
        (framegeometry-file (expand-file-name "~/.emacs.d/framegeometry"))
        )
    (when (not (number-or-marker-p framegeometry-left))
      (setq framegeometry-left 0))
    (when (not (number-or-marker-p framegeometry-top))
      (setq framegeometry-top 0))
    (when (not (number-or-marker-p framegeometry-width))
      (setq framegeometry-width 0))
    (when (not (number-or-marker-p framegeometry-height))
      (setq framegeometry-height 0))
    (with-temp-buffer
      (insert
       ";;; This is the previous emacs frame's geometry.\n"
       ";;; Last generated " (current-time-string) ".\n"
       "(setq initial-frame-alist\n"
       "      '(\n"
       (format "        (top . %d)\n" (max framegeometry-top 0))
       (format "        (left . %d)\n" (max framegeometry-left 0))
       (format "        (width . %d)\n" (max framegeometry-width 0))
       (format "        (height . %d)))\n" (max framegeometry-height 0)))
       (when (file-writable-p framegeometry-file)
       (write-file framegeometry-file))))
)

(defun load-framegeometry ()
  (let ((framegeometry-file (expand-file-name "~/.emacs.d/framegeometry")))
    (when (file-readable-p framegeometry-file)
      (load-file framegeometry-file)))
)

(defun setup-custom-prog-mode ()
  ;; 기본 인덴테이션을 설정한다.
  (setq typescript-indent-level 2)
  (setq emmet-indentation 2)
  (setq js-indent-level 2)
  ;; (setq global-git-gutter-mode t)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (flycheck-mode +1)
  ;; (fringe-mode '(20 . 20))
  (my/use-eslint-from-node-modules)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq lsp-ui-peek-fontify 'always)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (add-hook 'before-save-hook #'prettier-js nil 'local))

(defun custom-ts-mode ()
  "Custom hooks for ts-mode"
  (if (not (equal buffer-file-name 'nil))
      (let ((extname (file-name-extension buffer-file-name)))
        (when (or (string-equal "tsx" extname)
                  (string-equal "ts" extname))
          (setup-custom-prog-mode)
          ;; (set-company-backend! 'prog-mode '(company-tabnine company-capf company-yasnippet))
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
          (setup-custom-prog-mode)
          ;; (set-company-backend! 'prog-mode '(company-tabnine company-capf company-yasnippet))
          (flycheck-select-checker 'javascript-eslint)
          ))))

(defun my-org-config/after-org-mode-load ()
  (visual-line-mode)
  (require 'org-indent)
  (org-indent-mode)
  (setq org-fontify-quote-and-verse-blocks nil
        org-fontify-whole-heading-line t
        org-hide-leading-stars nil
        org-startup-indented nil)
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
      (xwidget-webkit-browse-url (concat "http://www.google.com/search?hl=en&q=" keyword "+site:stackoverflow.com")))
)

(defun google-search ()
"search word under cursor in google code search and google.com"
    (interactive)
    (require 'w3m)
    (let ((keyword (w3m-url-encode-string (read-string "Enter Search Text: "))))
      (xwidget-webkit-browse-url (concat "http://www.google.com/search?hl=en&q=" keyword )))
)

(defun custom-input-method-hook ()
  (when (equal nil (string-equal nil evil-input-method))
      (setq evil-input-method nil)
      )
  )

(defvar my-input-list '("korean-hangul"
                        "japanese"
                        "pyim"))

(defun choose-input-from-rotated-list ()
  "change input method"
  (interactive)
  (setq my-input-list (append (cdr my-input-list) (cons (car my-input-list) ())))
  (message (car my-input-list))
  (set-input-method (car my-input-list))
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

(defun er-find-shell-init-file ()
  "Edit the shell init file in another window."
  (interactive)
  (let* ((shell (car (reverse (split-string (getenv "SHELL") "/"))))
         (shell-init-file (cond
                           ((string-equal "zsh" shell) ".zshrc")
                           ((string-equal "bash" shell) ".bashrc")
                           (t (error "Unknown shell")))))
    (find-file-other-window (expand-file-name shell-init-file (getenv "HOME")))))
