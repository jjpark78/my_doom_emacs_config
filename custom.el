
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7b3d184d2955990e4df1162aeff6bfb4e1c3e822368f0359e15e2974235d9fa8" default))
 '(package-selected-packages '(wakatime-mode swiper))
 '(wakatime-api-key "c6ba049d-8360-45fd-8317-a4b25d0ab860")
 '(warning-suppress-types '((initialization) (lsp-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
