(put 'narrow-to-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "ac18cc10455c6c26a98354ba8f9d338842d7ecc9ae3d28c205ed154ef20d74ce" default))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(safe-local-variable-values
   '((org-time-stamp-rounding-minutes 0 15)
     (org-clock-rounding-minutes . 15)
     (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
      (add-hook 'write-contents-functions
                (lambda nil
                  (delete-trailing-whitespace)
                  nil))
      (require 'whitespace)
      "Sometimes the mode needs to be toggled off and on."
      (whitespace-mode 0)
      (whitespace-mode 1))
     (whitespace-line-column . 80)
     (whitespace-style face tabs trailing lines-tail))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(+org-priority-a ((t (:foreground "red3" :weight bold :height 0.95))))
 '(+org-priority-b ((t (:foreground "OrangeRed2" :weight bold))))
 '(+org-priority-c ((t (:foreground "DarkOrange2" :weight bold))))
 '(+org-priority-d ((t (:foreground "gold3" :weight bold))))
 '(+org-priority-e ((t (:foreground "OliveDrab1" :weight bold))))
 '(+org-priority-f ((t (:foreground "SpringGreen3" :weight bold))))
 '(+org-priority-g ((t (:foreground "cyan4" :weight bold))))
 '(+org-priority-h ((t (:foreground "DeepSkyBlue4" :weight bold))))
 '(+org-priority-i ((t (:foreground "LightSteelBlue3" :weight bold))))
 '(adoc-bold-face ((t (:bold t))))
 '(adoc-code-face ((t (:inherit org-block))))
 '(adoc-complex-replacement-face ((t (:inherit org-code :bold t))))
 '(adoc-emphasis-face ((t (:inherit org-verbatim))))
 '(adoc-internal-reference-face ((t (:inherit org-link))))
 '(adoc-meta-face ((t (:inherit org-meta-line))))
 '(adoc-reference-face ((t (:inherit org-link))))
 '(adoc-typewriter-face ((t (:inherit org-code))))
 '(adoc-verbatim-face ((t (:inherit org-verbatim))))
 '(adoc-warning-face ((t (:inherit org-warning))))
 '(magit-branch-current ((t (:foreground "#51afef" :box t))))
 '(notmuch-message-summary-face ((t (:foreground "#848d94"))))
 '(notmuch-search-subject ((t (:foreground "#b1b8c4"))))
 '(org-ql-view-due-date ((t (:foreground "dark goldenrod"))))
 '(ts-fold-replacement-face ((t (:foreground nil :box nil :inherit font-lock-comment-face :weight light)))))
