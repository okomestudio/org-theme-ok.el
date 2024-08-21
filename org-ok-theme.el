;;; org-ok-theme.el --- Okome Studio Theme for Org Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ok)

(defvar org-ok-theme-font-family-outline "URW Classico"
  "Font for outlines.")

(defvar org-ok-theme-font-family-outline-ja "Noto Sans CJK JP"
  "Font for outlines in Japanese.")

(defface org-ok-theme-face-outline '((t :inherit 'default))
  "Face for outlines.
Use when contrast with non-outline contenst is desired."
  :group 'org-ok-theme)

(defvar org-ok-theme-fixed-pitch-faces
  '(font-lock-builtin-face
    font-lock-comment-delimiter-face
    font-lock-comment-face
    font-lock-constant-face
    font-lock-doc-face
    font-lock-function-name-face
    font-lock-keyword-face
    font-lock-negation-char-face
    font-lock-preprocessor-face
    font-lock-regexp-grouping-backslash
    font-lock-regexp-grouping-construct
    font-lock-string-face
    font-lock-type-face
    font-lock-variable-name-face
    org-block
    org-block-begin-line
    org-block-end-line
    org-checkbox
    org-code
    org-document-info
    org-document-info-keyword
    org-drawer
    org-formula
    org-indent
    org-latex-and-related
    org-meta-line
    org-modern-bracket-line
    org-modern-tag
    org-property-value
    org-special-keyword
    ;; org-table
    org-verbatim)
  "Fixed-pitch faces in Org mode.")

(defvar org-ok-theme-outline-faces
  '((org-level-1 . '(:height 1.24))
    (org-level-2 . '(:height 1.12))
    (org-level-3 . '(:height 1.00))
    (org-level-4 . '(:height 0.90))
    (org-level-5 . '(:height 0.90))
    (org-level-6 . '(:height 0.90))
    (org-level-7 . '(:height 0.90))
    (org-level-8 . '(:height 0.90))
    (org-document-title . '(:height 1.24)))
  "Base outlines faces used in Org mode.")

(push '("URW Classico" . 1.28) face-font-rescale-alist)

(with-eval-after-load 'org-faces
  ;;   :ensure-system-package
  ;;   ("~/.local/share/fonts/u/URWClassico-Regular.otf"
  ;;    . "~/.config/emacs/bin/install-font \
  ;; 'https://github.com/okomestudio/fonts-urw-classico/raw/main/opentype/URWClassico-Bold.otf' \
  ;; 'https://github.com/okomestudio/fonts-urw-classico/raw/main/opentype/URWClassico-BoldItalic.otf' \
  ;; 'https://github.com/okomestudio/fonts-urw-classico/raw/main/opentype/URWClassico-Italic.otf' \
  ;; 'https://github.com/okomestudio/fonts-urw-classico/raw/main/opentype/URWClassico-Regular.otf'")

  (let ((fontset "fontset-urw classic")
        frame)
    ;; Create fontset.
    (ok-face-fontset-create fontset
                            org-ok-theme-font-family-outline
                            :subsets `((ja ,org-ok-theme-font-family-outline-ja))
                            :frame frame)
    (set-face-attribute 'org-ok-theme-face-outline frame :font fontset :fontset fontset)

    ;; Set faces.
    (dolist (it org-ok-theme-outline-faces)
      (let* ((face (car it))
             (prop (cdr it))
             (height (or (car (cdr (assoc :height prop)))
                         (face-attribute face :height nil t)))
             (foreground (face-attribute face :foreground nil t))
             (weight (face-attribute face :weight nil t))
             (inherit 'org-ok-theme-face-outline))
        (set-face-attribute face frame
                            :height height
                            :foreground foreground
                            :weight weight
                            :inherit inherit)))

    (set-face-attribute 'org-drawer frame
                        :foreground (face-attribute 'shadow :foreground))

    (set-face-attribute 'org-link frame :weight 'normal)

    (with-eval-after-load 'org-ref
      (set-face-attribute 'org-ref-cite-face nil :weight 'normal)))

  (defun org-ok-theme--remap-to-mixed-pitch ()
    (face-remap-add-relative 'default :inherit 'variable-pitch)
    (dolist (face org-ok-theme-fixed-pitch-faces)
      (face-remap-add-relative face :inherit 'fixed-pitch)))

  (defun org-ok-theme--handle-text-scale-mode ()
    (let ((height (ok-face-text-scale-mode-height)))
      (when height
        (dolist (face (append org-ok-theme-fixed-pitch-faces
                              org-ok-theme-outline-faces))
          (if (consp face)
              (setq face (car face)))
          (face-remap-add-relative face :height height)))))

  (add-hook 'org-mode-hook #'org-ok-theme--remap-to-mixed-pitch 91)
  (add-hook 'org-mode-hook #'org-ok-theme--handle-text-scale-mode 92)
  ;; (add-hook 'org-mode-hook #'foce-window-update 93)
  )

(provide 'org-ok-theme)
;;; org-ok-theme.el ends here
