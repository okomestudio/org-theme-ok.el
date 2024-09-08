;;; org-theme-ok.el --- Org Theme for Okome Studio  -*- lexical-binding: t -*-
;;
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (org "9.7") (org-modern "1.5") (valign "3.1.1"))
;;
;;; Commentary:
;;
;; Currently, only the light theme with minor adjustments for use with
;; `flexoki-themes' is implemented.
;;
;; Other packages to consider:
;;
;;   - `org-margin': not used due to conflicts with `org-indent-mode'
;;
;;; Code:

(require 'ok)
(require 'org)
(require 'org-modern)
(require 'valign)

(defvar oto-font-family-outline "URW Classico"
  "Font for outlines.")

(defvar oto-font-family-outline-ja "Noto Sans CJK JP"
  "Font for outlines in Japanese.")

(defface oto-face-outline '((t :inherit 'default))
  "Face for outlines.
Use when contrast with non-outline contenst is desired."
  :group 'org-theme-ok)

(defvar oto-fixed-pitch-faces
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
    org-modern-todo
    org-property-value
    org-special-keyword
    ;; org-table
    org-verbatim)
  "Fixed-pitch faces in Org.")

(defvar oto-outline-faces
  '((org-level-1 . '(:height 1.24))
    (org-level-2 . '(:height 1.12))
    (org-level-3 . '(:height 1.00))
    (org-level-4 . '(:height 0.90))
    (org-level-5 . '(:height 0.90))
    (org-level-6 . '(:height 0.90))
    (org-level-7 . '(:height 0.90))
    (org-level-8 . '(:height 0.90))
    (org-document-title . '(:height 1.24)))
  "Base outlines faces used in Org.")

(push '("URW Classico" . 1.28) face-font-rescale-alist)

;;; FONTS AND FACES

;;; Install fonts if missing
(let* ((fonts '("URWClassico-Bold.otf"
                "URWClassico-BoldItalic.otf"
                "URWClassico-Italic.otf"
                "URWClassico-Regular.otf"))
       (url-prefix (file-name-concat "https://github.com/okomestudio/"
                                     "fonts-urw-classico/raw/main/opentype/"))
       (result (cl-remove-if #'null
                             (mapcar (lambda (font)
                                       (ok-font-install-from-url
                                        (file-name-concat url-prefix font)))
                                     fonts))))
  (when result
    (ok-font-cache-update)))

(let ((fontset "fontset-urw classic") frame)
  ;; Create fontset that embeds the Japanese subset
  (ok-fontset-create fontset
                     oto-font-family-outline
                     :subsets `((ja ,oto-font-family-outline-ja))
                     :frame frame)
  (set-face-attribute 'oto-face-outline frame :font fontset :fontset fontset)

  ;; Apply outline face properties
  (dolist (it oto-outline-faces)
    (let* ((face (car it))
           (prop (cdr it)))
      (set-face-attribute
       face frame
       :height (or (cadr (assoc :height prop))
                   (face-attribute face :height nil t))
       :foreground (face-attribute face :foreground nil t)
       :weight (face-attribute face :weight nil t)
       :inherit 'oto-face-outline)))

  ;; Make drawer a little less prominent
  (set-face-attribute 'org-drawer frame
                      :foreground (face-attribute 'shadow :foreground))

  ;; Do not use bold face for links
  (set-face-attribute 'org-link frame :weight 'normal))

(defun oto--remap-to-mixed-pitch ()
  (face-remap-add-relative 'default :inherit 'variable-pitch)
  (dolist (face oto-fixed-pitch-faces)
    (face-remap-add-relative face :inherit 'fixed-pitch)))

(defun oto--handle-text-scale-mode ()
  (let ((height (ok-face-text-scale-mode-height)))
    (when height
      (dolist (face (append oto-fixed-pitch-faces
                            oto-outline-faces))
        (if (consp face)
            (setq face (car face)))
        (face-remap-add-relative face :height height)))))

(add-hook 'org-mode-hook #'oto--remap-to-mixed-pitch 91)
(add-hook 'org-mode-hook #'oto--handle-text-scale-mode 92)
;; (add-hook 'org-mode-hook #'foce-window-update 93)

;; Do not use bold face for cite refs
;; (set-face-attribute 'org-ref-cite-face nil :weight 'normal)

(set-face-attribute 'org-modern-todo nil :weight 'bold)

;;; PER-MODE CONFIG

(defun oto--ad-apply-only-in-current-buffer-window (func &rest rest)
  "Invoke FUNC only when current buffer window is visible."
  (when (get-buffer-window (current-buffer) t)
    (apply func rest)))

;;; `org-indent'
(add-hook 'org-mode-hook (lambda () (org-indent-mode 1)))
(advice-add #'org-indent-refresh-maybe
            :around #'oto--ad-apply-only-in-current-buffer-window)

;;; `org-modern'
(setopt org-modern-block-name t  ; use `org-modern-indent'
        org-modern-checkbox '((?X . #("▢𐄂" 0 2 (composition ((2)))))
                              (?- . #("▢–" 0 2 (composition ((2)))))
                              (?\s . #("▢" 0 1 (composition ((1))))))
        org-modern-hide-stars nil
        org-modern-keyword "‣ "
        org-modern-list '((?+ . "▷")
                          (?- . "𑁋")  ; "–"
                          (?* . "▶"))
        org-modern-priority t
        org-modern-replace-stars "◉🞛○▷"
        org-modern-star 'replace
        org-modern-statistics t
        org-modern-table nil
        org-modern-tag t
        org-modern-timestamp t
        org-modern-todo t)
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

;;; `org-modern-indent'
(defun org-modern-indent-ok--fix-top-level-indent ()
  "Activate org-modern-indent on the top-level file node.
See github.com/jdtsmith/org-modern-indent/issues/10."
  (if org-indent--text-line-prefixes
      (aset org-indent--text-line-prefixes
            0 (propertize " " 'face 'org-indent))))

(add-hook 'org-indent-mode-hook #'org-modern-indent-ok--fix-top-level-indent)
(add-hook 'org-mode-hook #'org-modern-indent-mode 90)
(advice-add #'org-modern-indent--refresh-watch
            :around #'oto--ad-apply-only-in-current-buffer-window)

;;; `valign'
(setopt valign-fancy-bar t
        valign-max-table-size 4000
        valign-signal-parse-error t)

(defvar valign-ok--max-buffer-size 100000
  "Default max-buffer-size above which `valign-mode' will not activate.")

(defun valign-ok--maybe-activate ()
  (when (<= (buffer-size) valign-ok--max-buffer-size)
    (valign-mode 1)))

(add-hook 'org-mode-hook #'valign-ok--maybe-activate)

(provide 'org-theme-ok)

;; Local Variables:
;; read-symbol-shorthands: (("oto" . "org-theme-ok"))
;; End:
;;; org-theme-ok.el ends here
