;;; org-theme-ok.el --- Org Theme for Okome Studio  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024-2025 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-theme-ok.el
;; Version: 0.2.1
;; Package-Requires: ((emacs "29.1") (ok "0.2.1") (org "9.7") (org-modern "1.5") (org-modern-indent "0.1.4") (valign "3.1.1"))
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
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
(require 'org-modern-indent)
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
  '((org-level-1 . '(:height 1.4))
    (org-level-2 . '(:height 1.2))
    (org-level-3 . '(:height 1.0))
    (org-level-4 . '(:height 1.0))
    (org-level-5 . '(:height 1.0))
    (org-level-6 . '(:height 1.0))
    (org-level-7 . '(:height 1.0))
    (org-level-8 . '(:height 1.0))
    (org-document-title . '(:height 1.6)))
  "Base outlines faces used in Org.")

;;; FONT RESCALE

(defun oto--fonts-rescale-add ()
  "Add fonts to rescale alist."
  (push '("URW Classico" . 1.28) face-font-rescale-alist))

(defun oto--fonts-rescale-remove ()
  "Remove fonts from rescale alist."
  (push '("URW Classico" . nil) face-font-rescale-alist))

;;; FONTS AND FACES

;; Install missing fonts.
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
  (set-face-attribute 'oto-face-outline frame
                      :family oto-font-family-outline
                      ;; :font fontset
                      :fontset fontset))

;; Apply outline face properties. This enables different faces (e.g.,
;; serif vs. sans serif) for the body text and the outline text.
(dolist (it oto-outline-faces)
  (let ((face (car it))
        (prop (cdr it)))
    (set-face-attribute face nil
                        :height (or (cadr (assoc :height prop))
                                    (face-attribute face :height nil t))
                        :foreground (face-attribute face :foreground nil t)
                        :weight (face-attribute face :weight nil t)
                        :inherit 'oto-face-outline)))

(defun oto--adjust-faces (theme)
  "Adjust font and faces for the new THEME.
The properties of the faces defined by less popular packages aren't
properly adjusted on theme change due to lack of handler code. Do it
here."
  (set-face-attribute 'org-hide nil
                      :foreground (face-attribute 'default :background)
                      :background (face-attribute 'default :background))

  ;; Make drawer a little less prominent.
  (set-face-attribute 'org-drawer nil
                      :foreground (face-attribute 'shadow :foreground))

  ;; Do not use bold face for links.
  (set-face-attribute 'link nil :weight 'unspecified)

  ;; Do not use bold face for cite refs.
  ;; (set-face-attribute 'org-ref-cite-face nil :weight 'normal)

  ;; Make TODO face prominent.
  (set-face-attribute 'org-modern-todo nil :weight 'bold)

  ;; Ensure tags are visible.
  (set-face-attribute 'org-modern-tag nil
                      :foreground (face-attribute 'default :foreground)))

(add-hook 'enable-theme-functions #'oto--adjust-faces)

;;; PER-MODE CONFIG

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

(defun oto--run-only-when-visible (fun &rest _)
  "Run FUN only if current buffer window is visible.
This is an advice to reduce unnecessary rendering."
  (when (get-buffer-window (current-buffer) t)
    (apply fun _)))

;;; `org-indent'

(advice-add #'org-indent-refresh-maybe :around #'oto--run-only-when-visible)

;;; `org-modern'

(setopt org-modern-block-name t         ; use `org-modern-indent'
        org-modern-checkbox '((?X . #("▢𐄂" 0 2 (composition ((2)))))
                              (?- . #("▢–" 0 2 (composition ((2)))))
                              (?\s . #("▢" 0 1 (composition ((1))))))
        org-modern-hide-stars nil
        org-modern-keyword "‣ "
        org-modern-list '((?+ . "▷")
                          (?- . "𑁋")    ; "–"
                          (?* . "▶"))
        org-modern-priority t
        org-modern-replace-stars "◉🞛○▷"
        org-modern-star 'replace
        org-modern-statistics t
        org-modern-table nil
        org-modern-tag t
        org-modern-timestamp t
        org-modern-todo t)

;;; `org-modern-indent'

(defun org-modern-indent-ok--fix-top-level-indent ()
  "Activate org-modern-indent on the top-level file node.
See github.com/jdtsmith/org-modern-indent/issues/10."
  (if org-indent--text-line-prefixes
      (aset org-indent--text-line-prefixes
            0 (propertize " " 'face 'org-indent))))

(add-hook 'org-indent-mode-hook #'org-modern-indent-ok--fix-top-level-indent)
(advice-add #'org-modern-indent--refresh-watch :around
            #'oto--run-only-when-visible)

;;; `valign'

(setopt valign-fancy-bar t
        valign-max-table-size 4000
        valign-signal-parse-error t)

(defvar valign-ok--max-buffer-size 100000
  "Default max-buffer-size above which `valign-mode' will not activate.")

(defun valign-ok--maybe-activate ()
  "Activate `valign' only if buffer is small enough."
  (when (<= (buffer-size) valign-ok--max-buffer-size)
    (valign-mode 1)))

;;; MODE HOOK

(defun oto--activate ()
  "Activate `org-theme-ok-mode'.
When creating a minor mode, this would be the mode activator function."
  (org-indent-mode 1)
  (org-modern-mode 1)
  (org-modern-indent-mode 1)
  (oto--remap-to-mixed-pitch)
  ;; (oto--handle-text-scale-mode)
  (valign-ok--maybe-activate)
  (oto--fonts-rescale-add)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

(defun oto--deactivate ()
  "Deactivate `org-theme-ok-mode'."
  (remove-hook 'org-agenda-finalize-hook #'org-modern-agenda)
  (oto--fonts-rescale-remove)
  (valign-mode -1)
  (org-modern-indent-mode -1)
  (org-modern-mode -1)
  (org-indent-mode -1))

;;;###autoload
(define-minor-mode org-theme-ok-mode
  "A minor mode for Okome Studio Org theme."
  :group 'org
  :lighter "org-them-ok-mode"
  :keymap nil
  (if org-theme-ok-mode (oto--activate) (oto--deactivate)))

(provide 'org-theme-ok)

;; Local Variables:
;; read-symbol-shorthands: (("oto" . "org-theme-ok"))
;; End:
;;; org-theme-ok.el ends here
