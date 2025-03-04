# org-theme-ok

A custom "theme" for Org.

### Installation

``` emacs-lisp
(use-package ok
  :straight (ok :host github :repo "okomestudio/ok.el"))

(use-package org-modern-indent
  :straight (org-modern-indent :host github :repo "jdtsmith/org-modern-indent"))

(use-package org-theme-ok
  :straight (org-theme-ok :host github :repo "okomestudio/org-theme-ok")
  :hook (org-mode . org-theme-ok-mode))
```
