# org-theme-ok

A custom "theme" for Org.

### Installation

``` emacs-lisp
(use-package org-theme-ok
  :straight (:host github :repo "okomestudio/org-theme-ok")
  :demand t
  :init
  (use-package ok
    :straight (:host github :repo "okomestudio/ok.el"))
  (use-package org-modern-indent
    :straight (:host github :repo "jdtsmith/org-modern-indent")))
```
