;;; hc-pdf.el --- summary          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; see ./pdf-tools.org

;; * cause
;; - `pdf-tools-install` is running legacy `nix-shell` code that does:
;;   `import <nixpkgs>`
;; - Determinate Nix often has no channels / no `NIX_PATH`, so that lookup fails. :contentReference[oaicite:0]{index=0}

;; * quickest fix
;; - make `<nixpkgs>` resolvable again

;; edit:

;; #+begin_src sh
;; sudo sh -c 'printf "\nextra-nix-path = nixpkgs=flake:nixpkgs\n" >> /etc/nix/nix.custom.conf'
;; sudo launchctl kickstart -k system/systems.determinate.nix-daemon
;; #+end_src

;; then:
;; - fully quit Emacs
;; - start Emacs again
;; - rerun:
;;   `M-x pdf-tools-install`

;; That `extra-nix-path` fix is a confirmed Determinate-Nix workaround for this exact class of `nix-shell` / `<nixpkgs>` failures. :contentReference[oaicite:1]{index=1}

;; * cleaner Nix-ish alternative
;; - avoid `pdf-tools-install` autobuild entirely
;; - install `pdf-tools` from Nix so `epdfinfo` is already built and on a stable path. A NixOS user reports this working via `emacsWithPackages`, with `pdf-info-epdfinfo-program` pointing into the Nix store. :contentReference[oaicite:2]{index=2}

;; * bottom line
;; - the failure is not pdf-tools itself
;; - it is pdf-tools using old `nix-shell` / `<nixpkgs>` assumptions
;; - add `extra-nix-path`, restart daemon, restart Emacs

;;  activate it
(with-no-warnings
(when (require 'pdf-tools nil :noerror)
  (progn
    (pdf-tools-install)
    ;; THIS WORKS
    (setq pdf-annot-list-format
          '((page     .  3)
            (type     . 10)
            (label    . 24)
            (contents . 56)))
    (global-set-key (kbd "C-c z") 'pdf-view-midnight-minor-mode)
  )
)
)

;; default
;;(setq pdf-annot-list-format
;;  '((page . 3)
;;    (type . 10)
;;    (label . 24)
;;    (date . 24)))

;; what I want but it does not work
;;(setq pdf-annot-list-format
;;  '((page . 3)
;;    (contents . 56)))

(provide 'hc-pdf)
