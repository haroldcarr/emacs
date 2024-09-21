;;; hc-projectile --- init file         -*- mode: emacs-lisp -*-

;;; Commentary:

;;; Code:

(eval-when-compile (require 'use-package))

(require 'tramp)

(use-package projectile
  :demand
  :diminish ""
  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))
  (setq projectile-create-missing-test-files t
        projectile-mode-line                 nil
        projectile-use-git-grep              t)
  (make-variable-buffer-local 'projectile-tags-command)
  (projectile-mode)
  :config
  ;; HACK to get haskell-cabal near the beginning so it is discovered
  ;; before nix, Make, stack, etc.
  ;; TODO : make this deterministic.
  (setq projectile-project-types (reverse projectile-project-types))
  ;; The version built-in to projectile does not specify :test-dir nor :test-suffix
  (projectile-register-project-type 'rust-cargo '("Cargo.toml")
                                    :project-file "Cargo.toml"
                                    :compile "cargo build"
                                    :test "cargo test"
                                    :run "cargo run"
                                    :test-dir "tests"
                                    :test-suffix "-test")

)


(provide 'hc-projectile)

;;; hc-projectile.el ends here
