;;; hc-firewall.el --- firewall          -*- lexical-binding: t; -*-

;;; Commentary:

(comment
 "
 ;; https://github.com/mrmekon/snitch-el
 cd `hcLocation emacs`
 unzip ~/Downloads/snitch-el-master.zip
 "
)
;;; Code:

(add-to-list 'load-path (concat (hcEmacsDir) "/snitch-el-master"))
(require 'snitch)
(snitch-mode +1)

(provide 'hc-firewall)

;;; hc-firewall.el ends here
