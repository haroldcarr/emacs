;;; hc-tags.el --- git          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; - http://www.emacswiki.org/emacs/BuildTags
;; - http://www.gnu.org/software/emacs/manual/html_node/emacs/Tags.html#Tags
;; - http://emacswiki.org/emacs/EmacsTags

;; alternate way to create using etags
;; cd <...>
;; need a regex instead of "*" - also only files
;; find . -name "*" -print -o -name SCCS -name RCS -prune | .../bin/etags -

(defun hcTagsCreate (dir-name &optional tags-dir-path-filename)
  "DIR-NAME TAGS-DIR-PATH-FILENAME: Create tags file."
  (interactive "DDirectory: ")
  ;; ctags via nix
  (let* ((dir (directory-file-name dir-name))
         (ctags-filename (if (null tags-dir-path-filename) (concat dir "/TAGS") tags-dir-path-filename)))
    (shell-command
     (format "ctags -f %s -e -R %s" ctags-filename dir))))

(defun        hcTagsDir       (x)"X."(concat (hcEsync)        "/TAGS/" x))
(hcDefineBean hcTagsCatalogSrc       (concat (hcWs)           "/catalog-service/subprojects/catalog-core/src/"))
(hcDefineBean hcTagsCatalogDst       (hcTagsDir               "TAGS-CATALOG"))
(hcDefineBean hcTagsJavaSrc          "/Library/Java/JavaVirtualMachines/jdk1.8.0_45.jdk/Contents/Home/src")
(hcDefineBean hcTagsJavaDst          (hcTagsDir               "TAGS-JAVA"))
(hcDefineBean hcTagsBuzzSrc          (concat (hcWs)           "/buzz-message-bus/src"))
(hcDefineBean hcTagsBuzzDst          (hcTagsDir               "TAGS-BUZZ"))
(hcDefineBean hcTagsMessageBusSrc    (concat (hcM2Repository) "/com/oracle/commons/fmw-commons/12.1.4-0-0-SNAPSHOT/sources/src"))
(hcDefineBean hcTagsMessageBusDst    (hcTagsDir               "TAGS-MESSAGE-BUS"))
(defun hcTagsCreateCatalog    ()"."(interactive) (hcTagsCreate (hcTagsCatalogSrc)    (hcTagsCatalogDst)))
(defun hcTagsCreateJava       ()"."(interactive) (hcTagsCreate (hcTagsJavaSrc)       (hcTagsJavaDst)))
(defun hcTagsCreateBuzz       ()"."(interactive) (hcTagsCreate (hcTagsBuzzSrc)       (hcTagsBuzzDst)))
(defun hcTagsCreateMessageBus ()"."(interactive) (hcTagsCreate (hcTagsMessageBusSrc) (hcTagsMessageBusDst)))
(defun hcTagsCreateAll () "."
  (interactive)
  (hcTagsCreateCatalog)
  (hcTagsCreateJava)
  (hcTagsCreateBuzz)
  (hcTagsCreateMessageBus))
(defun hcVtc ()"."(interactive) (visit-tags-table (hcTagsCatalogDst)))
(defun hcVtj ()"."(interactive) (visit-tags-table (hcTagsJavaDst)))
(defun hcVtb ()"."(interactive) (visit-tags-table (hcTagsBuzzDst)))
(defun hcVtm ()"."(interactive) (visit-tags-table (hcTagsMessageBusDst)))

(provide 'hc-tags)

;;; hc-tags.el ends here
