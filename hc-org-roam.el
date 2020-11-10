;;; hc-org-roam --- Summary

;;; Commentary:

;;;;
;;;; Created       : 2020 Apr 23 (Thu) 14:29:14 by Harold Carr.
;;;; Last Modified : 2020 Apr 24 (Fri) 18:47:07 by Harold Carr.
;;;;

;; https://org-roam.readthedocs.io/en/

;; this is abandoned

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "/Users/carr/org-roam/")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n b" . org-roam-switch-to-buffer)
               ("C-c n g" . org-roam-graph))
         :map org-mode-map
              (("C-c n i" . org-roam-insert))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/org-roam--backlinks-list (file)
  (if (org-roam--org-roam-file-p file)
      (--reduce-from
       (concat acc (format "- [[file:%s][%s]]\n"
                           (file-relative-name (car it) org-roam-directory)
                                 (org-roam--get-title-or-slug (car it))))
       "" (org-roam-db-query [:select [from] :from links :where (= to $s1)] file))
    ""))

(defun my/org-export-preprocessor (backend)
  (let ((links (my/org-roam--backlinks-list (buffer-file-name))))
    (unless (string= links "")
      (save-excursion
        (goto-char (point-max))
        (insert (concat "\n* Backlinks\n") links)))))

(add-hook 'org-export-before-processing-hook 'my/org-export-preprocessor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; playpen

(defun hc-org-roam-graph--dot (node-query)
  (org-roam-db--ensure-built)
  (org-roam--with-temp-buffer
    (let* ((nodes (org-roam-db-query node-query))
           (edges-query
            `[:with selected :as [:select [file] :from ,node-query]
              :select :distinct [to from] :from links
              :where (and (in to selected) (in from selected))])
           (edges-cites-query
            `[:with selected :as [:select [file] :from ,node-query]
              :select :distinct [file from]
              :from links :inner :join refs :on (and (= links:to refs:ref)
                                                     (= links:type "cite"))
              :where (and (in file selected) (in from selected))])
           (edges       (org-roam-db-query edges-query))
           (edges-cites (org-roam-db-query edges-cites-query)))
      (print "nodes")
      (dolist (node nodes) (print node))
      (print "edges")
      (dolist (edge edges) (print edge))
      (print "edges-cites")
      (dolist (edge edges-cites) (print edge)))))

(defun hc-org-roam-graph--build (&optional node-query)
  (unless org-roam-graph-executable
    (user-error "Can't find %s executable.  Please check if it is in your path"
                org-roam-graph-executable))
  (let* ((node-query (or node-query
                         `[:select [file titles]
                           :from titles
                           ,@(org-roam-graph--expand-matcher 'file t)]))
         (graph      (hc-org-roam-graph--dot node-query)))
    graph))

(provide 'hc-org-roam)

;;; hc-org-roam.el ends here

