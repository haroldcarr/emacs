;;; hc-kubernetes.el --- kubernetes          -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:

;; ------------------------------------------------------------------------------
;; KUBERNETES-EL
;; Chris Barrett : https://github.com/chrisbarrett/kubernetes-el

(use-package kubernetes
  :commands (kubernetes-overview))

;; ------------------------------------------------------------------------------
;; KUBEL
;; Adrien Brochard            : https://github.com/abrochard/kubel
;; Kubernetes with Emacs      : https://www.youtube.com/watch?v=w3krYEeqnyk
;; presentation (an org file) : https://gist.github.com/abrochard/dd610fc4673593b7cbce7a0176d897de
;; transient manual           : https://magit.vc/manual/transient.html
;; kubernetes cheatsheet      : https://kubernetes.io/docs/reference/kubectl/cheatsheet/

(use-package kubel)

;;;

(provide 'hc-kubernetes)

;;; hc-kubernetes.el ends here
