;;;;
;;;; Created       : 2010 Dec ...               by Harold Carr.
;;;; Last Modified : 2011 Apr 03 (Sun) 21:09:15 by carr.
;;;;

"Connect to a directory that has a pom file that references my
Clojure and/or Scala base poms and do meta-x cpom (or spom) and
you will have an interactive REPL for either Clojure or Scala
with the appropriate classpath added for the project from that
project's pom."

(require 'hcRunCommand)

(hcRunCommand cpom hcCPomCmd)
(hcRunCommand spom hcSPomCmd)

(defun hcCPomCmd ()
  (hcPomCmd "clojure:repl"))

(defun hcSPomCmd ()
  (hcPomCmd "scala:console"))

(defun hcPomCmd (x)
  (concat "hcMaven -o " x))

(provide 'hcPomCommand)
