;; -*- lexical-binding: t-*-

;;; package --- generate server and client configuration files; spawn servers; start a client shell.
;;; Commentary:

;; OPERATION
(comment
 ;; in shell
 stack exec genconfs
 (load-library "hcJuno.el")
 ;; evaluate elisp:
 (cp-config)
 (setq ledger-type "juno")
 (setq ledger-type "altledger")
 (juno-client-shell  ledger-type 10005)
 (spawn-juno-servers ledger-type 10000 5)
 ;; in client shell:
 0: CreateAccount foo
 0: CreateAccount bar
 0: AdjustAccount foo (1%1)
 0: transfer(foo->bar,101%100)
 ;;(#transfer "foo" "bar" (% 110 100) "baz")
 0: ObserveAccounts
 0: ObserveAccount foo
 ;; evaluate elisp:
 (kill-all-juno-servers)
 ;; manually kill juno client shell
)

;; When running emacs in Docker and sharing host/container directories, need this so emacs can exit.
;; (setq ido-save-directory-list-file "~/ido-last.JUNK")

;; TODO:
;; - `server-public-private-keys` is a list of public/private keys to be used by juno servers.
;;   It only contains 4 pairs.
;;   Add more pairs to generate configuration for more than 4 servers.
;;
;; - Factor of templates.
;;
;; - More parameterization of generation/templates.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spawn servers and a client shell.

(defvar ledger-type "altledger")

(defvar juno-home (concat (getenv "HOME") "/ws/OLABS/juno"))
;;(defvar juno-home (concat (getenv "HOME") "/.sync/.esync/openhc/pept/z-juno"))

(defun juno-client-shell (ltype clientPort)
  (let ((cmd (concat "stack exec " ltype "client --"
                     " -c  /tmp/" (number-to-string clientPort) "-client.yaml")))
    (pop-to-buffer (get-buffer-create (generate-new-buffer-name "*juno-client*")))
    (cd juno-home)
    (shell (current-buffer))
    (insert cmd)))

(defun spawn-juno-servers (ltype startPort numServers)
  (mapc (cl-function
         (lambda ((n port))
           (spawn-juno-server ltype n port)
           (sleep-for 1)))
        (zip `(,(number-sequence 0 (- numServers 1))
               ,(mkStartPorts startPort numServers)))))

(defun spawn-juno-server (ltype n port)
  "Spawn a single server N."
  (let* ((here    default-directory)
         (nS      (number-to-string n))
         (portS   (number-to-string port))
         (apiPort (concat "800" nS))
         (pname   (concat "*" apiPort "*")))
    (cd juno-home)
    (start-process
     pname pname
     "stack"
     "exec"
     "--" (concat ltype "server") "+RTS" "-N4" "-T" "-RTS"
     "-c" (concat "/tmp/" portS "-cluster.yaml")
     "--apiPort"
     apiPort)
    (cd here)))

;; For debug/development : replace start-process above with this.
(cl-defun my-start-process (&rest all) (message (format "ALL: %s" all)))

(defun kill-all-juno-servers ()
  "Kill all buffers and processes with names that start with '*800'."
  (mapc #'(lambda (p)
            (let ((pname (process-name p)))
              (message pname)
              (cond ((cl-equalp "*800" (substring pname 0 4))
                     (message (concat "killing " pname))
                     (kill-buffer pname)
                     (kill-process p)))))
        (process-list)))

(defun kajs ()
  "Shorthand: kill-all-junoservers."
  (interactive)
  (kill-all-juno-servers))

(defun mkStartPorts (startPort numServers)
  (mapcar #'(lambda (x) (+ x startPort))
          (number-sequence 0 (- numServers 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate config files.

(defun cp-config ()
  (shell-command (concat "cp " juno-home "/conf/*.yaml /tmp"))
  (shell-command (concat "cp " juno-home "/conf/clients/* /tmp"))
  (shell-command (concat "cp " juno-home "/conf/servers/* /tmp")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zip (lists)
  "Zip the lists inside LISTS."
  (apply #'cl-mapcar #'list lists))

(provide 'hcJuno)
;;; hcJuno.el ends here
