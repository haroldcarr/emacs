;; -*- lexical-binding: t-*-

;;; package --- generate server and client configuration files; spawn servers; start a client shell.
;;; Commentary:

;; OPERATION
(comment
 ;; in shell
 stack exec genconfs
 ;; evaluate elisp:
 (cp-config)
 (juno-client-shell  10004)
 (spawn-juno-servers 10000 4)
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

(defvar juno-home (concat (getenv "HOME") "/ws/OLABS/juno-orahub"))

(defun juno-client-shell (clientPort)
  (let ((cmd (concat "stack exec junoclient --"
                     " -c  /tmp/" (number-to-string clientPort) "-client.yaml")))
    (pop-to-buffer (get-buffer-create (generate-new-buffer-name "*juno-client*")))
    (cd juno-home)
    (shell (current-buffer))
    (insert cmd)))

(defun jcshell (clientPort)
  "Shorthand: juno-client-shell."
  (interactive)
  (juno-client-shell clientPort))

(defun spawn-juno-servers (startPort numServers)
  (mapc (cl-function
         (lambda ((n port))
           (spawn-juno-server n port)
           (sleep-for 1)))
        (zip `(,(number-sequence 0 (- numServers 1))
               ,(mkStartPorts startPort numServers)))))

(defun sjss (startPort numServers)
  "Shorthand: spawn all servers."
  (interactive)
  (spawn-junoservers startPort numServers))

(defun spawn-juno-server (n port)
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
     "--" "junoserver" "+RTS" "-N4" "-T" "-RTS"
     "-c" (concat "/tmp/" portS "-cluster.yaml")
     "--apiPort"
     apiPort)
    (cd here)))

(defun sjs (n port)
  "Shorthand: spawn a single server N."
  (interactive)
  (spawn-juno-server n port))

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
  (shell-command (concat "cp " juno-home "/conf/clients/* /tmp"))
  (shell-command (concat "cp " juno-home "/conf/servers/* /tmp")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zip (lists)
  "Zip the lists inside LISTS."
  (apply #'cl-mapcar #'list lists))

(provide 'hcJuno)
;;; hcJuno.el ends here
