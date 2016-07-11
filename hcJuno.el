;; -*- lexical-binding: t-*-

;;; package --- Summary
;;; Commentary:

;; TODO:
;; - `server-public-private-keys` is a list of public/private keys to be used by juno servers.
;;   It only contains 4 pairs.
;;   Add more pairs to generate configuration for more than 4 servers.

;; OPERATION
;; (generate-config    20001 4)
;; (spawn-juno-servers 20001 4)
;; (juno-client-shell  20005)
(comment
 CreateAccount foo
 CreateAccount bar
 AdjustAccount foo (1%1)
 transfer(foo->bar,101%100)
 ;;(#transfer "foo" "bar" (% 110 100) "baz")
 ObserveAccounts
 ObserveAccount foo
)
;; (kill-all-juno-servers)
;; manually kill juno client shell

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spawn servers and a client shell.

(defvar juno-home (concat (getenv "HOME") "/ws/juno-orahub"))

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
  "Spawn all servers."
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate config files.

(defun generate-config (startPort numServers)
  (mapc (cl-function
         (lambda (((serverPort serverConf)
                   (clientPort clientConf)))
           (with-temp-file
               (concat "/tmp/" serverPort "-cluster.yaml")
             (insert serverConf))
           ;; TODO: this creates and (overwrites) the client conf multiple times
           (with-temp-file
               (concat "/tmp/" clientPort "-client.yaml")
             (insert clientConf))))
        (gc1 startPort numServers)))

(defun gc (startPort numServers)
  "Shorthand: generate-config."
  (generate-config startPort numServers))

(defun gc1 (startPort numServers)
  (let* ((portPubKeyPairs
          (zip `(,(mapcar #'number-to-string (mkStartPorts startPort numServers))
                 ,server-public-private-keys)))
         (clientPort       (number-to-string (+ startPort numServers)))
         (clientPubKey     (1st client-public-private-keys))
         (clientPrivateKey (2nd client-public-private-keys))
         (gcf #'(lambda (a s c u) (gc2 clientPort clientPubKey clientPrivateKey a s c u))))
    (mapT gcf portPubKeyPairs)))

(defun gc2 (clientPort clientPubKey clientPrivateKey all s current u)
  "ALL S CURRENT U."
  (let* ((allPortPubKeys (mapcar (cl-function (lambda ((port (pubKey priKey-ignore)))
                                                `(,port ,pubKey)))
                                 all))
         (others (mapcar #'1st (append s u)))
         (myPublicKey (1st (2nd current)))
         (myPort (1st current))
         (myPrivateKey (2nd (2nd current))))
    `((,myPort ,(server-config-template
                 clientPort clientPubKey
                 allPortPubKeys
                 others
                 myPublicKey myPort myPrivateKey))
      (,clientPort ,(client-config-template
                     clientPort clientPubKey clientPrivateKey
                     allPortPubKeys)))
    ))

(defun mkStartPorts (startPort numServers)
  (mapcar #'(lambda (x) (+ x startPort))
          (number-sequence 0 (- numServers 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun node-id-template (port)
  (concat
"  fullAddr: tcp://127.0.0.1:"port"
  host: '127.0.0.1'
  port: "port"
"))

(defun address-template (ports)
  (apply #'concat
         (mapcar #'(lambda (port) (concat
"- fullAddr: tcp://127.0.0.1:"port"
  host: '127.0.0.1'
  port: "port"
"))
                 ports)))

(defun public-keys-template (portKeyPairs)
  (apply #'concat
         (mapcar (cl-function (lambda ((port key)) (concat
"- - fullAddr: tcp://127.0.0.1:"port"
    host: '127.0.0.1'
    port: "port"
  - "key"
")))
                 portKeyPairs)))

(defun server-config-template (clientPort clientPubKey
                               all
                               others
                               myPublicKey myPort myPrivateKey)
  "PORTA PUBKEYA PORTB PUBKEYB PORTC PUBKEYC PORTD PUBKEYD are the servers.
OTHERNODE1PORT OTHERNODE2PORT OTHERNODE3PORT are the other nodes.
MYPUBLICKEY MYPORT MYPRIVATEKEY are my stuff."
(concat
"clientTimeoutLimit: 50000
publicKeys:
" (public-keys-template all)
"heartbeatTimeout: 50000
dontDebugFollower: false
apiPort: 8000
clientPublicKeys:
" (public-keys-template `(,@all (,clientPort ,clientPubKey)))
"electionTimeoutRange:
- 100000
- 200000
otherNodes:
" (address-template others)
"myPublicKey: "myPublicKey"
nodeId:
" (node-id-template myPort)
"enableDebug: true
myPrivateKey: "myPrivateKey"
batchTimeDelta: 1 % 100
"))

(defun client-config-template (clientPort clientPubKey clientPrivateKey all)
(concat
"clientTimeoutLimit: 50000
publicKeys:
" (public-keys-template all)
"heartbeatTimeout: 1500000
dontDebugFollower: false
apiPort: 8000
clientPublicKeys:
" (public-keys-template `((,clientPort ,clientPubKey)))
"electionTimeoutRange:
- 3000000
- 6000000
otherNodes:
" (address-template (mapcar #'1st all))
"myPublicKey: "clientPubKey"
nodeId:
" (node-id-template clientPort)
"enableDebug: false
myPrivateKey: "clientPrivateKey"
batchTimeDelta: 1 % 100
"))

(defvar server-public-private-keys
  '(("9b58735cb5f329c49fe7922177ab8947b6d615f9bfadb1967c79cf211a219eab"
     "f1d41018daf0e140347888e9e7d4260ec5e3a351cb3c7da85a2a10729f1bf3c9")
    ("9e0f7aba65edab698a726c88249a8ff3079b11e2d905dce3416a0dc73a223343"
     "6d2703126ecc4f267a1f49f7b395ee4ed2e8b8b39c7748248e7ae9692751ea86")
    ("97f4ff8d9aab8492e872a394a3aecfc7c25dd990c234df14677670cdfdec3f1f"
     "afcd83f5089c6b6bf3fb144ef3f50c975e7df4b5f115855e246e7fe1ca8b654a")
    ("0bd04e4049684b4201319c3b4371cf25587cb69c5d2ae638beb9ffe9c16cf99d"
     "8a8bea0e250ef166b86bcbfa1565af95bc8525b71b289b53b5494ee04754b202")
    ))

(defvar client-public-private-keys
  '("0d697028fee9ca00a395c25d489f877ef68cd3725d5aa134e1f1fe98cc0b3922"
    "c8f0c17e4a8d14d0f4f7173630414c0f9497d6e5d9dc2c5c334bd183bc67fe21"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun 1st (x) "X[0]." (car x))
(defun 2nd (x) "X[1]." (cadr x))

(defun mapT (f l)
  "Map F over L."
  (mapT-aux f l '() l))

(defun mapT-aux (f all seen upcoming)
  "F is given ALL, SEEN, the current element, and UPCOMING."
  (if (null upcoming)
      nil
    (cons (funcall f all seen (car upcoming) (cdr upcoming))
          (mapT-aux f
                    all
                    (append seen (list (car upcoming))) ;; TODO
                    (cdr upcoming)))))

(defun zip (lists)
  "Zip the lists inside LISTS."
  (apply #'cl-mapcar #'list lists))

(provide 'hcJuno)
;;; hcJuno.el ends here
