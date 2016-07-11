;; -*- lexical-binding: t-*-

;;; package --- Summary
;;; Commentary:

;; TODO
;; - Generalize to N servers (rather than always 4).
;; - Refactor templates

;; OPERATION
;; (gc   20001)
;; (sjc  20005)
;; (sjss 20001)
(comment
 CreateAccount foo
 ObserveAccounts
 ObserveAccount foo
 CreateAccount bar
 AdjustAccount foo (1%1)
 transfer(foo->bar,101%100)
 ;;(#transfer "foo" "bar" (% 110 100) "baz")
)
;; (kajs)

;;; Code:

(defvar juno-home (concat (getenv "HOME") "/ws/juno-orahub"))

(defun sjc (clientPort)
  (interactive)
  (let ((cmd (concat "stack exec junoclient --"
                     " -c  /tmp/" (number-to-string clientPort) "-client.yaml")))
    (pop-to-buffer (get-buffer-create (generate-new-buffer-name "*juno-client*")))
    (cd juno-home)
    (shell (current-buffer))
    (insert cmd)))

(defun sjss (startPort)
  "Shorthand: spawn all servers."
  (interactive)
  (spawn-junoservers startPort))

(defun spawn-junoservers (startPort)
  "Spawn all servers."
  (mapc #'(lambda (i)
            (spawn-junoserver (1st i) (2nd i))
            (sleep-for 1))
        `((1 ,startPort) (2 ,(+ startPort 1)) (3 ,(+ startPort 2)) (4 ,(+ startPort 3)))))

(defun sjs (n port)
  "Shorthand: spawn a single server N."
  (interactive)
  (spawn-junoserver n port))

(defun spawn-junoserver (n port)
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

(defun kill-all-junoservers ()
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
  (kill-all-junoservers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun gc (startPort)
  "STARTPORT."
  (mapc #'(lambda (portAndContents)
            (let* ((serverInfo (1st portAndContents))
                   (serverPort (1st serverInfo))
                   (serverConf (2nd serverInfo))
                   (clientInfo (2nd portAndContents))
                   (clientPort (1st clientInfo))
                   (clientConf (2nd clientInfo)))
              (with-temp-file
                  (concat "/tmp/" serverPort "-cluster.yaml")
                (insert serverConf))
              ;; TODO: this creates and (overwrites) the client conf multiple times
              (with-temp-file
                  (concat "/tmp/" clientPort "-client.yaml")
                (insert clientConf))))
        (gc1 startPort)))

(defun gc1 (startPort)
  "STARTPORT."
  (let* ((portKeyPairs
          (zip (list (mapcar #'number-to-string
                             (list startPort (+ startPort 1) (+ startPort 2) (+ startPort 3)))
                     server-public-private-keys)))
         (clientPort       (number-to-string (+ startPort 4)))
         (clientPubKey     (1st client-public-private-keys))
         (clientPrivateKey (2nd client-public-private-keys))
         (gcf #'(lambda (a s c u) (gc2 clientPort clientPubKey clientPrivateKey a s c u))))
    (mapT gcf portKeyPairs)))

(defun gc2 (clientPort clientPubKey clientPrivateKey all s current u)
  "ALL S CURRENT U."
  (let* ((a  (1st all))
         (portA (1st a))
         (pubKeyA (1st (2nd a)))
         (b  (2nd all))
         (portB (1st b))
         (pubKeyB (1st (2nd b)))
         (c  (3rd all))
         (portC (1st c))
         (pubKeyC (1st (2nd c)))
         (d  (4th all))
         (portD (1st d))
         (pubKeyD (1st (2nd d)))
         (others (append s u))
         (o1 (1st (1st others)))
         (o2 (1st (2nd others)))
         (o3 (1st (3rd others)))
         (myPublicKey (1st (2nd current)))
         (myPort (1st current))
         (myPrivateKey (2nd (2nd current))))
    `((,myPort ,(server-config-template
                 clientPort clientPubKey
                 portA pubKeyA portB pubKeyB portC pubKeyC portD pubKeyD
                 o1 o2 o3
                 myPublicKey myPort myPrivateKey))
      (,clientPort ,(client-config-template
                     clientPort clientPubKey clientPrivateKey
                     portA pubKeyA portB pubKeyB portC pubKeyC portD pubKeyD)))
    ))

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
                               portA pubKeyA
                               portB pubKeyB
                               portC pubKeyC
                               portD pubKeyD
                               otherNode1Port otherNode2Port otherNode3Port
                               myPublicKey myPort myPrivateKey)
  "PORTA PUBKEYA PORTB PUBKEYB PORTC PUBKEYC PORTD PUBKEYD are the servers.
OTHERNODE1PORT OTHERNODE2PORT OTHERNODE3PORT are the other nodes.
MYPUBLICKEY MYPORT MYPRIVATEKEY are my stuff."
(concat
"clientTimeoutLimit: 50000
publicKeys:
" (public-keys-template `((,portA ,pubKeyA) (,portB ,pubKeyB) (,portC ,pubKeyC) (,portD ,pubKeyD)))
"heartbeatTimeout: 50000
dontDebugFollower: false
apiPort: 8000
clientPublicKeys:
" (public-keys-template `((,portA ,pubKeyA) (,portB ,pubKeyB) (,portC ,pubKeyC) (,portD ,pubKeyD)
                          (,clientPort ,clientPubKey)))
"electionTimeoutRange:
- 100000
- 200000
otherNodes:
" (address-template `(,otherNode1Port ,otherNode2Port ,otherNode3Port))
"myPublicKey: "myPublicKey"
nodeId:
" (node-id-template myPort)
"enableDebug: true
myPrivateKey: "myPrivateKey"
batchTimeDelta: 1 % 100
"))

(defun client-config-template (clientPort clientPubKey clientPrivateKey
                               portA pubKeyA
                               portB pubKeyB
                               portC pubKeyC
                               portD pubKeyD)
(concat
"clientTimeoutLimit: 50000
publicKeys:
- - fullAddr: tcp://127.0.0.1:"portA"
    host: '127.0.0.1'
    port: "portA"
  - "pubKeyA"
- - fullAddr: tcp://127.0.0.1:"portB"
    host: '127.0.0.1'
    port: "portB"
  - "pubKeyB"
- - fullAddr: tcp://127.0.0.1:"portC"
    host: '127.0.0.1'
    port: "portC"
  - "pubKeyC"
- - fullAddr: tcp://127.0.0.1:"portD"
    host: '127.0.0.1'
    port: "portD"
  - "pubKeyD"
heartbeatTimeout: 1500000
dontDebugFollower: false
apiPort: 8000
clientPublicKeys:
- - fullAddr: tcp://127.0.0.1:"clientPort"
    host: '127.0.0.1'
    port: "clientPort"
  - "clientPubKey"
electionTimeoutRange:
- 3000000
- 6000000
otherNodes:
- fullAddr: tcp://127.0.0.1:"portA"
  host: '127.0.0.1'
  port: "portA"
- fullAddr: tcp://127.0.0.1:"portB"
  host: '127.0.0.1'
  port: "portB"
- fullAddr: tcp://127.0.0.1:"portC"
  host: '127.0.0.1'
  port: "portC"
- fullAddr: tcp://127.0.0.1:"portD"
  host: '127.0.0.1'
  port: "portD"
myPublicKey: "clientPubKey"
nodeId:
  fullAddr: tcp://127.0.0.1:"clientPort"
  host: '127.0.0.1'
  port: "clientPort"
enableDebug: false
myPrivateKey: "clientPrivateKey"
batchTimeDelta: 1 % 100
"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun 1st (x) "X[0]." (car x))
(defun 2nd (x) "X[1]." (cadr x))
(defun 3rd (x) "X[3]." (caddr x))
(defun 4th (x) "X[4]." (cadddr x))

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
