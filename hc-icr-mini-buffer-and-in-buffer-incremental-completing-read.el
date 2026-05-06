;;; hc-icr-mini-buffer-and-in-buffer-incremental-completing-read.el --- Incremental Completing Read -*- lexical-binding: t; -*-

;;; Commentary:

;; https://www.chiply.dev/post-vompeccc

;; ICR: Incremental Completing Read
;;
;; Candidate display.
;; - Where do completion candidates appear?
;;   (e.g., minibuffer, vertically, horizontally, separate buffer, popup
;; - VERTICO

;; Filtering (i.e., 'matching')
;; - how does input match against candidates?
;; - ORDERLESS

;; Sorting (after filtering)
;; - display order (e.g., alpha, string length, recent, frequency)
;; - PRESCIENT

;; Annotation.
;; - candidates have associated metadata
;;   (e.g., 'M-x' : keybinding and docstring; file : size, modification date
;;   buffer : major mode and file path)
;; - MARGINALIA

;; Actions
;; - selecting a candidate and running an action
;;   (e.g., file : open, delete file; 'M-x' : describe function instead of run)
;; - EMBARK
;; - CONSULT

;; In-buffer completion.
;; - Everything above applies to the minibuffer.
;; - completion inside buffers
;;   (e.g., symbol completion while writing code, dictionary words while writing prose,
;;   file paths while editing configuration.)
;; - has its own display requirements (popup near cursor)
;;   CORFU
;; - has its own backend requirements (language servers, dynamic abbreviations, file system paths).
;;   CAPE

;;; Code:

;; ------------------------------------------------------------------------------
;; the display layer
(use-package hc-icr-mb-display-vertico)

;; ------------------------------------------------------------------------------
;; the filtering layer
(use-package hc-icr-mb-filtering-orderless)

;; ------------------------------------------------------------------------------
;; the sorting layer
(use-package hc-icr-mb-sorting-prescient)

;; ------------------------------------------------------------------------------
;; the annotation layer
(use-package hc-icr-mb-annotation-marginalia)

;; ------------------------------------------------------------------------------
;; the action layer
(use-package hc-icr-mb-action-embark)
(use-package hc-icr-mb-action-consult)

;; ==============================================================================

;; ------------------------------------------------------------------------------
;; the in-buffer display layer
(use-package hc-icr-ib-display-corfu)

;; ------------------------------------------------------------------------------
;; the in-buffer backend layer (completions)
(use-package hc-icr-ib-completions-cape)

;; ==============================================================================

(provide 'hc-icr-mini-buffer-and-in-buffer-incremental-completing-read)

;;; hc-icr-mini-buffer-and-in-buffer-incremental-completing-read.el ends here
