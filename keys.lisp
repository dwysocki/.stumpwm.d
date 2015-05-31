;; -*-lisp-*-
;;
;; keys.lisp

(in-package :stumpwm)

;; change the prefix key to something else
(set-prefix-key (kbd "C-t"))

;; bind multiple keys to a single command
(defun define-keys (map key-list command)
  (dolist (key key-list)
    (define-key map (kbd key) command)))

;; bind urxvt to $PREFIX C-x
(define-key *root-map* (kbd "C-x") "exec urxvt")
;; bind Firefox to $PREFIX C-f
(define-key *root-map* (kbd "C-f") "exec firefox")
;; bind Google Chrome to $PREFIX C-c
(define-key *root-map* (kbd "C-c") "exec google-chrome-stable")

;; bind volume keys
(define-key *top-map* (kbd "XF86AudioLowerVolume") "volume-down")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "volume-up")
(define-key *top-map* (kbd "XF86AudioMute")        "volume-toggle")

;; bind brightness keys
(define-key *top-map* (kbd "XF86MonBrightnessDown") "brightness-down")
(define-key *top-map* (kbd "XF86MonBrightnessUp")   "brightness-up")


