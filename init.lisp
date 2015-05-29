;; -*-lisp-*-
;;
;; init.lisp

(in-package :stumpwm)

;;
;; -- misc --
;;

;; change the prefix key to something else
(set-prefix-key (kbd "C-t"))

;; change focus on mouse click
(setf *mouse-focus-policy* :click)

;;
;; -- modelne --
;;

;; Turn on the modeline
(if (not (head-mode-line (current-head)))
     (toggle-mode-line (current-screen) (current-head)))

;; Show time, cpu usage and network traffic in the modeline
(setf *screen-mode-line-format*
      (list '(:eval (run-shell-command "date '+%R, %F %a'|tr -d [:cntrl:]" t))
	    '(:eval (run-shell-command "acpi -b | cut -d " " -f 3-4" t))
	    " [^B%n^b] %W"))


;;
;; -- key bindings --
;;

(defun define-keys (map key-list command)
  (dolist (key key-list)
    (define-key map (kbd key) command)))

;; bind urxvt to $PREFIX C-x
(define-key *root-map* (kbd "C-x") "exec urxvt")
;; bind Firefox to $PREFIX C-f
(define-key *root-map* (kbd "C-f") "exec firefox")
;; bind Google Chrome to $PREFIX C-c
(define-key *root-map* (kbd "C-c") "exec google-chrome-stable")


;;
;; -- message and input bar --
;;

;; center message and input windows on screen
(dolist (var '(*message-window-gravity*
	       *input-window-gravity*))
  (set var :center))
