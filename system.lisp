;; -*-lisp-*-
;;
;; system.lisp

(in-package :stumpwm)

;;
;; -- volume commands --
;;

(defcommand volume-up () ()
  (run-shell-command "amixer set Master 1%+ >> /dev/null" t))

(defcommand volume-down () ()
  (run-shell-command "amixer set Master 1%- >> /dev/null" t))

(defcommand volume-toggle () ()
  (run-shell-command "amixer set Master toggle >> /dev/null" t))

;;
;; -- brightness commands --
;;

(defcommand brightness-up () ()
  (run-shell-command "xbacklight -inc 10"))

(defcommand brightness-down () ()
  (run-shell-command "xbacklight -dec 10"))
