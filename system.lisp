;; -*-lisp-*-
;;
;; system.lisp

(in-package :stumpwm)

;;
;; -- volume commands --
;;

(defcommand volume-up (percent)
  ((:number "percent: "))
  "Increase the volume by `percent'"
  (run-shell-command (concatenate 'string
                       "amixer set Master "
                       (write-to-string percent)
                       "%+")))

(defcommand volume-down (percent)
  ((:number "percent: "))
  "Decrease the volume by `percent'"
  (run-shell-command (concatenate 'string
                       "amixer set Master "
                       (write-to-string percent)
                       "%-")))

(defcommand volume-set (percent)
  ((:number "percent: "))
  "Set the volume to `percent'"
  (run-shell-command (concatenate 'string
                       "amixer set Master "
                       (write-to-string percent)
                       "%")))

(defcommand volume-toggle () ()
  (run-shell-command "amixer set Master toggle"))

;;
;; -- brightness commands --
;;

(defcommand brightness-up (percent)
  ((:number "percent: "))
  "Increase the screen's brightness by `percent'"
  (run-shell-command (concatenate 'string
                       "xbacklight -inc "
                       (write-to-string percent))))

(defcommand brightness-down (percent)
  ((:number "percent: "))
  "Decrease the screen's brightness by `percent'"
  (run-shell-command (concatenate 'string
                       "xbacklight -dec "
                       (write-to-string percent))))

(defcommand brightness-set (percent)
  ((:number "percent: "))
  "Set the screen's brightness to `percent'"
  (when (and percent (<= 0 percent 100))
    (run-shell-command (concatenate 'string
                         "xbacklight -set "
                         (write-to-string percent)))))

;;
;; -- power commands --
;;

(defcommand power-shutdown (confirm) ((:y-or-n "Shutdown? "))
  "Shutdown the computer."
  (when confirm
    (run-shell-command "systemctl poweroff")))

(defcommand power-reboot (confirm) ((:y-or-n "Reboot? "))
  "Restart the computer."
  (when confirm
    (run-shell-command "systemctl reboot")))

(defcommand power-suspend (confirm) ((:y-or-n "Suspend? "))
  "Suspend the computer."
  (when confirm
    (run-shell-command "systemctl suspend")))

(defcommand power-hibernate (confirm) ((:y-or-n "Hibernate? "))
  "Hibernate the computer."
  (when confirm
    (run-shell-command "systemctl hibernate")))
