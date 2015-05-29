;; -*-lisp-*-
;;
;; init.lisp

(in-package :stumpwm)

;;
;; -- helper functions (move to separate files) --
;;

(defvar *whitespace* '(#\Space #\Newline #\Tab))

(defun whitespace-trim (str)
  (string-trim *whitespace* str))

(defun whitespace-left-trim (str)
  (string-left-trim *whitespace* str))

(defun whitespace-right-trim (str)
  (string-right-trim *whitespace* str))


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

;; obtain battery status
;;   formatted as +XX% when    charging
;;                -XX% when discharging
;;                100% when full
(setf battery-status-command
  "acpi -b |
   awk -F '[ ,]' '{printf \"%s%s\", $3, $5}' |
   sed s/Discharging/\-/ |
   sed s/Unknown// |
   sed s/Full// |
   sed s/Charging/+/ |
   awk '{printf \"%4s\", $1}' ")

(setf vol-status-command
  "amixer sget Master |
   awk -F'[][]' '/dB/ { printf \"%s\", $6 }' |
   sed 's/on/Vol\\./' |
   sed 's/off/Mut\\./'")

(setf vol-percent-command
  "amixer sget Master |
   awk -F'[][]' '/dB/ { printf \"%4s\", $2 }'")

(setf *time-modeline-string*
      "%a %b %e %k:%M")

(setf *screen-mode-line-format*
      (list "[^B%n^b] %W^>"
            '(:eval (run-shell-command battery-status-command t))
            " | "
            '(:eval (run-shell-command vol-status-command t))
            " "
            '(:eval (run-shell-command vol-percent-command t))
            " | %d"))

(setf *mode-line-timeout* 10)

;; Turn on the modeline
(if (not (head-mode-line (current-head)))
    (toggle-mode-line (current-screen) (current-head)))

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

;;
;; -- key bindings --
;;

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



;;
;; -- message and input bar --
;;

;; center message and input windows on screen
(dolist (var '(*message-window-gravity*
               *input-window-gravity*))
  (set var :center))
