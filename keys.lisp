;; -*-lisp-*-
;;
;; keys.lisp

(in-package :stumpwm)

(defmacro fill-keymap (map &rest bindings)
  "wipes the keymap and adds list of binding pairs"
  `(setf ,map
         (let ((m (make-sparse-keymap)))
           ,@(loop for i = bindings then (cddr i)
                   while i
                   collect `(define-key m ,(first i) ,(second i)))
           m)))

;; change the prefix key to Control-q
(set-prefix-key (kbd "C-q"))

(setf *escape-key* (kbd "C-q"))
(setf *escape-fake-key* (kbd "q"))

;; map for launching new applications
(defvar *new-apps-map* nil)
;; map for chatting programs
(defvar *chat-map* nil)
;; map for system information and commands
(defvar *system-map* nil)
;; map for power commands
(defvar *power-map* nil)


;; bare keybindings

(fill-keymap *top-map*
  ;; root map
  *escape-key* '*root-map*
  ;; volume controls
  (kbd "XF86AudioLowerVolume") "volume-down 1"
  (kbd "XF86AudioRaiseVolume") "volume-up 1"
  (kbd "XF86AudioMute")        "volume-toggle"
  ;; brightness controls
  (kbd "XF86MonBrightnessDown") "brightness-down 10"
  (kbd "XF86MonBrightnessUp")   "brightness-up 10"
  ;; super shortcuts
  (kbd "s-t") "urxvt"
  (kbd "s-e") "emacs"
  (kbd "s-f") "firefox"
  (kbd "s-;") "fnext")



;; prefix keybindings
(fill-keymap *root-map*
  (kbd "h")   '*help-map*
  (kbd "C-a") '*new-apps-map*
  (kbd "C-y") '*chat-map*
  (kbd "C-s") '*system-map*
  (kbd "s-p") '*power-map*
  (kbd "g")   '*groups-map*
  (kbd "x")   '*exchange-window-map*
  ;; switch to apps
  (kbd "C-t") "urxvt"
  (kbd "C-e") "emacs"
  (kbd "C-f") "firefox"
  (kbd "C-c") "google-chrome-stable"
  (kbd "C-m") "ncmpcpp"
  ;;
  (kbd "C-b") "banish"
  (kbd "!")   "exec"
  (kbd "C-g") "abort"
  (kbd ";")   "colon"
  (kbd ":")   "eval"
  (kbd "m") "lastmsg"
  (kbd "G")   "vgroups"
  *escape-fake-key* "send-escape"
  (kbd "F1")  "gselect 1"
  (kbd "F2")  "gselect 2"
  (kbd "F3")  "gselect 3"
  (kbd "F4")  "gselect 4"
  (kbd "F5")  "gselect 5"
  (kbd "F6")  "gselect 6"
  (kbd "F7")  "gselect 7"
  (kbd "F8")  "gselect 8"
  (kbd "F9")  "gselect 9"
  (kbd "F10") "gselect 10")

(fill-keymap *new-apps-map*
  (kbd "C-t") "exec urxvt"
  (kbd "C-e") "exec emacsclient -c -a \"\""
  (kbd "C-f") "exec firefox"
  (kbd "C-c") "exec google-chrome-stable"
  (kbd "C-m") "exec urxvt -e ncmpcpp")

(fill-keymap *chat-map*
  (kbd "s")   "skype"
  (kbd "C-s") "skype"
  (kbd "t")   "teamspeak"
  (kbd "C-t") "teamspeak")


(fill-keymap *system-map*
  (kbd "t")   "time"
  (kbd "C-t") "time"
  (kbd "a")   "alsamixer"
  (kbd "C-a") "alsamixer"
  (kbd "v")   "volume-set"
  (kbd "C-v") "volume-set"
  (kbd "b")   "brightness-set"
  (kbd "C-b") "brightness-set")

(fill-keymap *power-map*
  (kbd "C-S") "power-shutdown y"
  (kbd "C-R") "power-reboot y"
  (kbd "C-P") "power-suspend y"
  (kbd "C-H") "power-hibernate y")

(fill-keymap *group-top-map*
  *escape-key* '*group-root-map*)

(fill-keymap *group-root-map*
  (kbd "C-u") "next-urgent"
  (kbd "C-k") "delete"
  (kbd "K")   "kill"
  (kbd "'")   "select"
  (kbd "\"")  "windowlist"
  (kbd "0")   "select-window-by-number 0"
  (kbd "1")   "select-window-by-number 1"
  (kbd "2")   "select-window-by-number 2"
  (kbd "3")   "select-window-by-number 3"
  (kbd "4")   "select-window-by-number 4"
  (kbd "5")   "select-window-by-number 5"
  (kbd "6")   "select-window-by-number 6"
  (kbd "7")   "select-window-by-number 7"
  (kbd "8")   "select-window-by-number 8"
  (kbd "9")   "select-window-by-number 9"
  (kbd "C-N") "number"
  (kbd "#")   "mark"
  (kbd "F11") "fullscreen"
  (kbd "A")   "title"
  (kbd "i")   "info")

(fill-keymap *tile-group-top-map*
  *escape-key* '*tile-group-root-map*)

(fill-keymap *tile-group-root-map*
  (kbd "C-n")     "pull-hidden-next"
  (kbd "M-n")     "next"
  (kbd "C-M-n")   "next-in-frame"
  (kbd "C-SPC")   "pull-hidden-next"
  (kbd "p")       "pull-hidden-previous"
  (kbd "C-p")     "pull-hidden-previous"
  (kbd "M-p")     "prev"
  (kbd "C-M-p")   "prev-in-frame"
  (kbd "W")       "place-existing-windows"
  *escape-key*    "pull-hidden-other"
  (kbd "M-t")     "other-in-frame"
  (kbd "C-0")     "pull 0"
  (kbd "C-1")     "pull 1"
  (kbd "C-2")     "pull 2"
  (kbd "C-3")     "pull 3"
  (kbd "C-4")     "pull 4"
  (kbd "C-5")     "pull 5"
  (kbd "C-6")     "pull 6"
  (kbd "C-7")     "pull 7"
  (kbd "C-8")     "pull 8"
  (kbd "C-9")     "pull 9"
  (kbd "R")       "remove"
  (kbd "s")       "vsplit"
  (kbd "S")       "hsplit"
  (kbd "r")       "iresize"
  (kbd "o")       "fnext"
  (kbd "C-o")     "fnext"
  (kbd "TAB")     "fnext"
  (kbd "M-TAB")   "fother"
  (kbd "f")       "fselect"
  (kbd "F")       "curframe"
  (kbd "-")       "fclear"
  (kbd "Q")       "only"
  (kbd "Up")      "move-focus up"
  (kbd "Down")    "move-focus down"
  (kbd "Left")    "move-focus left"
  (kbd "Right")   "move-focus right"
  (kbd "M-Up")    "move-window up"
  (kbd "M-Down")  "move-window down"
  (kbd "M-Left")  "move-window left"
  (kbd "M-Right") "move-window right"
  (kbd "+")       "balance-frames"
  (kbd "l")       "redisplay"
  (kbd "C-l")     "redisplay"
  (kbd "s-l")     "exec slock")

(fill-keymap *groups-map*
  (kbd "g")     "groups"
  (kbd "c")     "gnew"
  (kbd "C-n")   "gnext"
  (kbd "C-SPC") "gnext"
  (kbd "N")     "gnext-with-window"
  (kbd "C-p")   "gprev"
  (kbd "P")     "gprev-with-window"
  (kbd "o")     "gother"
  (kbd "'")     "gselect"
  (kbd "\"")    "grouplist"
  (kbd "m")     "gmove"
  (kbd "M")     "gmove-marked"
  (kbd "k")     "gkill"
  (kbd "A")     "grename"
  (kbd "r")     "grename"
  (kbd "1")     "gselect 1"
  (kbd "2")     "gselect 2"
  (kbd "3")     "gselect 3"
  (kbd "4")     "gselect 4"
  (kbd "5")     "gselect 5"
  (kbd "6")     "gselect 6"
  (kbd "7")     "gselect 7"
  (kbd "8")     "gselect 8"
  (kbd "9")     "gselect 9"
  (kbd "0")     "gselect 10")

(fill-keymap *exchange-window-map*
  (kbd "Up")    "exchange-direction up"
  (kbd "Down")  "exchange-direction down"
  (kbd "Left")  "exchange-direction left"
  (kbd "Right") "exchange-direction right"
  (kbd "p")     "exchange-direction up"
  (kbd "n")     "exchange-direction down"
  (kbd "b")     "exchange-direction left"
  (kbd "f")     "exchange-direction right"
  (kbd "k")     "exchange-direction up"
  (kbd "j")     "exchange-direction down"
  (kbd "l")     "exchange-direction left"
  (kbd "h")     "exchange-direction right")

(fill-keymap *help-map*
  (kbd "v") "describe-variable"
  (kbd "f") "describe-function"
  (kbd "k") "describe-key"
  (kbd "c") "describe-command"
  (kbd "w") "where-is")
