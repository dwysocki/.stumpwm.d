;; -*-lisp-*-
;;
;; apps.lisp

(in-package :stumpwm)

(defcommand urxvt () ()
  (run-or-raise "urxvt" '(:class "URxvt")))

(defcommand emacs () ()
  (run-or-raise "emacsclient -c -a \"\"" '(:class "Emacs")))

(defcommand firefox () ()
  (run-or-raise "firefox" '(:class "Firefox")))

(defcommand google-chrome-stable () ()
  (run-or-raise "google-chrome-stable" '(:class "Google-chrome-stable")))

(defcommand ncmpcpp () ()
  (run-or-raise "urxvt -e ncmpcpp" '(:title "ncmpcpp")))
