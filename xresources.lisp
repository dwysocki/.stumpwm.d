;; -*-lisp-*-
;;
;; xresources.lisp

;; This reads settings in from X's resources. If a value is not set, the default
;; from StumpWM is used. Sample entries for ~/.Xresources are listed below,
;; simply append them to ~/.Xresources, modify the values to your liking,
;; and reload with `xrdb -merge ~/.Xresources'. The changes will take effect
;; after StumpWM is restarted.
;;
;; ~/.Xresources |
;; --------------+
;;
;; stumpwm.fg.color:                   #93a1a1
;; stumpwm.bg.color:                   #002b36
;; stumpwm.border.color:               #93a1a1
;; stumpwm.msg.border.width:           4
;; stumpwm.win.bg.color:               #002b36
;; stumpwm.focus.color:                #fdf6e3
;; stumpwm.unfocus.color:              #93a1a1
;; stumpwm.float.focus.color:          #fdf6e3
;; stumpwm.float.unfocus.color:        #93a1a1
;; stumpwm.mode.line.border.width:     2
;; stumpwm.mode.line.pad.x:            5
;; stumpwm.mode.line.pad.y:            2
;; stumpwm.mode.line.background.color: #002b36
;; stumpwm.mode.line.foreground.color: #93a1a1
;; stumpwm.mode.line.border.color:     #93a1a1
;;


(in-package :stumpwm)

(export '(get-resource))

(defun resource-query (name)
  "Returns the shell command used by `get-resource'"
  (concatenate 'string
    ;; list all resources
    "xrdb -query -all     |"
    ;; search for ones containing name
    "grep -e \"" name "\" |"
    ;; take the last entry
    "tail -n 1            |"
    ;; take the 2nd column, containing the value
    "cut -f 2             |"
    ;; remove the trailing newline
    "tr -d '\\n'"))

(defun get-resource (name)
  "Returns the value bound to NAME in X resources."
  (let ((output (run-shell-command (resource-query name) t)))
    (when (> (length output) 0)
      output)))


;;
;; -- extract resources --
;;

(defvar *fg-color*
  (get-resource "stumpwm.fg.color"))

(defvar *bg-color*
  (get-resource "stumpwm.bg.color"))

(defvar *border-color*
  (get-resource "stumpwm.border.color"))

(defvar *msg-border-width*
  (let ((value (get-resource "stumpwm.msg.border.width")))
    (when value
      (parse-integer value))))

(defvar *font*
  (get-resource "stumpwm.font"))

(defvar *win-bg-color*
  (get-resource "stumpwm.win.bg.color"))

(defvar *focus-color*
  (get-resource "stumpwm.focus.color"))

(defvar *unfocus-color*
  (get-resource "stumpwm.unfocus.color"))

(defvar *float-focus-color*
  (get-resource "stumpwm.float.focus.color"))

(defvar *float-unfocus-color*
  (get-resource "stumpwm.float.unfocus.color"))

;; the following resources already have variables
;; just overwrite them if anything is set in X resources

(setf *mode-line-border-width*
  (let ((value (get-resource "stumpwm.mode.line.border.width")))
    (if value
      (parse-integer value)
      *mode-line-border-width*)))

(setf *mode-line-pad-x*
  (let ((value (get-resource "stumpwm.mode.line.pad.x")))
    (if value
      (parse-integer value)
      *mode-line-pad-x*)))

(setf *mode-line-pad-y*
  (let ((value (get-resource "stumpwm.mode.line.pad.y")))
    (if value
      (parse-integer value)
      *mode-line-pad-y*)))

(setf *mode-line-background-color*
  (or (get-resource "stumpwm.mode.line.background.color")
      *mode-line-background-color*))

(setf *mode-line-foreground-color*
  (or (get-resource "stumpwm.mode.line.foreground.color")
      *mode-line-foreground-color*))

(setf *mode-line-border-color*
  (or (get-resource "stumpwm.mode.line.border.color")
      *mode-line-border-color*))


;;
;; -- assign resources --
;;

(defun apply-resources (resource-pairs)
  (dolist (pair resource-pairs)
    (let ((var (eval (first pair)))
          (fun (second pair)))
      (when var
        (funcall fun var)))))

(apply-resources
  '((*fg-color*            set-fg-color)
    (*bg-color*            set-bg-color)
    (*border-color*        set-border-color)
    (*msg-border-width*    set-msg-border-width)
    (*font*                set-font)
    (*win-bg-color*        set-win-bg-color)
    (*focus-color*         set-focus-color)
    (*unfocus-color*       set-unfocus-color)
    (*float-focus-color*   set-float-focus-color)
    (*float-unfocus-color* set-float-unfocus-color)))


;;
;; -- altering X resources --
;;

(defcommand xresources-load () ()
  "Load X resources from ~/.Xresources"
  (run-shell-command "xrdb -load ~/.Xresources"))

(defcommand xresources-merge () ()
  "Merge X resources from ~/.Xresources"
  (run-shell-command "xrdb -merge ~/.Xresources"))
