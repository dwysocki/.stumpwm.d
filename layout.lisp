;; -*-lisp-*-
;;
;; layout.lisp

(in-package :stumpwm)

(export '(wallpaper-formats
          load-wallpaper))


;;
;; -- windows --
;;

(setf *window-border-style* :thin)


;;
;; -- message bar --
;;

;; center message bar
(setf *message-window-gravity* :center)


;;
;; -- input bar --
;;

;; center input bar
(setf *input-window-gravity* :center)


;;
;; -- wallpaper --
;;

(defvar *wallpaper-formats* '("png" "jpg" "jpeg")
  "A list of the allowed image formats for wallpapers, sorted by preference.")

(defun call-display (filename)
  (run-shell-command
    (concatenate 'string
      "display -window root " filename)))

(defcommand load-wallpaper (&optional filename) ((:string nil))
  "Load the wallpaper stored in ~/Pictures/wallpaper.*, or one provided as an
  argument."
  (let* (;; construct list of paths to potential wallpaper files,
         ;; sorted by most favorable format
         (candidate-paths (mapcar (lambda (extension)
                                    (concatenate 'string
                                      "~/Pictures/wallpaper."
                                      extension))
                                  *wallpaper-formats*))
         ;; append the provided filename if given
         (candidate-paths (if filename
                            (cons filename candidate-paths)
                            candidate-paths))
         ;; select the first file which exists
         (wallpaper-path (some #'probe-file candidate-paths)))
    (when wallpaper-path
      (run-shell-command
        (concatenate 'string
          "display -window root " (namestring wallpaper-path))))))

(load-wallpaper)
