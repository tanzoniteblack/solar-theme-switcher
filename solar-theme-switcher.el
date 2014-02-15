;;; solar-theme-switcher.el
;;; Author: Ryan Smith (rnsmith2@gmail.com)
;;; Date 02/14/2014
;;; Inspired (and forked) from moe-theme-switcher by kuanyui (https://github.com/kuanyui/moe-theme.el)
;;;
;;; Run like this to have the moe-light theme used when daylight outside and moe-dark theme used when dark
;;; (initialize-solar-theme-switcher 'moe-light 'moe-dark)
;;;
;;; Sunrise and sunset times are read using (solar-sunrise-and-sunset) from built in solar package. In order to use this
;;; longitude and latitude must be set, call (solar-setup) to configure this information. Or before calling
;;; initialize-solar-theme-switcher set latitude and longitude in .emacs.d like:
;;; (setq calendar-latitude +38.8977) (setq calendar-longitude -77.0366)
;;;
;;; If we're not able to actually
;;; get real sunrise and sunset we'll use the fixed ones set in fixed-sunrise (default 7:00) and fixed-sunset (default 18:00).
;;; If you want to always use a fixed time, then set use-fixed-sunrise-sunset to a non-nil value
;;;
;;; Sunrise & Sunset times can be varied by use of the sunrise-flex-time and sunset-flex-time variables.
;;; Setting these to 15, for example, will cause anything 15 minutes before sunrise and 15 minutes before
;;; sunset to be treated as sunrise/sunset.
;;;
;;; By default which theme should be loaded is rechecked every 10 minutes, this can be varied with the variable
;;; solar-theme-switcher-check-daylight-every or you can just cancel the running timer with (cancel-solar-theme-switcher)
;;;
;;; If you don't wish to load a timer to have the theme continually checked for daylight, then just run the function
;;; switch-theme-by-daylight
;;;
;;; If the themes you wish to use don't come with emacs, then be sure to load the directory they're contained in to
;;; custom-theme-load-path, such as: (add-to-list 'custom-theme-load-path "~/.live-packs/ryan-pack/lib/moe-theme.el/")

(require 'solar)

(defvar solar-theme-switcher-current-theme nil "Variable to hold the current theme used for solar-theme-switcher.el.")

(defun switch-theme (theme-to-switch-to)
  "Avoid unnecessary load-theme and screen flashing in GUI version Emacs"
  (unless (equal solar-theme-switcher-current-theme
                 theme-to-switch-to)
    (progn (load-theme theme-to-switch-to t)
           (set 'solar-theme-switcher-current-theme theme-to-switch-to))))

(defvar sunrise-flex-time 15 "Consider it to be after sunrise if we're within N minutes of actual sunrise.")
(defvar sunset-flex-time 15 "Consider it to be after sunset if we're within N minutes of actual sunset.")
(defvar use-fixed-sunrise-sunset nil "Always use fixed sunrise/sunset times.")
(defvar fixed-sunrise 7.00 "Fake sunrise time, default is 7:00 am")
(defvar fixed-sunset 18.00 "Fake sunset time, default is 18:00 (6:00 pm)")

(defun daylightp ()
  (let* ((sun-info (if (and (not use-fixed-sunrise-sunset) calendar-latitude calendar-longitude calendar-time-zone)
					   (solar-sunrise-sunset (calendar-current-date))
					 (list (list fixed-sunrise "PST") (list fixed-sunset "PST"))))
         (sunrise-minutes (* 60 (car (car sun-info))))
         (sunset-minutes (* 60 (car (second sun-info))))
         (now (+ (* 60 (string-to-number (format-time-string "%H")))
                 (string-to-number (format-time-string "%M")))))
    (and (< sunrise-minutes
            (+ now sunrise-flex-time))
         (> sunset-minutes
            (+ now sunset-flex-time)))))

(defun switch-theme-by-daylight (light-theme dark-theme)
  "When light outside, switch to given light theme, when dark outside switch to given dark theme."
  (if (daylightp)
      (switch-theme light-theme)
    (switch-theme dark-theme)))

(defvar solar-theme-switcher-running-timer nil "Variable to hold the running timer object.")
(defvar solar-theme-switcher-check-daylight-every 10 "Check to see if still (not) daylight every N minutes.")

(defun cancel-solar-theme-switcher ()
  (when solar-theme-switcher-running-timer
    (progn (cancel-timer solar-theme-switcher-running-timer)
           (set 'solar-theme-switcher-running-timer nil))))

(defun initialize-solar-theme-switcher (light-theme dark-theme)
  "Initialize solar-theme-switcher.el to check whether it's daylight or not and adjust between the given light and dark themes appropriately. Cancel this by running cancel-solar-theme-switcher"
  (cancel-solar-theme-switcher)
  (switch-theme-by-daylight light-theme dark-theme)
  (setq solar-theme-switcher-running-timer
        (run-at-time (* 60 solar-theme-switcher-check-daylight-every)
                     (* 60 solar-theme-switcher-check-daylight-every)
                     'switch-theme-by-daylight
                     light-theme
                     dark-theme)))

(provide 'solar-theme-switcher)
