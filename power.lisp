;; Copyright (C) 2016 Kyle K
;;
;;  This file is part of stumpwm.
;;
;; stumpwm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; stumpwm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

(in-package :stumpwm)

(export '())

;; ---
;; autodetect whether systemctl is available. if not, perhaps autodetect other
;; things that may be helpful to our cause. the aim is to get a shutdown/reboot
;; stumpwm command that works out of the box for most users, with some support
;; for suspend/hibernate/hybrid
;;
;; allow user to specify custom commands using a set function with keyword args
;; for other cases. easy enough to add additional features (e.g. halt etc.)
;; this way.

(defun systemctl-present ()
  "Detects whether systemctl is in the PATH."
  (string/= "" (run-shell-command "which systemctl" t)))

(defparameter *current-power-mode*
  (cond ((systemctl-present) 'systemctl)
        (t 'standard)))

(defparameter *power-modes*
  '((systemctl "systemctl poweroff" "systemctl reboot"
               "systemctl suspend" "systemctl hibernate")
    (standard "shutdown now" "reboot" "" "")
    (custom "" "" "" ""))
  "A-list of different modes available for powering down/rebooting the system. 
Entries are of the format:
(MODE-NAME <SHUTDOWN-CMD> <REBOOT-CMD> <SUSPEND-CMD> <HIBERNATE-CMD>)")

(defun set-power-command (&key shutdown reboot suspend hibernate)
  "Sets the appropriate custom power command's entry for the keyword arguments 
provided. For example, if @var{shutdown} is a string, the custom power mode will
use the string for the stumpwm shutdown command. Finally, if arguments have been
provided, @var{*custom-power-mode*} is set to 'custom."
  (unless (or shutdown reboot suspend hibernate)
    (let ((mode (assoc 'custom *power-modes*))
          (without-custom (remove 'custom *power-modes* :key #'car)))
      (if (stringp shutdown) (setf (elt mode 1) shutdown))
      (if (stringp reboot) (setf (elt mode 2) reboot))
      (if (stringp suspend) (setf (elt mode 3) suspend))
      (if (stringp hibernate) (setf (elt mode 4) hibernate))
      (setf *power-modes* (cons mode without-custom))
      (setf *current-power-mode* 'custom))))

(defcommand shutdown (&key (mode *current-power-mode*)) ()
  "Shuts down the system (using @var{mode} if provided, else uses 
@var{*current-power-mode*}, TODO: prompt the user unless @{no-prompt} is t."
  (let ((command (elt (assoc mode *power-modes*) 1)))
    (if (string/= "" command)
        (run-shell-command command)
        (message "^B^1*Shutdown command is an empty string!"))))

(defcommand reboot (&key (mode *current-power-mode*)) ()
  "Reboots the system (using @var{mode} if provided, else uses 
@var{*current-power-mode*}, TODO: prompt the user unless @{no-prompt} is t."
  (let ((command (elt (assoc mode *power-modes*) 2)))
    (if (string/= "" command)
        (run-shell-command command)
        (message "^B^1*Reboot command is an empty string!"))))
