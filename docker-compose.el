;;; docker-compose.el ---                            -*- lexical-binding: t; -*-
;; Copyright (C) 2017  Charlie Brodie

;; Author: Charlie Brodie <charliembrodie@gmail.com>
;; Keywords: tools


;;; docker.el --- Emacs interface to Docker

;; Author: Philippe Vaucher <philippe.vaucher@gmail.com>
;; URL: https://github.com/Silex/docker.el
;; Keywords: filename, convenience
;; Version: 0.5.2
;; Package-Requires: ((emacs "24.4") (dash "2.12.1") (docker-tramp "0.1") (magit-popup "2.6.0") (s "1.11.0") (tablist "0.70") (json-mode "1.7.0") (docker "20171121.2316")

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; # Emacs interface to Docker-Compose!
;;
;; This package allows you to interact with docker-compose from Emacs.
;; This package is lifted near-totally from the wonderful Docker package with at most, nominal changes.

;;; Code:

(defgroup docker-compose nil
  "Docker-Compose customization group."
  :group 'convenience)

(defun docker-compose-get-home ()
  "Get the desired docker-compose file."
  (interactive)
  (let ((compose-list (split-string
		       (shell-command-to-string "locate docker-compose.yml | grep -v \\~")))
	)
    (let ((chosen-path (completing-read "Select compose file: " compose-list)))
      (defvar docker-compose-home chosen-path)
      (defvar docker-compose-name
	    (file-name-nondirectory
	     (directory-file-name
	      (file-name-directory chosen-path)))))))


(defcustom docker-compose-keymap-prefix "C-c d c"
  "Prefix for `docker-compose-mode'."
  :group 'docker
  :type 'string)

;; (defvar docker-compose-images-command-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "d" 'docker-compose-rmi)
;;     (define-key map "f" 'docker-compose-pull)
;;     (define-key map "i" 'docker-compose-images)
;;     (define-key map "p" 'docker-compose-push)
;;     (define-key map "r" 'docker-compose-run)
;;     map)
;;   "Keymap for docker images.")

(defvar docker-compose-containers-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'docker-compose-containers)
    (define-key map "d" 'docker-compose-rm)
    (define-key map "u" 'docker-compose-unpause)
    (define-key map "o" 'docker-compose-stop)
    (define-key map "p" 'docker-compose-pause)
    (define-key map "r" 'docker-compose-restart)
    (define-key map "k" 'docker-compose-kill)
    (define-key map "s" 'docker-compose-start)
    map)
  "Keymap for docker containers.")

;; (defvar docker-compose-volumes-command-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "d" 'docker-compose-volume-rm)
;;     (define-key map "v" 'docker-compose-volumes)
;;     map)
;;   "Keymap for docker volumes.")

;; (defvar docker-compose-networks-command-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "d" 'docker-compose-network-rm)
;;     (define-key map "n" 'docker-compose-networks)
;;     map)
;;   "Keymap for docker networks.")

(defvar docker-compose-command-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map "i" docker-compose-images-command-map)
    ;; (define-key map "I" 'docker-compose-images)
    (define-key map "c" docker-compose-containers-command-map)
    (define-key map "C" 'docker-compose-containers)
    ;; (define-key map "v" docker-compose-volumes-command-map)
    ;; (define-key map "V" 'docker-compose-volumes)
    ;; (define-key map "n" docker-compose-networks-command-map)
    ;; (define-key map "N" 'docker-compose-networks)
    (define-key map "B" 'dockerfile-build-buffer)
    map)
  "Keymap for `docker-compose-mode' after `docker-compose-keymap-prefix' was pressed.")

(defvar docker-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd docker-compose-keymap-prefix) docker-compose-command-map)
    map)
  "Keymap for `docker-compose-mode'.")

;;;###autoload
(define-minor-mode docker-compose-mode
  "Minor mode to manage docker-compose."
  nil
  " docker-compose"
  docker-compose-mode-map
  :group 'docker-compose)

;;;###autoload
(define-globalized-minor-mode docker-compose-global-mode
  docker-compose-mode
  docker-compose-mode)

(provide 'docker-compose)

;;; docker-compose.el ends here
