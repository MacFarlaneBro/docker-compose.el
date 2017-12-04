;;; docker-compose-containers.el --- Emacs interface to docker-compose-containers

;; Author: Philippe Vaucher <philippe.vaucher@gmail.com>
;;         Yuki Inoue <inouetakahiroki@gmail.com>

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

;;; Code:

(require 'docker-compose-process)
(require 'docker-compose-utils)
(require 'magit-popup)
(require 'tablist)
(require 'json)

(defcustom docker-compose-containers-show-all t
  "When nil, `docker-compose-containers' will only show running containers."
  :group 'docker
  :type 'boolean)

(defun docker-compose-containers-entries ()
  "Return the docker containers data for `tabulated-list-entries'."
  (let* ((fmt "[{{json .Names}},{{json .Status}},{{json .RunningFor}},{{json .Ports}}]")
         (data (docker "ps" (format "--format=\"%s\"" fmt) (when docker-compose-containers-show-all "-a ")))
         (lines (s-split "\n" data t)))
    (-map #'docker-compose-container-parse lines)))

;;;###autoload
(defun docker-compose-containers ()
  "List docker-compose containers."
  (interactive)
  (let ((compose-list (split-string
		       (shell-command-to-string "locate docker-compose.yml | grep -v \\~")))
	)
    (let ((arg (completing-read "Select compose file: " compose-list)))
	  (defvar docker-compose-home arg))))
  ;; (docker-compose-utils-pop-to-buffer "*docker-compose ps*")
  ;; (docker-compose-containers-mode)
  ;; (tablist-revert))

(defalias 'docker-compose-ps 'docker-compose-containers)


(define-derived-mode docker-compose-containers-mode tabulated-list-mode "Containers Menu"
  "Major mode for handling a list of docker containers."
  (setq tabulated-list-format [("Names" 20 t)("Status" 20 t)("RunningFor" 10 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Names" nil))
  (add-hook 'tabulated-list-revert-hook 'docker-compose-containers-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(defun docker-compose-container-parse (line)
  "Convert a LINE from \"docker ps\" to a `tabulated-list-entries' entry."
  (let (data)
    (condition-case err
        (setq data (json-read-from-string line))
      (json-readtable-error
       (error "could not read following string as json:\n%s" line)))
    (list (aref data 6) data)))

(defun docker-compose-read-container-name (prompt)
  "Read an container name using PROMPT."
  (completing-read prompt (-map #'car (docker-compose-containers-entries))))

;;;###autoload
(defun docker-compose-start (name)
  "Start the container named NAME."
  (interactive (list (docker-compose-read-container-name "Start container: ")))
  (docker "start" name))

;;;###autoload
(defun docker-compose-stop (name &optional timeout)
  "Stop the container named NAME.

TIMEOUT is the number of seconds to wait for the container to stop before killing it."
  (interactive (list (docker-compose-read-container-name "Stop container: ") current-prefix-arg))
  (docker "stop" (when timeout (format "-t %d" timeout)) name))

;;;###autoload
(defun docker-compose-restart (name &optional timeout)
  "Restart the container named NAME.

TIMEOUT is the number of seconds to wait for the container to stop before killing it."
  (interactive (list (docker-compose-read-container-name "Restart container: ") current-prefix-arg))
  (docker "restart" (when timeout (format "-t %d" timeout)) name))

;;;###autoload
(defun docker-compose-pause (name)
  "Pause the container named NAME."
  (interactive (list (docker-compose-read-container-name "Pause container: ")))
  (docker "pause" name))

;;;###autoload
(defun docker-compose-unpause (name)
  "Unpause the container named NAME."
  (interactive (list (docker-compose-read-container-name "Unpause container: ")))
  (docker "unpause" name))

;;;###autoload
(defun docker-compose-rm (name &optional force link volumes)
  "Remove the container named NAME.

With prefix argument, sets FORCE to true.

Force the removal even if the container is running when FORCE is set.
Remove the specified link and not the underlying container when LINK is set.
Remove the volumes associated with the container when VOLUMES is set."
  (interactive (list (docker-compose-read-container-name "Delete container: ") current-prefix-arg))
  (docker "rm" (when force "-f") (when link "-l") (when volumes "-v") name))

;;;###autoload
(defun docker-compose-kill (name &optional signal)
  "Kill the container named NAME using SIGNAL."
  (interactive (list (docker-compose-read-container-name "Kill container: ")))
  (docker "kill" (when signal (format "-s %s" signal)) name))

;;;###autoload
(defun docker-compose-inspect (name)
  "Inspect the container named NAME."
  (interactive (list (docker-compose-read-container-name "Inspect container: ")))
  (docker "inspect" name))

;;;###autoload
(defun docker-compose-container-find-file (container file)
  (interactive
   (let* ((container-name (docker-compose-read-container-name "container: "))
          (tramp-filename (read-file-name "file: " (format "/docker:%s:/" container-name))))
     (with-parsed-tramp-file-name tramp-filename nil
       (list host localname))))
  (find-file (format "/docker:%s:%s" container file)))

;;;###autoload
(defun docker-compose-container-dired (container directory)
  (interactive
   (let* ((container-name (docker-compose-read-container-name "container: "))
          (tramp-filename (read-directory-name "directory: " (format "/docker:%s:/" container-name))))
     (with-parsed-tramp-file-name tramp-filename nil
       (list host localname))))
  (dired (format "/docker:%s:%s" container directory)))

;;;###autoload
(defun docker-compose-container-shell (container)
  (interactive (list (docker-compose-read-container-name "container: ")))
  (let* ((container-address (format "docker:%s:/" container))
         (file-prefix (if (file-remote-p default-directory)
                          (with-parsed-tramp-file-name default-directory nil
                            (format "/%s:%s|" method host))
                        "/"))
         (default-directory (format "%s%s" file-prefix container-address)))
    (shell (format "*shell %s*" default-directory))))

(defun docker-compose-containers-find-file-selection ()
  "Run `docker-compose-container-find-file' on the containers selection."
  (interactive)
  (--each (docker-compose-utils-get-marked-items-ids)
    (docker-compose-container-find-file it "/")))

(defun docker-compose-containers-shell-selection ()
  "Run `docker-compose-container-shell' on the containers selection."
  (interactive)
  (--each (docker-compose-utils-get-marked-items-ids)
    (docker-compose-container-shell it)))

(defun docker-compose-containers-run-command-on-selection (command arguments)
  "Run a docker COMMAND on the containers selection with ARGUMENTS."
  (interactive "sCommand: \nsArguments: ")
  (--each (docker-compose-utils-get-marked-items-ids)
    (docker command arguments it))
  (tablist-revert))

(defun docker-compose-containers-run-command-on-selection-print (command arguments)
  "Run a docker COMMAND on the containers selection with ARGUMENTS and print"
  (interactive "sCommand: \nsArguments: ")
  (docker-compose-utils-run-command-on-selection-print
   (lambda (id) (docker command arguments id))))

(defmacro docker-compose-containers-create-selection-functions (&rest functions)
  (declare (indent defun) (doc-string 2))
  `(progn ,@(--map
             `(defun ,(intern (format "docker-compose-containers-%s-selection" it)) ()
                ,(format "Run `docker-compose-%s' on the containers selection." it)
                (interactive)
                (docker-compose-containers-run-command-on-selection ,(symbol-name it)
                                                            (s-join " " ,(list (intern (format "docker-compose-containers-%s-arguments" it))))))
             functions)))

;;;###autoload
(defun docker-compose-containers-rename ()
  (interactive)
  (docker-compose-utils-select-if-empty)
  (let ((ids (docker-compose-utils-get-marked-items-ids)))
    (if (/= 1 (length ids))
        (error "Multiple containers cannot be selected.")
      (let ((new-name (read-string "New Name: ")))
        (docker "rename" (nth 0 ids) new-name)
        (tablist-revert)))))

(defalias 'docker-compose-rename-entry 'docker-compose-containers-rename)

(defun docker-compose-containers-cp-from (container-path host-path)
  "Run `docker-compose-cp' on the container to copy files from."
  (interactive "sContainerPath: \nFHostFile: ")
  (docker "cp" (concat (tabulated-list-get-id) ":" container-path) host-path))

(defun docker-compose-containers-cp-to-selection (host-path container-path)
  "Run `docker-compose-cp' on the containers selection to copy file into."
  (interactive "fHostFile: \nsContainerPath: ")
  (--each (docker-compose-utils-get-marked-items-ids)
    (docker "cp" host-path (concat it ":" container-path))))

(defun docker-compose-containers-convert-container-info-to-command (container-info)
  (-map
   (lambda (container-info)
     `("docker" "run"
       ,(assoc-default 'Image container-info)
       ,@(->>
          (assoc-default 'Config container-info)
          (assoc-default 'Env)
          (append)
          (-map
           (lambda (env-cmd) (list "-e" env-cmd)))
          (apply '-concat))
       )) container-info))

(defun docker-compose-containers-inspect-command-selection ()
  (interactive)
  (-each (docker-compose-utils-get-marked-items-ids)
    (lambda (id)
      (let* ((json (docker "inspect" id))
             (parsed (json-read-from-string json))
             (commands
              (docker-compose-containers-convert-container-info-to-command parsed)))
        (docker-compose-utils-with-result-buffer
         (--each commands
           (insert (combine-and-quote-strings it))))))))

(defmacro docker-compose-containers-create-selection-print-functions (&rest functions)
  `(progn ,@(--map
             `(defun ,(intern (format "docker-compose-containers-%s-selection" it)) ()
                ,(format "Run `docker-compose-%s' on the containers selection." it)
                (interactive)
                (docker-compose-containers-run-command-on-selection-print ,(symbol-name it)
                                                                  (s-join " " ,(list (intern (format "docker-compose-containers-%s-arguments" it))))))
             functions)))

(docker-compose-containers-create-selection-functions
  start
  stop
  restart
  pause
  unpause
  rm
  kill)

(docker-compose-containers-create-selection-print-functions inspect logs diff)

(docker-compose-utils-define-popup docker-compose-containers-diff-popup
  "Popup for showing containers diffs."
  'docker-compose-containers-popups
  :man-page "docker-compose-diff"
  :actions  '((?d "Diff" docker-compose-containers-diff-selection)))

(docker-compose-utils-define-popup docker-compose-containers-find-file-popup
  "Popup for opening containers files."
  'docker-compose-containers-popups
  :actions  '((?f "Open file" docker-compose-containers-find-file-selection)))

(docker-compose-utils-define-popup docker-compose-containers-shell-popup
  "Popup for doing M-x `shell' to containers."
  'docker-compose-containers-popups
  :actions  '((?b "Shell" docker-compose-containers-shell-selection)))

(docker-compose-utils-define-popup docker-compose-containers-inspect-popup
  "Popup for inspecting containers."
  'docker-compose-containers-popups
  :man-page "docker-compose-inspect"
  :actions  '((?I "Inspect" docker-compose-containers-inspect-selection)
              (?C "As Command" docker-compose-containers-inspect-command-selection)))

(docker-compose-utils-define-popup docker-compose-containers-logs-popup
  "Popup for showing containers logs."
  'docker-compose-containers-popups
  :man-page "docker-compose-logs"
  :actions  '((?L "Logs" docker-compose-containers-logs-selection)))

(docker-compose-utils-define-popup docker-compose-containers-start-popup
  "Popup for starting containers."
  'docker-compose-containers-popups
  :man-page "docker-compose-start"
  :actions  '((?S "Start" docker-compose-containers-start-selection)))

(docker-compose-utils-define-popup docker-compose-containers-stop-popup
  "Popup for stoping containers."
  'docker-compose-containers-popups
  :man-page "docker-compose-stop"
  :options '((?t "Timeout" "-t "))
  :actions '((?O "Stop" docker-compose-containers-stop-selection)))

(docker-compose-utils-define-popup docker-compose-containers-restart-popup
  "Popup for restarting containers."
  'docker-compose-containers-popups
  :man-page "docker-compose-restart"
  :options '((?t "Timeout" "-t "))
  :actions '((?R "Restart" docker-compose-containers-restart-selection)))

(docker-compose-utils-define-popup docker-compose-containers-pause-popup
  "Popup for pauseing containers."
  'docker-compose-containers-popups
  :man-page "docker-compose-pause"
  :actions  '((?P "Pause" docker-compose-containers-pause-selection)
              (?U "Unpause" docker-compose-containers-unpause-selection)))

(docker-compose-utils-define-popup docker-compose-containers-rm-popup
  "Popup for removing containers."
  'docker-compose-containers-popups
  :man-page "docker-compose-rm"
  :switches '((?f "Force" "-f")
              (?v "Volumes" "-v"))
  :actions  '((?D "Remove" docker-compose-containers-rm-selection)))

(docker-compose-utils-define-popup docker-compose-containers-kill-popup
  "Popup for kill signaling containers"
  'docker-compose-containers-popups
  :man-page "docker-compose-kill"
  :options  '((?s "Signal" "-s "))
  :actions  '((?K "Kill" docker-compose-containers-kill-selection)))

(docker-compose-utils-define-popup docker-compose-containers-cp-popup
  "Popup for copying files from/to containers."
  'docker-compose-containers-popups
  :man-page "docker-compose-cp"
  :actions  '((?F "Copy From" docker-compose-containers-cp-from)
              (?T "Copy To" docker-compose-containers-cp-to-selection)))

(defun docker-compose-containers-refresh ()
  "Refresh the containers list."
  (setq tabulated-list-entries (docker-compose-containers-entries)))

(defvar docker-compose-containers-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'docker-compose-containers-diff-popup)
    (define-key map "f" 'docker-compose-containers-find-file-popup)
    (define-key map "b" 'docker-compose-containers-shell-popup)
    (define-key map "C" 'docker-compose-containers-cp-popup)
    (define-key map "I" 'docker-compose-containers-inspect-popup)
    (define-key map "K" 'docker-compose-containers-kill-popup)
    (define-key map "L" 'docker-compose-containers-logs-popup)
    (define-key map "S" 'docker-compose-containers-start-popup)
    (define-key map "O" 'docker-compose-containers-stop-popup)
    (define-key map "R" 'docker-compose-containers-restart-popup)
    (define-key map "P" 'docker-compose-containers-pause-popup)
    (define-key map "D" 'docker-compose-containers-rm-popup)
    (define-key map "r" 'docker-compose-containers-rename)
    map)
  "Keymap for `docker-compose-containers-mode'.")



(provide 'docker-compose-containers)

;;; docker-compose-containers.el ends here
