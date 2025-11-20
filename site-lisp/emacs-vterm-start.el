(require 'subr-x)
(require 'tramp nil t)

;; Default visual setup for Emacs VTerm.app
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(load-theme 'modus-operandi t)

(defgroup emacs-vterm nil
  "Customization group for the homebrew Emacs vterm helpers."
  :group 'term)

(defcustom emacs-vterm-reserved-buffer-names '("*vterm*" "vterm")
  "Buffer names that should never be auto-renamed by emacs-vterm."
  :type '(repeat string)
  :group 'emacs-vterm)

(defvar-local emacs-vterm--last-buffer-name nil)
(defvar emacs-vterm--load-error-announced nil
  "Non-nil once we have informed the user that bundled vterm failed to load.")

(defun emacs-vterm--report-bundled-vterm-error ()
  "Emit (at most once) the bundled vterm load failure message."
  (unless emacs-vterm--load-error-announced
    (setq emacs-vterm--load-error-announced t)
    (message "emacs-vterm: bundled vterm could not be loaded. Try `brew reinstall emacs-vterm`.")))

(defun emacs-vterm--rename-eligible-p (&optional buffer)
  "Return non-nil when BUFFER should be auto-renamed."
  (with-current-buffer (or buffer (current-buffer))
    (and (derived-mode-p 'vterm-mode)
         (not (member (buffer-name) emacs-vterm-reserved-buffer-names)))))

(defun emacs-vterm--current-directory (&optional buffer)
  "Return the best effort directory string for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (cond
     ((and (fboundp 'vterm--get-pwd) (bound-and-true-p vterm--term))
      (or (ignore-errors (vterm--get-pwd)) default-directory))
     (t default-directory))))

(defun emacs-vterm--abbreviate-path (path remote)
  "Return a readable variant of PATH. REMOTE indicates remote paths."
  (let ((clean (directory-file-name (or path ""))))
    (cond
     ((string-empty-p clean) "/")
     ((and remote clean) clean)
     ((fboundp 'abbreviate-file-name)
      (abbreviate-file-name clean))
     (t clean))))

(defun emacs-vterm--short-hostname (host)
  "Return HOST without domain segments."
  (when host
    (car (split-string host "\\."))))

(defun emacs-vterm--format-host (host &optional port)
  "Format HOST (optionally PORT) for display while stripping domain noise."
  (let* ((name (or (emacs-vterm--short-hostname host) host))
         (port-str (cond
                    ((integerp port) (number-to-string port))
                    ((and (stringp port)
                          (string-match-p "\\`[0-9]+\\'" port))
                     port)
                    (t nil))))
    (if (and port-str (not (string-empty-p port-str)))
        (format "%s#%s" name port-str)
      name)))

(defun emacs-vterm--format-buffer-name (&optional buffer)
  "Build the desired buffer name for BUFFER based on directory/host."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((dir (emacs-vterm--current-directory))
           (remote-host (and dir (file-remote-p dir 'host)))
           (remote-port (and dir (file-remote-p dir 'port)))
           (remote-path (and dir (file-remote-p dir 'localname)))
           (path (emacs-vterm--abbreviate-path (or remote-path dir) remote-host))
           (host (or (and remote-host (emacs-vterm--format-host remote-host remote-port))
                     (emacs-vterm--format-host (and (fboundp 'system-name) (system-name)))
                     "localhost")))
      (when (and path host)
        (format "vterm: %s@%s" path host)))))

(defun emacs-vterm--generate-buffer-name ()
  "Generate a new buffer name using our dynamic formatter."
  (generate-new-buffer-name
   (or (emacs-vterm--format-buffer-name) "vterm")))

(defun emacs-vterm--refresh-buffer-name (&rest _)
  "Rename the current vterm buffer unless it is reserved."
  (when (emacs-vterm--rename-eligible-p)
    (when-let ((new-name (emacs-vterm--format-buffer-name)))
      (unless (equal new-name emacs-vterm--last-buffer-name)
        (setq emacs-vterm--last-buffer-name new-name)
        (rename-buffer new-name t)))))

(defun emacs-vterm--setup-buffer ()
  "Initialize dynamic buffer naming for the current vterm."
  (setq emacs-vterm--last-buffer-name nil)
  (emacs-vterm--refresh-buffer-name))

(defun emacs-vterm--local-default-directory ()
  "Return a local directory path suitable for launching VTerm."
  (let ((dir default-directory))
    (or (and dir (not (file-remote-p dir)) dir)
        (when-let ((home (getenv "HOME")))
          (file-name-as-directory home))
        (and (boundp 'user-emacs-directory) user-emacs-directory)
        dir)))

(defun emacs-vterm--start-vterm-buffer ()
  "Attempt to start the bundled vterm and return the new buffer, or nil."
  (let ((default-directory (or (emacs-vterm--local-default-directory)
                               default-directory)))
    (condition-case nil
        (if (and (require 'vterm nil 'noerror)
                 (fboundp 'vterm))
            (vterm (emacs-vterm--generate-buffer-name))
          (emacs-vterm--report-bundled-vterm-error)
          nil)
      (error
       (emacs-vterm--report-bundled-vterm-error)
       nil))))

(defun emacs-vterm--frame-supports-vterm-p (&optional frame)
  "Return non-nil when FRAME can host a GUI VTerm."
  (let ((frame (or frame (selected-frame))))
    (and (display-graphic-p frame)
         (not noninteractive))))

(defun emacs-vterm--initial-buffer ()
  "Return the buffer that should open on GUI launches."
  (if (emacs-vterm--frame-supports-vterm-p)
      (or (emacs-vterm--start-vterm-buffer)
          (get-buffer-create "*scratch*"))
    (get-buffer-create "*scratch*")))

(defun emacs-vterm--start-vterm-on-frame (frame)
  "Open a VTerm buffer in FRAME when suitable."
  (when (emacs-vterm--frame-supports-vterm-p frame)
    (with-selected-frame frame
      (emacs-vterm--start-vterm-buffer))))

(defun emacs-vterm--populate-split-with-vterm (orig-fn &rest args)
  "Advice to open a fresh vterm in the new split when run interactively.
ORIG-FN is the original split function and ARGS are its arguments."
  (let ((new-window (apply orig-fn args)))
    (when (and (called-interactively-p 'interactive)
               (window-live-p new-window)
               (emacs-vterm--frame-supports-vterm-p (window-frame new-window)))
      (with-selected-window new-window
        (emacs-vterm--start-vterm-buffer)))
    new-window))

(with-eval-after-load 'vterm
  (add-hook 'vterm-mode-hook #'emacs-vterm--setup-buffer)
  (advice-add 'vterm--set-directory :after #'emacs-vterm--refresh-buffer-name))

;; Auto-start vterm for GUI, no-args launches, while respecting user config
(when (and (not noninteractive)
           (null command-line-args-left)
           (not (bound-and-true-p initial-buffer-choice)))
  (setq initial-buffer-choice #'emacs-vterm--initial-buffer))

;; Also open vterm on newly created GUI frames
(add-hook 'after-make-frame-functions #'emacs-vterm--start-vterm-on-frame)

;; After splitting (C-x 2 / C-x 3), show a new vterm in the created window
(advice-add 'split-window-below :around #'emacs-vterm--populate-split-with-vterm)
(advice-add 'split-window-right :around #'emacs-vterm--populate-split-with-vterm)
