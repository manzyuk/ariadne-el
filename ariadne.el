(require 'bert)
(require 'bindat)

(defvar ariadne-process nil
  "Process object representing a network connection to Ariadne.")

(defun ariadne-connect ()
  "Connect to the Ariadne server."
  (let ((process
         (make-network-process
          :name     "ariadne"
          :host     "localhost"
          :service  39014
          :buffer   "*ariadne*"
          :filter   'ariadne-filter
          :sentinel 'ariadne-sentinel)))
    (with-current-buffer (process-buffer process)
      (set-buffer-multibyte nil))
    ;; Delete the process without querying if the process buffer is
    ;; killed.
    (set-process-query-on-exit-flag process nil)
    (setq ariadne-process process)))

(defun ariadne-close (process)
  (setq ariadne-process nil)
  (kill-buffer (process-buffer process)))

(defun ariadne-filter (process string)
  "Accept output from the socket and process all complete
messages."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string))
  (ariadne-process-available-output process))

(defun ariadne-sentinel (process message)
  (message "Ariadne connection closed unexpectedly: %s" message)
  (ariadne-close process))

(defun ariadne-process-available-output (process)
  "Process all complete messages that have arrived from Ariadne."
  (with-current-buffer (process-buffer process)
    (while (ariadne-have-input-p)
      (let ((event (ariadne-read-or-lose process))
            (ok nil))
        (unwind-protect
            (save-current-buffer
              (ariadne-dispatch-event event process)
              (setq ok t))
          (unless ok
            (ariadne-run-when-idle
             'ariadne-process-available-output process)))))))

(defun ariadne-have-input-p ()
  "Return T if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 4)
       (>= (- (buffer-size) 4) (ariadne-decode-length))))

(defun ariadne-run-when-idle (function &rest args)
  "Call FUNCTION as soon as Emacs is idle."
  (apply #'run-at-time 0 nil function args))

(defun ariadne-read-or-lose (process)
  (condition-case error
      (ariadne-read)
    (error
     (ariadne-close process)
     (error "ariadne-read: %S" error))))

(defun ariadne-read ()
  "Read a message from the Ariadne buffer."
  (goto-char (point-min))
  (let* ((length (ariadne-decode-length))
         (start (+ (point) 4))
         (end (+ start length)))
    (prog1 (bert-unpack (buffer-substring-no-properties start end))
      (delete-region (point-min) end))))

(defun ariadne-encode-length (length)
  (bindat-pack '((length u32)) `((length . ,length))))

(defun ariadne-decode-length ()
  (bindat-get-field
   (bindat-unpack
    '((length u32))
    (buffer-substring-no-properties (point) (+ (point) 4)))
   'length))

(defun ariadne-dispatch-event (event process)
  (case (aref event 0)
    (reply (ariadne-handle-reply (aref event 1)))
    (error (error "BERT-RPC error: %s" (aref (aref event 1) 3)))))

(defun ariadne-handle-reply (reply)
  (case (aref reply 0)
    (no_name
     (message "No recognized name at point."))
    (loc_known
     (ariadne-goto (aref reply 1) (aref reply 2) (aref reply 3)))
    (loc_unknown
     (message "The name at point is defined in %s" (aref reply 1)))
    (error
     (message "ariadne error: %s" (aref reply 1)))))

(defun ariadne-goto (filename line column)
  "Go to a given position in a given file."
  (find-file filename)
  (widen)
  (goto-char (point-min))
  (forward-line (1- line))
  (forward-char (1- column)))

(defun ariadne-send (obj process)
  "Send OBJ to Ariadne over the socket PROCESS."
  (let* ((bert (bert-pack obj))
         (berp (concat (ariadne-encode-length (length bert)) bert)))
    (process-send-string process berp)))

(defun ariadne-current-line ()
  "Return the vertical position of point."
  (save-restriction
    (widen)
    (line-number-at-pos)))

(defun ariadne-goto-definition ()
  "Go to the definition of a name at point."
  (interactive)
  (let ((file-name (buffer-file-name))
        (line-number (ariadne-current-line))
        (column-number (current-column)))
    (when file-name
      (unless ariadne-process (ariadne-connect))
      (push-mark (point))
      (ariadne-send
       (vector 'call 'ariadne 'find
               (list file-name line-number column-number))
       ariadne-process))))
