;;; ivy-springboard.el --- Temporarily change default-directory for one command

;; Copyright (C) 2012 John Wiegley

;; Author: John Wiegley <jwiegley@gmail.com>
;; Created: 13 Jun 2012
;; Version: 1.0
;; Keywords: ivy
;; X-URL: https://github.com/jwiegley/springboard

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; How many times have you wanted to fire off a quick command, such as M-!,
;; but the directory you want to run the command in isn't the same as the
;; directory of the current buffer?  In those situations, you want a quick way
;; to change the default-directory *for only the next command*.  That is what
;; Ivy-Springboard aims to solve.
;;
;; It overrides command keys in ivy-mode so that they use the
;; default-directory of the current viable completion, rather than the
;; default-directory of the buffer where you started the ivy command.

(require 'ivy)

(defgroup ivy-springboard nil
  "Change default-directory for commands invoked at `ivy-switch-buffer'."
  :group 'ivy)

(defcustom ivy-springboard-ignored-commands
  '(self-insert-command
    delete-backward-char
    abort-recursive-edit
    exit-minibuffer
    switch-to-buffer
    backward-char
    forward-char
    kill-line
    move-beginning-of-line
    move-end-of-line
    backward-kill-word
    forward-kill-word)
  "Commands that will not be trapped by Ivy-Springboard."
  :type '(repeat command)
  :group 'ivy-springboard)

(eval-when-compile
  (defvar ivy-springboard-trapped nil))

(defun ivy-springboard-match-directory ()
  (let ((item (or (let ((buf (get-buffer (car ivy--all-candidates ;; ido-matches
                                              ))))
                    (and buf
                         (with-current-buffer buf
                           default-directory)))
                  (and ivy-use-virtual-buffers ivy--virtual-buffers
                       (cdr (assoc (car ivy--all-candidates)
                                   ivy--virtual-buffers))))))
    (cond ((file-directory-p item)
           item)
          ((file-exists-p item)
           (file-name-directory item))
          (t
           nil))))

(defun ivy-springboard-add-trap ()
  (add-hook 'pre-command-hook 'ivy-springboard-trap-command t t))

(defun ivy-springboard-remove-trap ()
  (remove-hook 'pre-command-hook 'ivy-springboard-trap-command t))

(defun ivy-springboard-trap-command ()
  (unless ivy-springboard-trapped
    (condition-case err
        (unless (or (memq this-command ivy-springboard-ignored-commands)
                    (string-match "\\`ivy-" (symbol-name this-command)))
          (let ((dir (ivy-springboard-match-directory)))
            (when dir
              ;; (message "Trapped command: %s" this-command)
              (loop for buf in (buffer-list)
                    when (minibufferp buf)
                    do (with-current-buffer buf
                         (ivy-springboard-remove-trap)))
              (setq ivy-springboard-trapped t)
              (throw 'abort dir))))
      (error
       (message "Error occurred: %s" err)))))

;;;###autoload
(defadvice ivy-switch-buffer (around ivy-springboard-ivy-switch-buffer activate)
  "Adds ability to set `default-directory' for commands at ivy minibuffer."
  (interactive)
  (add-hook 'minibuffer-setup-hook 'ivy-springboard-add-trap)
  (add-hook 'minibuffer-exit-hook 'ivy-springboard-remove-trap)
  (unwind-protect
      (let* (ivy-springboard-trapped
             ivy-springboard-already-trapped
             (default-directory (catch 'abort (ignore ad-do-it))))
        (if default-directory
            (call-interactively this-command)))
    (remove-hook 'minibuffer-setup-hook 'ivy-springboard-add-trap)
    (remove-hook 'minibuffer-exit-hook 'ivy-springboard-remove-trap)))

(provide 'ivy-springboard)

;;; ivy-springboard.el ends here
