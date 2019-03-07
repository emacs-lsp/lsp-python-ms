;;; lsp-python-ms.el --- lsp-mode client for Microsoft python-language-server -*- lexical-binding: t -*-

;; Author: Charl Botha
;; Maintainer: Andrew Christianson
;; Version: 0.1.0
;; Package-Requires: (cl-lib lsp-mode python json (emacs "24.4"))
;; Homepage: https://github.com/andrew-christianson/lsp-python-ms
;; Keywords: languages tools


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; from https://vxlabs.com/2018/11/19/configuring-emacs-lsp-mode-and-microsofts-visual-studio-code-python-language-server/

;;; Code:
(require 'cl-lib)
(require 'lsp-mode)
(require 'python)
(require 'json)
(require 'projectile nil 'noerror)

;; forward declare variable
(defvar lsp-render-markdown-markup-content)

(defvar lsp-python-ms-dir nil
  "Path to language server directory.

This is the directory containing Microsoft.Python.LanguageServer.dll.")

(defvar lsp-python-ms-cache-dir
  (directory-file-name (locate-user-emacs-file ".lsp-python/"))
  "Path to directory where the server will write cache files.

If this is nil, the language server will write cache files in a directory
sibling to the root of every project you visit")

(defun lsp-python-ms--find-dotnet ()
  "Get the path to dotnet, or return `lsp-python-ms-dotnet'."
  (cond
   ((boundp 'lsp-python-ms-dotnet) lsp-python-ms-dotnet)
   ((executable-find "dotnet"))
   ((eq system-type 'windows-nt) "dotnet")
   (t nil)))

(defvar lsp-python-ms-dotnet
  (lsp-python-ms--find-dotnet)
  "Full path to dotnet executable.

You only need to set this if dotnet is not on your path.")

(defun lsp-python-ms--find-server-executable ()
  "Get path to the python language server executable."
  (cond
   ((boundp 'lsp-python-ms-executable) lsp-python-ms-executable)
   ((executable-find "Microsoft.Python.LanguageServer"))
   ((executable-find "Microsoft.Python.LanguageServer.LanguageServer"))
   ((executable-find "Microsoft.Python.LanguageServer.exe"))
   (t nil)))

(defvar lsp-python-ms-executable
  (lsp-python-ms--find-server-executable)
  "Path to Microsoft.Python.LanguageServer.exe.")

;; it's crucial that we send the correct Python version to MS PYLS,
;; else it returns no docs in many cases furthermore, we send the
;; current Python's (can be virtualenv) sys.path as searchPaths
(defun lsp-python-ms--get-python-ver-and-syspath (workspace-root)
  "Return list with pyver-string and list of python search paths.

The WORKSPACE-ROOT will be prepended to the list of python search
paths and then the entire list will be json-encoded."
  (let ((python (executable-find "python"))
        (init "from __future__ import print_function; import sys; import json;")
        (ver "print(\"%s.%s\" % (sys.version_info[0], sys.version_info[1]));")
        (sp (concat "sys.path.insert(0, '" workspace-root "'); print(json.dumps(sys.path))")))
    (with-temp-buffer
      (call-process python nil t nil "-c" (concat init ver sp))
      (cl-subseq (split-string (buffer-string) "\n") 0 2))))

(defun lsp-python-ms--workspace-root ()
  "Get the path of the root of the current workspace.

Use `lsp-workspace-root', which is pressent in the \"new\"
lsp-mode and works when there's an active session.  Next try ffip
or projectile, or just return `default-directory'."
  (cond
   ((and (fboundp #'lsp-workspace-root) (lsp-workspace-root)))
   ((fboundp #'ffip-get-project-root-directory) (ffip-get-project-root-directory))
   ((fboundp #'projectile-project-root)) (projectile-project-root)
   (t default-directory)))

;; I based most of this on the vs.code implementation:
;; https://github.com/Microsoft/vscode-python/blob/master/src/client/activation/languageServer/languageServer.ts#L219
;; (it still took quite a while to get right, but here we are!)
(defun lsp-python-ms--extra-init-params (&optional workspace)
  "Return form describing parameters for language server.

Old lsp will pass in a WORKSPACE, new lsp has a global
lsp-workspace-root function that finds the current buffer's
workspace root.  If nothing works, default to the current file's
directory"
  (let ((workspace-root (if workspace (lsp--workspace-root workspace) (lsp-python-ms--workspace-root))))
    (cl-destructuring-bind (pyver pysyspath)
        (lsp-python-ms--get-python-ver-and-syspath workspace-root)
      `(:interpreter
        (:properties (:InterpreterPath
                      ,(executable-find "python")
                      ;; this database dir will be created if required
                      :DatabasePath ,(expand-file-name (directory-file-name lsp-python-ms-cache-dir))
                      :Version ,pyver))
        ;; preferredFormat "markdown" or "plaintext"
        ;; experiment to find what works best -- over here mostly plaintext
        :displayOptions (
                         :preferredFormat "plaintext"
                         :trimDocumentationLines :json-false
                         :maxDocumentationLineLength 0
                         :trimDocumentationText :json-false
                         :maxDocumentationTextLength 0)
        :searchPaths ,(json-read-from-string pysyspath)))))


(defun lsp-python-ms--filter-nbsp (str)
  "Filter nbsp entities from STR."
  (let ((rx "&nbsp;"))
    (when (eq system-type 'windows-nt)
      (setq rx (concat rx "\\|\r")))
    (when str
      (replace-regexp-in-string rx " " str))))

(defun lsp-python-ms--language-server-started-callback (workspace _params)
  "Handle the python/languageServerStarted message.

WORKSPACE is just used for logging and _PARAMS is unused."
  (lsp-workspace-status "::Started" workspace)
  (message "Python language server started"))

(defun lsp-python-ms--client-initialized (client)
  "Callback to register and configure client after it's initialized.

After CLIENT is initialized, this function is called to configure
other handlers. "
  (lsp-client-on-notification client "python/languageServerStarted"
                              #'lsp-python-ms--language-server-started-callback)
  (lsp-client-on-notification client "telemetry/event" #'ignore))

;; this gets called when we do lsp-describe-thing-at-point
;; see lsp-methods.el. As always, remove Microsoft's unwanted entities :(
(setq lsp-render-markdown-markup-content #'lsp-python-ms--filter-nbsp)

;; lsp-ui-doc--extract gets called when hover docs are requested
;; as always, we have to remove Microsoft's unnecessary &nbsp; entities
(advice-add 'lsp-ui-doc--extract
            :filter-return #'lsp-python-ms--filter-nbsp)

;; lsp-ui-sideline--format-info gets called when lsp-ui wants to show
;; hover info in the sideline again &nbsp; has to be removed
(advice-add 'lsp-ui-sideline--format-info
            :filter-return #'lsp-python-ms--filter-nbsp)

(defun lsp-python-ms--command-string ()
  "Return the command to start the server."
  (cond
   ((lsp-python-ms--find-server-executable))
   ((and (lsp-python-ms--find-dotnet) lsp-python-ms-dir)
    (list (lsp-python-ms--find-dotnet)
          (concat lsp-python-ms-dir "Microsoft.Python.LanguageServer.dll")))
   (t (error "Could find Microsoft python language server"))))

(if (fboundp 'lsp-register-client)
    ;; New lsp-mode
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection 'lsp-python-ms--command-string)
      :major-modes '(python-mode)
      :server-id 'mspyls
      :priority 1
      :initialization-options 'lsp-python-ms--extra-init-params
      :notification-handlers (lsp-ht ("python/languageServerStarted"
                                      'lsp-python-ms--language-server-started-callback)
                                     ("telemetry/event" 'ignore)
                                     ;; TODO handle this more gracefully
                                     ("python/reportProgress" 'ignore)
                                     ("python/beginProgress" 'ignore)
                                     ("python/endProgress" 'ignore))))
  ;; Old lsp-mode
  (lsp-define-stdio-client
   lsp-python "python"
   #'lsp-python-ms--workspace-root
   nil
   :command-fn 'lsp-python-ms--command-string
   :extra-init-params #'lsp-python-ms--extra-init-params
   :initialize #'lsp-python-ms--client-initialized))

(provide 'lsp-python-ms)

;;; lsp-python-ms.el ends here
