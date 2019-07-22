;;; lsp-python-ms.el --- lsp-mode client for Microsoft python-language-server -*- lexical-binding: t -*-

;; Author: Charl Botha
;; Maintainer: Andrew Christianson
;; Version: 0.2.0
;; Package-Requires: ((cl-lib "0.6.1") (lsp-mode "6.0") (python "0.26.1") (json "1.4") (emacs "24.4"))
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
(require 'find-file-in-project nil 'noerror)

;; forward declare variable
(defvar lsp-render-markdown-markup-content)

(defvar lsp-python-ms-dir (expand-file-name "mspyls/" user-emacs-directory)
  "Path to language server directory.

This is the directory containing Microsoft.Python.LanguageServer.dll.")

;; not used since ms-pyls 0.2.92+
;; see https://github.com/microsoft/vscode-python/blob/master/src/client/activation/languageServer/analysisOptions.ts#L93
;; (defvar lsp-python-ms-cache-dir
;;   (directory-file-name (locate-user-emacs-file ".lsp-python/"))
;;   "Path to directory where the server will write cache files.

;; If this is nil, the language server will write cache files in a directory
;; sibling to the root of every project you visit")

(defvar lsp-python-ms-extra-paths '()
  "A list of additional paths to search for python packages.

This should be a list of paths corresponding to additional python
library directories you want to search for completions.  Paths
should be as they are (or would appear) in sys.path.  Paths will
be prepended to the search path, and so will shadow duplicate
names in search paths returned by the interpreter.")

(defvar lsp-python-executable-cmd "python"
  "Command to specify the python command for ms-pyls.

Similar to the `python-shell-interpreter', but used only with `ms-pyls'.
Useful when there are multiple python versions in system.
e.g, there are `python2' and `python3', both in system PATH,
and the default `python' links to python2,
set as `python3' to let ms-pyls use python 3 environments.")

(defvar lsp-python-ms-executable (concat lsp-python-ms-dir
                                         "Microsoft.Python.LanguageServer"
                                         (and (eq system-type 'windows-nt) ".exe"))
  "Path to Microsoft.Python.LanguageServer.exe.")

(defvar lsp-python-ms-nupkg-channel "stable"
  "The channel of nupkg for Microsoft Python Language Server:
stable, beta or daily.")

(defun lsp-python-ms-latest-nupkg-url (&optional channel)
  "Get the nupkg url of the latest Microsoft Python Language Server."
  (let ((channel (or channel "stable")))
    (unless (member channel '("stable" "beta" "daily"))
      (error (format "Unknown channel: %s" channel)))
    (with-current-buffer
        (url-retrieve-synchronously
         (format "https://pvsc.blob.core.windows.net/python-language-server-%s\
?restype=container&comp=list&prefix=Python-Language-Server-%s-x64"
                 channel
                 (cond ((eq system-type 'darwin)  "osx")
                       ((eq system-type 'gnu/linux) "linux")
                       ((eq system-type 'windows-nt) "win")
                       (t (error (format "Unsupported system: %s" system-type))))))
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (pcase (xml-parse-region (point) (point-max))
        (`((EnumerationResults
            ((ContainerName . ,_))
            (Prefix nil ,_)
            (Blobs nil . ,blobs)
            (NextMarker nil)))
         (cdar
          (sort
           (mapcar (lambda (blob)
                     (pcase blob
                       (`(Blob
                          nil
                          (Name nil ,_)
                          (Url nil ,url)
                          (Properties nil (Last-Modified nil ,last-modified) . ,_))
                        (cons (apply #'encode-time (parse-time-string last-modified)) url))))
                   blobs)
           (lambda (t1 t2)
             (time-less-p (car t2) (car t1))))))))))

(defun lsp-python-ms-setup (&optional forced)
  "Downloading Microsoft Python Language Server to path specified.
With prefix, FORCED to redownload the server."
  (interactive "P")
  (unless (and (not forced)
               (executable-find lsp-python-ms-executable))
    (let ((temp-file (make-temp-file "mspyls" nil ".zip"))
          (unzip-script (cond ((executable-find "powershell")
                               "powershell -noprofile -noninteractive \
-nologo -ex bypass Expand-Archive -path '%s' -dest '%s'")
                              ((executable-find "unzip")
                               "bash -c 'mkdir -p %2$s && unzip -qq %1$s -d %2$s'")
                              (t (error "Unable to unzip! You may need to install the `unzip` executable.")))))
      (message "Downloading Microsoft Python Language Server...")

      (url-copy-file (lsp-python-ms-latest-nupkg-url lsp-python-ms-nupkg-channel)
                     temp-file 'overwrite)
      (when (file-exists-p lsp-python-ms-dir)
        (delete-directory lsp-python-ms-dir 'recursive))
      (shell-command (format unzip-script temp-file lsp-python-ms-dir))
      (when (file-exists-p lsp-python-ms-executable)
        (chmod lsp-python-ms-executable #o755))

      (message "Downloaded Microsoft Python Language Server!"))))

(defun lsp-python-ms-update-server ()
  "Update Microsoft Python Language Server.

On Windows, if the server is running, the updating will fail.
After stopping or killing the process, retry to update."
  (interactive)
  (message "Server update started...")
  (lsp-python-ms-setup t)
  (message "Server update finished..."))

;; it's crucial that we send the correct Python version to MS PYLS,
;; else it returns no docs in many cases furthermore, we send the
;; current Python's (can be virtualenv) sys.path as searchPaths
(defun lsp-python-ms--get-python-ver-and-syspath (workspace-root)
  "Return list with pyver-string and list of python search paths.

The WORKSPACE-ROOT will be prepended to the list of python search
paths and then the entire list will be json-encoded."
  (let ((python (executable-find lsp-python-executable-cmd))
        (init "from __future__ import print_function; import sys; import json;")
        (ver "print(\"%s.%s\" % (sys.version_info[0], sys.version_info[1]));")
        (sp (concat "sys.path.insert(0, '" workspace-root "'); print(json.dumps(sys.path));"))
        (ex "print(sys.executable)"))
    (with-temp-buffer
      (call-process python nil t nil "-c" (concat init ver sp ex))
      (cl-subseq (split-string (buffer-string) "\n") 0 3))))

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
    (cl-destructuring-bind (pyver _pysyspath _pyintpath)
        (lsp-python-ms--get-python-ver-and-syspath workspace-root)
      `(:interpreter
        (:properties (:InterpreterPath
                      ,_pyintpath
                      ;; this database dir will be created if required
                      ;; :DatabasePath ,(expand-file-name (directory-file-name lsp-python-ms-cache-dir))
                      :Version ,pyver))
        ;; preferredFormat "markdown" or "plaintext"
        ;; experiment to find what works best -- over here mostly plaintext
        :displayOptions (
                         :preferredFormat "markdown"
                         :trimDocumentationLines :json-false
                         :maxDocumentationLineLength 0
                         :trimDocumentationText :json-false
                         :maxDocumentationTextLength 0)
        :searchPaths ,(if lsp-python-ms-extra-paths
                          (vconcat lsp-python-ms-extra-paths nil)
                        [])
        :analysisUpdates t
        :asyncStartup t
        :typeStubSearchPaths ,(vector (concat lsp-python-ms-dir "Typeshed"))))))

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
  ;; Try to download server if it doesn't exists
  (unless (executable-find lsp-python-ms-executable)
    (lsp-python-ms-setup))

  (if (executable-find lsp-python-ms-executable)
      lsp-python-ms-executable
    (error (concat "Cannot find Microsoft Python Language Server executable! It's expected to be "
                   lsp-python-ms-executable))))

(defgroup lsp-mspyls nil
  "LSP support for Python, using Microsoft Python Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/emacs-lsp/lsp-python-ms"))

;; See https://github.com/microsoft/python-language-server for more diagnostics
(defcustom lsp-mspyls-errors ["unknown-parameter-name"
                              "undefined-variable"
                              "parameter-missing"
                              "positional-argument-after-keyword"
                              "too-many-function-arguments"]
  "Microsoft Python LSP Error types."
  :group 'lsp-mspyls
  :type 'vector)

(defcustom lsp-mspyls-warnings ["unresolved-import"
                                "parameter-already-specified"
                                "too-many-positional-arguments-before-star"]
  "Microsoft Python LSP Warning types."
  :group 'lsp-mspyls
  :type 'vector)

(lsp-register-custom-settings '(("python.analysis.errors" lsp-mspyls-errors)))
(lsp-register-custom-settings '(("python.analysis.warnings" lsp-mspyls-warnings)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection 'lsp-python-ms--command-string)
  :major-modes '(python-mode)
  :server-id 'mspyls
  :priority 1
  :initialization-options 'lsp-python-ms--extra-init-params
  :notification-handlers (lsp-ht ("python/languageServerStarted" 'lsp-python-ms--language-server-started-callback)
                                 ("telemetry/event" 'ignore)
                                 ;; TODO handle this more gracefully
                                 ("python/reportProgress" 'ignore)
                                 ("python/beginProgress" 'ignore)
                                 ("python/endProgress" 'ignore))
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration (lsp-configuration-section "python"))))))

(provide 'lsp-python-ms)

;;; lsp-python-ms.el ends here
