;;; Copyright (c) 2014, Rolando Pereira
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(in-package :vlc)


(ql:quickload 'telnetlib)
(ql:quickload 'split-sequence)


(defstruct videolan-connection (host "localhost") (port 4212) (timeout 10) (password "admin") debug telnet)

(defun login (videolan-instance)
  (let ((host (videolan-connection-host videolan-instance))
         (port (videolan-connection-port videolan-instance))
         (timeout (videolan-connection-timeout videolan-instance))
         (password (videolan-connection-password videolan-instance))
         (debug (videolan-connection-debug videolan-instance))
         (telnet (videolan-connection-telnet videolan-instance)))
    (unless telnet
      (setf (videolan-connection-telnet videolan-instance) (telnetlib:open-telnet-session host port))
      (setf telnet (videolan-connection-telnet videolan-instance)))
    ;; Perform the initial login
    (telnetlib:read-until telnet "Password:" :timeout timeout :case-insensitive-mode t)
    (telnetlib:write-ln telnet password)
    (telnetlib:read-until telnet "> " :timeout timeout :case-insensitive-mode t)))

(defun logout (videolan-instance)
  (telnetlib:write-ln (videolan-connection-telnet videolan-instance) "exit")
  (telnetlib:close-telnet-session (videolan-connection-telnet videolan-instance))
  (setf (videolan-connection-telnet videolan-instance) nil))

(defun shutdown (videolan-instance)
  (telnetlib:write-ln (videolan-connection-telnet videolan-instance) "shutdown")
  (telnetlib:close-telnet-session (videolan-connection-telnet videolan-instance))
  (setf (videolan-connection-telnet videolan-instance) nil))

(defun cmd (videolan-instance cmd)
  (let ((tn (videolan-connection-telnet videolan-instance)))
    (telnetlib:write-ln tn cmd)
    (remove-if (lambda (line) (string= line "> "))
      (split-sequence:split-sequence #\Newline 
        (telnetlib:read-until tn "> ")
        :test #'string=))))

(let ((vl (make-videolan-connection)))
  (login vl)
  (cmd vl "get_time"))


