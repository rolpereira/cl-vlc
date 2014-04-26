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
(ql:quickload 'alexandria)





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





(defun cmd (videolan-instance cmd)
  (let ((tn (videolan-connection-telnet videolan-instance)))
    (telnetlib:write-ln tn cmd)
    (remove-if (lambda (line) (string= line "> "))
      (split-sequence:split-sequence #\Newline
        (telnetlib:read-until tn "> ")
        :test #'string=))))



(defun optional-argument-p (argument-list)
  (let ((string (symbol-name argument-list)))
    (and (alexandria:starts-with #\[ string :test #'string=)
      (alexandria:ends-with #\] string ))))

(defun name-of-optional-argument (argument-list)
  (assert (optional-argument-p argument-list) (argument-list) "~a is not optional argument list" argument-list)
  (let ((string (symbol-name argument-list)))
    (subseq string 1 (1- (length string)))))

(defun name-of-optional-argument-or-self (argument)
  (if (optional-argument-p argument)
    (name-of-optional-argument argument)
    argument))


(defun convert-to-vlc-command (symbol)
  (assert (alexandria:starts-with-subseq "VLC-CMD-" (symbol-name symbol))
    (symbol) "~a is not a vlc command" symbol)
  (let ((vlc-command (subseq (symbol-name symbol) (length "VLC-CMD-"))))
    (string-downcase vlc-command)))




(defmacro defcommand (name &rest rest)
  (if (alexandria:starts-with '[on rest)
    `(defcommand-with-on-or-off ,name ,@rest)
    `(defcommand-normal ,name ,@rest)))




(defmacro defcommand-normal (name &rest rest)
  (let* ((command-name (intern (format nil "VLC-CMD-~s" name)))
         (command-args (butlast rest))
         (has-optional-arguments (some #'optional-argument-p command-args))
         (docstring (alexandria:lastcar rest)))
    ;; Add the &optional symbol to `command-args' if the vlc command contains an optional argument
    (when has-optional-arguments
      (push '&optional command-args)
      (setf command-args (mapcar (lambda (arg)
                                   (if (optional-argument-p arg)
                                     (intern (name-of-optional-argument arg))
                                     arg))
                           command-args)))
    `(defun ,command-name (vl ,@command-args)
       ,docstring
       (cmd vl (concatenate 'string ,(convert-to-vlc-command command-name) " " ,(alexandria:lastcar command-args))))))




(defmacro defcommand-with-on-or-off (name [on or off] docstring)
  (declare (ignore [on or off])) ;; we know that `args' is always the symbol [on :or off]
  (let ((command-name (intern (format nil "VLC-CMD-~s" name))))
    `(defun ,command-name (vl &optional switch)
       ,docstring
       (if switch
         (progn
           (assert (or (eq switch 'on) (eq switch 'off)) (switch)
             "argument `switch' doesn't have the value ON or OFF. Its value is: ~A" switch)
           (cmd vl (concatenate 'string ,(convert-to-vlc-command command-name) " "
                     ;; Use `string-downcase' because VLC doesn't like (i.e. it ignores) uppercase ON and OFF
                     (string-downcase (symbol-name switch)))))
         ;; Don't use "(symbol-name switch)" since that would send a string "NIL" to VLC
         (cmd vl (concatenate 'string ,(convert-to-vlc-command command-name)))))))






;;; Create the wrappers around the VLC commands
(progn
  (defcommand add XYZ "add XYZ to playlist")
  (defcommand enqueue XYZ "queue XYX to playlist")
  (defcommand playlist "show items currently in playlist")
  (defcommand search [string] "search for items in playlist (or reset search)")
  (defcommand sort key "sort the playlist")
  (defcommand sd [sd] "show services discovery or toggle")
  (defcommand play "play stream")
  (defcommand stop "stop stream")
  (defcommand next "next playlist item")
  (defcommand prev "previous playlist item")
  (defcommand goto "goto item at index")
  (defcommand gotoitem "goto item at index")
  (defcommand repeat [on :or off] "toggle playlist repeat")
  (defcommand loop [on :or off] "toggle playlist loop")
  (defcommand random [on :or off] "toggle playlist random")
  (defcommand clear "clear the playlist")
  (defcommand status "current playlist status")
  (defcommand title [x] "set/get title in current item")
  (defcommand title_n "next title in current item")
  (defcommand title_p "previous title in current item")
  (defcommand chapter [x] "set/get chapter in current item")
  (defcommand chapter_n "next chapter in current item")
  (defcommand chapter_p "previous chapter in current item")
  (defcommand seek X "seek in seconds, for instance `seek 12'")
  (defcommand pause "toggle pause")
  (defcommand fastforward "set to maximum rate")
  (defcommand rewind "set to minimum rate")
  (defcommand faster "faster playing of stream")
  (defcommand slower "slower playing of stream")
  (defcommand normal "normal playing of stream")
  (defcommand rate [playback-rate] "set playback rate to value")
  (defcommand frame "play frame by frame")
  (defcommand fullscreen [on :or off] "toggle fullscreen")
  (defcommand f [on :or off] "toggle fullscreen")
  (defcommand info "information about the current stream")
  (defcommand stats "show statistical information")
  (defcommand get_time "seconds elapsed since stream's beginning")
  (defcommand is_playing "1 if a stream plays, 0 otherwise")
  (defcommand get_title "the title of the current stream")
  (defcommand get_length "the length of the current stream")
  (defcommand volume [X] "set/get audio volume")
  (defcommand volup [X] "raise audio volume X steps")
  (defcommand voldown [X] "lower audio volume X steps")
  (defcommand adev [X] "set/get audio device")
  (defcommand achan [X] "set/get audio channels")
  (defcommand atrack [X] "set/get audio track")
  (defcommand vtrack [X] "set/get video track")
  (defcommand vratio [X] "set/get video aspect ratio")
  (defcommand vcrop [X] "set/get video crop")
  (defcommand crop [X] "set/get video crop")
  (defcommand vzoom [X] "set/get video zoom")
  (defcommand zoom [X] "set/get video zoom")
  (defcommand vdeinterlace [X] "set/get video deinterlace")
  (defcommand vdeinterlace_mode [X] "set/get video deinterlace mode")
  (defcommand snapshot "take video snapshot")
  (defcommand strack [X] "set/get subtitles track")
  (defcommand vlm "load the VLM")
  (defcommand description "describe this module")
  (defcommand help [pattern] "a help message") ; Do not use the alias "?"
  (defcommand longhelp [pattern] "a longer help message")
  (defcommand lock "lock the telnet prompt")
  ;; These commands need some extra processing (i.e. closing the telnet connections so they're
  ;; defined as normal lisp functions instead of using the `defcommand' macro
  ;; (defcommand quit "quit VLC (or logout if in a socket connection)")
  ;; (defcommand shutdown "shutdown VLC")
  ;; (defcommand logout "exit (if in a socket connection)")
  )


;; These commands need some extra processing (i.e. closing the telnet connections so they're
;; defined as normal lisp functions instead of using the `defcommand' macro
(defun vlc-cmd-logout (vl)
  (telnetlib:write-ln (videolan-connection-telnet vl) "exit")
  (telnetlib:close-telnet-session (videolan-connection-telnet vl))
  (setf (videolan-connection-telnet vl) nil))

(defun vlc-cmd-shutdown (vl)
  (telnetlib:write-ln (videolan-connection-telnet vl) "shutdown")
  (telnetlib:close-telnet-session (videolan-connection-telnet vl))
  (setf (videolan-connection-telnet vl) nil))

(defun vlc-cmd-quit (vl)
  (telnetlib:write-ln (videolan-connection-telnet vl) "quit")
  (telnetlib:close-telnet-session (videolan-connection-telnet vl))
  (setf (videolan-connection-telnet vl) nil))


;;; List of VLC commands as obtained from using the "help" command in the terminal
;; +----[ CLI commands ]
;; | add XYZ  . . . . . . . . . . . . . . . . . . . . add XYZ to playlist
;; | enqueue XYZ  . . . . . . . . . . . . . . . . . queue XYZ to playlist
;; | playlist . . . . . . . . . . . . . .show items currently in playlist
;; | search [string]  . .  search for items in playlist (or reset search)
;; | sort key . . . . . . . . . . . . . . . . . . . . . sort the playlist
;; | sd [sd]  . . . . . . . . . . . . . show services discovery or toggle
;; | play . . . . . . . . . . . . . . . . . . . . . . . . . . play stream
;; | stop . . . . . . . . . . . . . . . . . . . . . . . . . . stop stream
;; | next . . . . . . . . . . . . . . . . . . . . . .  next playlist item
;; | prev . . . . . . . . . . . . . . . . . . . .  previous playlist item
;; | goto, gotoitem . . . . . . . . . . . . . . . . . .goto item at index
;; | repeat [on|off]  . . . . . . . . . . . . . .  toggle playlist repeat
;; | loop [on|off]  . . . . . . . . . . . . . . . .  toggle playlist loop
;; | random [on|off]  . . . . . . . . . . . . . .  toggle playlist random
;; | clear  . . . . . . . . . . . . . . . . . . . . . .clear the playlist
;; | status . . . . . . . . . . . . . . . . . . . current playlist status
;; | title [X]  . . . . . . . . . . . . . . set/get title in current item
;; | title_n  . . . . . . . . . . . . . . . .  next title in current item
;; | title_p  . . . . . . . . . . . . . .  previous title in current item
;; | chapter [X]  . . . . . . . . . . . . set/get chapter in current item
;; | chapter_n  . . . . . . . . . . . . . .  next chapter in current item
;; | chapter_p  . . . . . . . . . . . .  previous chapter in current item
;; |
;; | seek X . . . . . . . . . . . seek in seconds, for instance `seek 12'
;; | pause  . . . . . . . . . . . . . . . . . . . . . . . .  toggle pause
;; | fastforward  . . . . . . . . . . . . . . . . . . set to maximum rate
;; | rewind . . . . . . . . . . . . . . . . . . . . . set to minimum rate
;; | faster . . . . . . . . . . . . . . . . . .  faster playing of stream
;; | slower . . . . . . . . . . . . . . . . . .  slower playing of stream
;; | normal . . . . . . . . . . . . . . . . . .  normal playing of stream
;; | rate [playback rate] . . . . . . . . . .  set playback rate to value
;; | frame  . . . . . . . . . . . . . . . . . . . . . play frame by frame
;; | fullscreen, f, F [on|off]  . . . . . . . . . . . . toggle fullscreen
;; | info . . . . . . . . . . . . . .information about the current stream
;; | stats  . . . . . . . . . . . . . . . .  show statistical information
;; | get_time . . . . . . . . . .seconds elapsed since stream's beginning
;; | is_playing . . . . . . . . . . . .  1 if a stream plays, 0 otherwise
;; | get_title  . . . . . . . . . . . . . the title of the current stream
;; | get_length . . . . . . . . . . . .  the length of the current stream
;; |
;; | volume [X] . . . . . . . . . . . . . . . . . .  set/get audio volume
;; | volup [X]  . . . . . . . . . . . . . . . .raise audio volume X steps
;; | voldown [X]  . . . . . . . . . . . . . .  lower audio volume X steps
;; | adev [X] . . . . . . . . . . . . . . . . . . . .set/get audio device
;; | achan [X]  . . . . . . . . . . . . . . . . . .set/get audio channels
;; | atrack [X] . . . . . . . . . . . . . . . . . . . set/get audio track
;; | vtrack [X] . . . . . . . . . . . . . . . . . . . set/get video track
;; | vratio [X] . . . . . . . . . . . . . . . .set/get video aspect ratio
;; | vcrop, crop [X]  . . . . . . . . . . . . . . . .  set/get video crop
;; | vzoom, zoom [X]  . . . . . . . . . . . . . . . .  set/get video zoom
;; | vdeinterlace [X] . . . . . . . . . . . . . .set/get video deintelace
;; | vdeinterlace_mode [X]  . . . . . . . . set/get video deintelace mode
;; | snapshot . . . . . . . . . . . . . . . . . . . . take video snapshot
;; | strack [X] . . . . . . . . . . . . . . . . . set/get subtitles track
;; |
;; | vlm  . . . . . . . . . . . . . . . . . . . . . . . . . .load the VLM
;; | description  . . . . . . . . . . . . . . . . . .describe this module
;; | help, ? [pattern]  . . . . . . . . . . . . . . . . . .a help message
;; | longhelp [pattern] . . . . . . . . . . . . . . a longer help message
;; | lock . . . . . . . . . . . . . . . . . . . .  lock the telnet prompt
;; | logout . . . . . . . . . . . . . .  exit (if in a socket connection)
;; | quit . . . . . . . .  quit VLC (or logout if in a socket connection)
;; | shutdown . . . . . . . . . . . . . . . . . . . . . . . .shutdown VLC
;; +----[ end of help ]
;;>

