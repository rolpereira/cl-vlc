A Common Lisp library to control [VLC](http://www.videolan.org/vlc)
through its telnet protocol.

Currently in a non-functional state.


# Pre Requisites #

To use this module you need to have a VLC instance open and with the
Telnet interface active.

To automatically open the GUI interface of VLC and start the telnet
interface run the following command:

    vlc --extraintf telnet

To start VLC with only the telnet interface (i.e. no GUI) run the
following command:

    vlc --intf telnet


If you have already started VLC, you can also start the telnet
interface by going to the View menu and selecting the option "Add
interface" and then picking the "Telnet" option.

# Installation #

Currently this module isn't available in
[Quicklisp](http://www.quicklisp.org/beta/). However can still
install this module using Quicklisp by cloning this repository into
the folder `~/quicklisp/local-projects/` with the following command.

    git clone git@github.com:rolpereira/cl-vlc.git ~/quicklisp/local-projects


After starting you lisp session use the command `(ql:quickload
'cl-vlc)` to load the module

# Usage #

After opening VLC as specified in the *Preparation* section of this
README create a `videolan-connection` object and use the
`cl-vlc:login` function to connect to the running instance of VLC.

In other words, do the following:

```lisp
(defvar *vlc-connection* (make-videolan-connection))

(cl-vlc:login *vlc-connection*)
```

After that you can send raw commands to the telnet interface by using
the function `cl-vlc:cmd`. For example the following code will start
playing the file "Lorem Ipsum.mp3" in the current instance of VLC:

```lisp
(cl-vlc:cmd "add Lorem Ipsum.mp3")
```

You can also use one of the wrapper functions. In this case the
previous call to `cl-vlc:cmd` could be replaced with a call to the
function `cl-vlc:vlc-cmd-add` like so:

```lisp
(cl-vlc:vlc-cmd-add "Lorem Ipsum.mp3")
```

# Bugs #

* Currently it doesn't seem to support sending strings to VLC
  containing non ASCII characters. For example the following may not
  work:

```lisp
(cl-vlc:vlc-cmd-add "Lórêm Ìpsum.mp4")
```
