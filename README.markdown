# AutoComplete in text shell mode buffers

I find it particularly annoying that Emacs *shell* buffers do not offer satisfactory completions, but xterm does. I even used ansi-term for a while because of this (oh god). What I really wanted was for Emacs to show completions via auto-complete.el, like every other mode I use often. This package hopes to deliver on that promise.

Installation and more info: see .el file.

## Screenshots

* Auto fill in what you were probably going to type:
  ![](screenshots/path-1.png)
* Tab from there, and show the files in that directory:
  ![](screenshots/path-2.png)
* Readline completion is pretty smart: let us try to build emacs...
  ![](screenshots/parameter.png)
* SSH completion of hosts:
  ![](screenshots/ssh.png)
* Completion seems to work on this host...
  ![](screenshots/ssh-host.png)
* Ruby completion inside REPL:
  ![](screenshots/ruby.png)
