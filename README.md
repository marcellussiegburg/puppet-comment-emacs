puppet-comment for Emacs
========================
This module provides skeletons for writing comments in puppet, if you are using
emacs.

Of course you need skeletons installed, for example inside your site-lisp
directory.

Aim of this project
-------------------
The aim of this piece of software is to support all comment types mentioned in
the puppet coding style guide
(http://docs.puppetlabs.com/guides/style_guide.html#puppet-doc) 

Provided Functions
------------------
At the moment you can use the command ***puppet-variable-comment*** only.

***puppet-variable-comment***
* Promts you for the variable name
* Next it promts for the description of that variable
* Finally it is starting again until you submit an empty string (nil) for
  variable name
