puppet-comment for Emacs
========================
This module provides skeletons for writing comments in puppet, if you are using
emacs.

You need skeletons installed, for example inside your site-lisp directory.

Aim of this project
-------------------
The aim of this piece of software is to support all comment types mentioned in
the puppet coding style guide
(http://docs.puppetlabs.com/guides/style_guide.html#puppet-doc).

Provided Functions
------------------
***puppet-class-comment***
* Resolves the class name to the name of the current class
* Promts you for all other required information according to the coding style
  guide

***puppet-define-comment***
* Resolves the define name to the name of the current class
* Promts you for all other required information according to the coding style
  guide

***puppet-parameter-comment***
* Promts you for the name of the parameter
* Next it promts for the description of that parameter
* Finally it is starting again until you submit an empty string (nil) for the
  name of the parameter

***puppet-variable-comment***
* Promts you for the name of the variable
* Next it promts for the description of that variable
* Finally it is starting again until you submit an empty string (nil) for the
  name of the variable

***puppet-author-comment****
* Promts you for the name of the author
* Next it promts for the email of that author
* Finally it is starting again until you submit an empty string (nil) for the
  name of the author