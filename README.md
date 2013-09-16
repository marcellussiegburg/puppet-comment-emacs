puppet-comment for Emacs
========================
This emacs module provides a minor-mode which will be loaded automatically together with puppet mode.

It provides skeletons for writing comments in puppet. In the minor mode these
skeleton functions can be called via default keybindings.

You need skeletons installed, for example inside your site-lisp directory. It
should be installed, if you have the latest emacs version installed.

Aim of this project
-------------------
The aim of this piece of software is to support all comment types mentioned in
the puppet coding style guide
(http://docs.puppetlabs.com/guides/style_guide.html#puppet-doc).

At the moment it fullfills requirements of the puppet style guide version 1.1.2.

How to install
--------------

Put *puppet-comment.el* in your load-path (e.g. site-lisp directory) and require puppet-comment or put the following lines in you .emacs file:

```lisp
(add-to-list 'load-path "***path to puppet-comment***/")
(require 'puppet-comment)
```

If puppet-mode is not in your load-path you should additionally enable loading
of puppet-comment-mode by putting this in your .emacs file:
```lisp
(add-to-list 'auto-mode-alist '("\\.pp\\'" . puppet-comment-mode))
```

Provided Keyboard Bindings
--------------------------
| Key Binding | Called Function                |
| ----------- | ------------------------------ |
| C-c c C-c   | ***puppet-class-comment***     |  
| C-c c C-d   | ***puppet-define-comment***    |
| C-c c C-p   | ***puppet-parameter-comment*** |
| C-c c C-v   | ***puppet-variable-comment***  |
| C-c c C-a   | ***puppet-author-comment***    |

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

***puppet-author-comment***
* Promts you for the name of the author
* Next it promts for the email of that author
* Finally it is starting again until you submit an empty string (nil) for the
  name of the author