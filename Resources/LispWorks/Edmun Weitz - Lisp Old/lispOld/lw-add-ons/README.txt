[See the file `doc/index.html' for more documentation.]


To use LW-ADD-ONS you need LW-DOC and a recent version of
LispWorks:

  <http://weitz.de/lw-doc/>
  <http://www.lispworks.com/>

Use of Quicklisp is recommended:

  <http://www.quicklisp.org/>

If you already have a LispWorks init file, append the included file
`.lispworks' to it, otherwise instruct LispWorks to use this file as
your initialization file.  In that file, modify the special variables
*ASDF-DIRS*, and *WORKING-DIR* to fit your local settings.
Specifically, make sure that LW-ADD-ONS, LW-DOC and their supporting
libraries can be found via *ASDF-DIRS*.

Download the HTML page <http://www.lisp.org/mop/dictionary.html> and
store it locally.  At the end of the init file (after LW-ADD-ONS has
been loaded) set the value of LW-ADD-ONS:*MOP-PAGE* to the pathname of
the saved HTML file.  (There are some other special variables that can
be used to modfiy the behaviour of LW-ADD-ONS.  See the documentation
for details.)

You should now be able to use LW-ADD-ONS by simply starting LispWorks.


Note: The Personal Edition of LispWorks doesn't support the automatic
loading of initialization files.  You'll have to use some kind of
workaround.
