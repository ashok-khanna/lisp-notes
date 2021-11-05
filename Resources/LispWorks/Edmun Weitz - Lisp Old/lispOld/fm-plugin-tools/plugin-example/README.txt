With the code in this subdirectory you can create an example plug-in
that basically does the same stuff the example that comes with
FileMaker Pro Advanced does.

To build it adjust the values in build-plugin.cmd and deliver.lisp to
your local settings and just run the script - that should do the
trick.

Note that you need some freely available CL libraries for this
example:

  CL-PPCRE               http://weitz.de/cl-ppcre/
  TRIVIAL-GRAY-STREAMS   http://cliki.net/trivial-gray-streams
  FLEXI-STREAMS          http://weitz.de/flexi-streams/
  ZPB-EXIF               http://xach.com/lisp/zpb-exif/
