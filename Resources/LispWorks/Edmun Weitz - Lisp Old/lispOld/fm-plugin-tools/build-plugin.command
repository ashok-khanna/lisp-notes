#!/bin/sh
# $Header: /usr/local/cvsrep/fm-plugin-tools/build-plugin.command,v 1.1 2008/01/06 00:21:46 edi Exp $

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# + Example build script for OS X                               +
# +                                                             +
# + Change the following assignments to adapt them to           +
# + your local settings:                                        +
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# The directory where the DLL will be built
build_dir="/tmp"

# The FileMaker extension directory
target_dir="/Applications/FileMaker Pro 9/Extensions"

# The delivery script
script="/Users/karlpurtz/lisp/fm-plugin-tools/deliver.lisp"

# The LispWorks executable
lispworks="/Applications/LispWorks 5.1/LispWorks.app/Contents/MacOS/lispworks-5-1-0-macos-universal"

# The name of the plug-in (the .fmplugin file)
name=LispPlugInExample

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# + Don't change anything below this point.                     +
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cd "$build_dir"
if [ $? -eq 0 ]
then
  "$lispworks" -build "$script" "$name" "$build_dir"
  if [ $? -eq 0 ]
  then
    rm -rf "$target_dir/$name.fmplugin"
    mv "$build_dir/$name.app" "$target_dir/$name.fmplugin"
   if [ $? -ne 0 ]
   then
      echo "Couldn't copy the bundle to $target_dir."
   fi
  else
    echo "Couldn't build the loadable bundle."
  fi
else
  echo "Couldn't change to directory $build_dir."
fi

