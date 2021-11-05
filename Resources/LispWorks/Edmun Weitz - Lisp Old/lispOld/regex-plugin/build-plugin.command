#!/bin/sh
# $Header: /usr/local/cvsrep/regex-plugin/build-plugin.command,v 1.1 2008/01/07 20:33:09 edi Exp $

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# + Build script adapted from FM-PLUGIN-TOOLS example           +
# +                                                             +
# + Change the following assignments to adapt them to           +
# + your local settings:                                        +
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# The directory where the DLL will be built
build_dir="/tmp"

# The FileMaker extension directory
target_dir="/Applications/FileMaker Pro 9/Extensions"

# The delivery script
script="/Users/karlpurtz/lisp/regex-plugin/deliver.lisp"

# The LispWorks executable
lispworks="/Applications/LispWorks5.1/LispWorks.app/Contents/MacOS/lispworks-5-1-0-macos-universal"

# The name of the plug-in (the .fmplugin file)
name=RegexPlugIn

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

