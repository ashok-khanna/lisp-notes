REM $Header: /usr/local/cvsrep/regex-plugin/build-plugin.cmd,v 1.4 2008/01/07 07:36:02 edi Exp $

REM +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
REM + Build script adapted from FM-PLUGIN-TOOLS example           +
REM +                                                             +
REM + Change the following `set' statements to adapt them to your +
REM + local settings:                                             +
REM +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

REM The directory where the DLL will be built
set build_dir="c:\tmp"

REM The FileMaker extension directory
set target_dir="c:\Program Files\FileMaker\FileMaker Pro 8.5 Advanced\Extensions"

REM The delivery script
set script="c:\home\lisp\regex-plugin\deliver.lisp"

REM The LispWorks executable
set lispworks="c:\Program Files\LispWorks\lispworks-5-1-0-x86-win32.exe"

REM The name of the plug-in (.dll and .fmx files)
set name=RegexPlugIn

REM +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
REM + Don't change anything below this point.                     +
REM +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cd %build_dir%
if errorlevel 1 (
  echo Couldn't change to directory %build_dir%.
  pause
) else (
  %lispworks% -build %script% %name%
  if errorlevel 1 (
    echo Couldn't build DLL.
    pause
  ) else (
    copy /Y %name%.fmx %target_dir%
    if errorlevel 1 (
      echo Couldn't copy DLL into %target_dir%.
    pause
    )
  )
)
