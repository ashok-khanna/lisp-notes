REM $Header: /usr/local/cvsrep/starter-pack/build-exe.cmd,v 1.8 2011/02/11 19:53:00 edi Exp $

REM +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
REM + Change the following `set' statements to adapt them to your +
RME + local settings:                                             +
REM +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

REM The directory where the EXE will be built
set build_dir="c:\Users\edi\Desktop"

REM The delivery script
set script="%~dp0%deliver.lisp"

REM The LispWorks executable
set lispworks="c:\Program Files (x86)\LispWorks\lispworks-6-0-0-x86-win32.exe"

REM The name of the executable
set name=LispStarterPack

REM +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
REM + Don't change anything below this point.                     +
REM +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cd %build_dir%
if errorlevel 1 (
  echo Couldn't change to directory %build_dir%.
  pause
) else (
  %lispworks% -init %script% %name%
  if errorlevel 1 (
    echo Couldn't build application.
    pause
  ) else (
    del %name%.zaps
  )
)