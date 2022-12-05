@echo off

REM
REM ssh [command] and sshfs work
REM scp and rsync don't work :(
REM

setlocal ENABLEDELAYEDEXPANSION

if [%1] == [-c] (
    set rest=%*
    set rest=!rest:*%1 =!
)

if defined rest (
    set args=%1 "%rest%"
) else (
    set args=
)

set HOME=
\msys64\\msys2_shell.cmd -defterm -full-path -no-start -mingw64 %args%
