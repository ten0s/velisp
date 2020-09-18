**VeLisp** is a simple AutoLISP compatible interpreter to run AutoCAD independent code.

## Prerequisites

In order to build **VeLisp** you need to have [Node.js 10.x](https://nodejs.org/dist/latest-v10.x/) and
[GNU Make](http://www.gnu.org/software/make/) installed.

## Build

```
$ npm install
$ make
$ make test
```

## Run REPL (Read–Eval–Print Loop)

```
$ npm start
VeLisp 0.0.2 on linux
Type ".help" for more information
> (load "examples/fib.lsp")
55
55
```

## Run code from file

```
$ npm start -- examples/fib.lsp
55
```

## Run code from standard input

```
$ cat examples/fib.lsp npm start
55
```

## Create Linux, Windows and MacOS executables that can be run without Node.js installed


```
$ make pkgLinux
$ make pkgWin86
$ make pkgWin64
$ make pkgMacOS
```

```
$ ls velisp*
velisp-0.0.2-linux-x64  velisp-0.0.2-macos-x64  velisp-0.0.2-win-x64.exe  velisp-0.0.2-win-x86.exe
```

```
$ ./velisp-0.0.2-linux-x64 examples/fib.lsp
55
```
