# VeLisp

[![Build Status](https://travis-ci.com/ten0s/velisp.svg?branch=master)](https://travis-ci.com/ten0s/velisp)

**VeLisp** is AutoLISP / DCL compatible interpreter to run AutoCAD independent code.

## Prerequisites

In order to build **VeLisp** you need to have
[Docker](https://docs.docker.com/installation/#installation) with [non-root access](https://docs.docker.com/engine/installation/linux/linux-postinstall/)
installed.

## Build **VeLisp**

```
$ ./build-in-docker.sh
...
Build Done
```

```
$ ls velisp*
velisp-0.6.7-linux-x64
```

## Run **VeLisp**

### Run REPL (Read–Eval–Print Loop)

```
$ ./velisp-0.6.7-linux-x64
VeLisp 0.6.7 on linux
Type ".help" for more information
> (+ 1 2)
3
> (load "examples/fib.lsp")
5555
> (defun add (a b) (+ a b))
ADD
> (mapcar '(lambda (x y) (itoa (add x y))) '(1 2 3) '(9 8 7))
("10" "10" "10")
> (quit)
```

### Run code from file

Calculate 10th Fibonacci number

```
$ ./velisp-0.6.7-linux-x64 examples/fib.lsp
55
```

Run Calculator

```
$ ./velisp-0.6.7-linux-x64 examples/calc.lsp
```

![App Calc Image](/images/app-calc.png)

Run Minesweeper

```
$ ./velisp-0.6.7-linux-x64 examples/mines.lsp
```

![App Mines Image](/images/app-mines.png)

### Run code from standard input

```
$ cat examples/fib.lsp | ./velisp-0.6.7-linux-x64
55
```

```
$ echo '(alert "Hi from VeLisp!")' | ./velisp-0.6.7-linux-x64
```

![Alert Hello Image](/images/alert-hello.png)

## Implemented functions

* [AutoLISP Functions](/AutoLISP-Functions.md)
* [DCL Functions](DCL-Functions.md)

## Environment variables

Various behaviors can be customized using the following environment variables:

* VELISP_REPL_HISTORY: When a valid path is given, persistent REPL history will be
saved to the specified file rather than .velisp_repl_history in the user's home
directory. Setting this value to '' (an empty string) will disable persistent
REPL history.
* VELISP_REPL_HISTORY_SIZE: Controls how many lines of history will be persisted
if history is available. Must be a positive number. Default: 1000.

## Development

### Build deps

```
$ sudo apt install jq make
```

### [Node.js 12.x](https://nodejs.org/dist/latest-v12.x/)

```
$ curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.0/install.sh | bash
$ nvm install 12
```

### [GTK+3](https://www.gtk.org/) deps

```
$ sudo apt-get install build-essential libgtk-3-dev gobject-introspection libgirepository1.0-dev libcairo2 libcairo2-dev
```

### [ANTLR](https://www.antlr.org/)

[ANTLR](https://www.antlr.org/) is only needed for making changes in the grammars:

* [VeLisp.g4](/grammar/VeLisp.g4)
* [VeDcl.g4](/grammar/VeDcl.g4)

## Build

```
$ make
```

## Run tests

```
$ make test
```
