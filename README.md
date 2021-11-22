# VeLisp

[![Build Status](https://travis-ci.com/ten0s/velisp.svg?branch=master)](https://travis-ci.com/ten0s/velisp)

**VeLisp** is AutoLISP / DCL compatible interpreter to run AutoCAD independent code.

## Prerequisites

In order to build **VeLisp** you need to have [Node.js 12.x](https://nodejs.org/dist/latest-v12.x/) and
[GNU Make](http://www.gnu.org/software/make/) installed.

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

## Build

```
$ make install
```

## Run tests

```
$ make test
```

## Run REPL (Read–Eval–Print Loop)

```
$ npm start
VeLisp 0.6.0 on linux
Type ".help" for more information
> (+ 1 2)
3
> (load "examples/fib.lsp")
55
55
> (defun add (a b) (+ a b))
ADD
> .inspect 'add
Sym { sym: 'ADD' }
> .inspect add
Fun { name: 'ADD', params: [ 'A', 'B' ], locals: [], fun: [Function] }
> .inspect (add 1 2)
Int { int: 3 }
> (mapcar '(lambda (x y) (itoa (+ x y))) '(1 2 3) '(9 8 7))
("10" "10" "10")
```

## Run code from file

```
$ npm start -- examples/fib.lsp
55
```

## Run code from standard input

```
$ cat examples/fib.lsp | npm start
55
```

## Environment variables

Various behaviors can be customized using the following environment variables:

* VELISP_REPL_HISTORY: When a valid path is given, persistent REPL history will be
saved to the specified file rather than .velisp_repl_history in the user's home
directory. Setting this value to '' (an empty string) will disable persistent
REPL history.
* VELISP_REPL_HISTORY_SIZE: Controls how many lines of history will be persisted
if history is available. Must be a positive number. Default: 1000.

## Create Linux executables that can be run without Node.js installed


```
$ make pkgLinux
```

```
$ ls velisp*
velisp-0.6.0-linux-x64
```

```
$ ./velisp-0.6.0-linux-x64 examples/fib.lsp
55
```
