# VeLisp

[![Build Status](https://travis-ci.com/ten0s/velisp.svg?branch=master)](https://travis-ci.com/ten0s/velisp)

**VeLisp** is AutoLISP / DCL compatible interpreter to run AutoCAD independent code.

## Run **VeLisp**

### Installation

#### Linux

```
$ wget https://github.com/ten0s/velisp/releases/download/0.6.9/velisp-0.6.9-linux-x64
$ chmod +x velisp-0.6.9-linux-x64
```

#### Windows

TODO

### Run REPL (Read–Eval–Print Loop)

```
$ ./velisp-0.6.9-linux-x64
VeLisp 0.6.9 on linux
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
$ ./velisp-0.6.9-linux-x64 examples/fib.lsp
55
```

Run Calculator

```
$ ./velisp-0.6.9-linux-x64 examples/calc.lsp
```

![App Calc Image](/images/app-calc.png)

Run Minesweeper

```
$ ./velisp-0.6.9-linux-x64 examples/mines.lsp
```

![App Mines Image](/images/app-mines.png)

Run Fifteen Puzzle

```
$ ./velisp-0.6.9-linux-x64 examples/fifteen.lsp
```

![App Fifteen Image](/images/app-fifteen.png)

### Run code from standard input

```
$ cat examples/fib.lsp | ./velisp-0.6.9-linux-x64
55
```

```
$ echo '(alert "Hi from VeLisp!")' | ./velisp-0.6.9-linux-x64
```

![Alert Hello Image](/images/alert-hello.png)

## Implemented functions

* [AutoLISP Functions](/AutoLISP-Functions.md)
* [DCL Functions](DCL-Functions.md)

## Versioning

For example, the version 0.6.1 should be read as **VeLisp** is 0.61 (61%) ready
comparing to the reference implementation.

## Known issues and limitations

## See also

* [Development](/DEVEL.md)
* [How To](/HOWTO.md)
