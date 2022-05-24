# VeLisp

[![Build Status](https://travis-ci.com/ten0s/velisp.svg?branch=master)](https://travis-ci.com/ten0s/velisp)

**VeLisp** is AutoLISP / DCL compatible interpreter to run AutoCAD agnostic code.
The goal is to treat AutoLISP as a general purpose language and be able to run
AutoLISP programs outside of AutoCAD.

## Run **VeLisp**

### Installation

#### Linux

```
$ wget https://github.com/ten0s/velisp/releases/download/0.6.12/velisp-0.6.12-linux-x64
$ chmod +x velisp-0.6.12-linux-x64
```

#### Windows

On Windows 10 (with Windows Subsystem for Linux (WSL)):

Install:
1. Windows: Activate WSL: https://docs.microsoft.com/en-us/windows/wsl/install
2. Windows: Open CMD and run: 'bash' in order to start the WSL-bash
3. WSL-Bash: run: 'sudo apt-get install libgtk-3-dev'

Run:
1. Windows: Install a Windows X-Server like VcXsrv: https://sourceforge.net/projects/vcxsrv
3. Windows: Start the X-Server with: 'vcxsrv :0 -ac -terminate -lesspointer -multiwindow -clipboard -dpi auto'
4. Windows: Open CMD and run: 'bash' in order to start the WSL-bash
5. WSL-Bash: run: 'DISPLAY=:0 velisp-0.6.12-linux-x64'

TODO

### Run REPL (Read–Eval–Print Loop)

```
$ ./velisp-0.6.12-linux-x64
VeLisp 0.6.12 on linux
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

Calculate 10th (default) Fibonacci number

```
$ ./velisp-0.6.12-linux-x64 examples/fib.lsp
55
```

Calculate 11th Fibonacci number

```
$ ./velisp-0.6.12-linux-x64 examples/fib.lsp 11
89
```

Run Calculator

```
$ ./velisp-0.6.12-linux-x64 examples/calc.lsp
```

![App Calc Image](/images/app-calc.png)

Run Minesweeper

```
$ ./velisp-0.6.12-linux-x64 examples/mines.lsp
```

![App Mines Image](/images/app-mines.png)

Run Fifteen Puzzle

```
$ ./velisp-0.6.12-linux-x64 examples/fifteen.lsp
```

![App Fifteen Image](/images/app-fifteen.png)

### Run code from standard input

```
$ cat examples/fib.lsp | ./velisp-0.6.12-linux-x64
55
```

```
$ cat examples/fib.lsp | ./velisp-0.6.12-linux-x64 -- 11
89
```

```
$ echo '(alert "Hello from VeLisp!")' | ./velisp-0.6.12-linux-x64
```

![Alert Hello From VeLisp Image](/images/alert-hello-velisp.png)

```
$ echo '(alert (strcat "Hello from " (argv 1) "!"))' | ./velisp-0.6.12-linux-x64 -- Arg
```

![Alert Hello From Arg Image](/images/alert-hello-arg.png)

## Implemented functions

* [AutoLISP Functions](/AutoLISP-Functions.md)
* [DCL Functions](DCL-Functions.md)

## Versioning

The version reflects subjective percentage of completeness.
For example, the version 0.6.1 should be read as **VeLisp** is 0.61 (61%) ready.

## Known issues and limitations

* Integers are signed numbers with values ranging from -9,007,199,254,740,991 (-2<sup>53</sup>+1) to +9,007,199,254,740,991 (+2<sup>53</sup>-1)

## See also

* [Development](/DEVEL.md)
* [How To](/HOWTO.md)
