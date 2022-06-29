# VeLisp

[![Build Status](https://travis-ci.com/ten0s/velisp.svg?branch=master)](https://travis-ci.com/ten0s/velisp)

**VeLisp** is AutoLISP / DCL compatible interpreter to run AutoCAD agnostic code.
The goal is to treat AutoLISP as a general purpose language and be able to run
AutoLISP programs outside of AutoCAD.

## Run **VeLisp**

### Installation

#### Linux

```
$ wget https://github.com/ten0s/velisp/releases/download/0.7.0-3/velisp-0.7.0-3-linux-x64
$ chmod +x velisp-0.7.0-3-linux-x64
```

#### Windows

TODO

### Run REPL (Read–Eval–Print Loop)

```
$ ./velisp-0.7.0-3-linux-x64
VeLisp 0.7.0-3 on linux
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
$ ./velisp-0.7.0-3-linux-x64 examples/fib.lsp
55
```

Calculate 11th Fibonacci number

```
$ ./velisp-0.7.0-3-linux-x64 examples/fib.lsp 11
89
```

Run Calculator

```
$ ./velisp-0.7.0-3-linux-x64 examples/calc.lsp
```

![App Calc Image](/images/app-calc.png)

Run Minesweeper

```
$ ./velisp-0.7.0-3-linux-x64 examples/mines.lsp
```

![App Mines Image](/images/app-mines.png)

Run Fifteen Puzzle

```
$ ./velisp-0.7.0-3-linux-x64 examples/fifteen.lsp
```

![App Fifteen Image](/images/app-fifteen.png)

### Run code from standard input

```
$ cat examples/fib.lsp | ./velisp-0.7.0-3-linux-x64
55
```

```
$ cat examples/fib.lsp | ./velisp-0.7.0-3-linux-x64 -- 11
89
```

```
$ echo '(alert "Hello from VeLisp!")' | ./velisp-0.7.0-3-linux-x64
```

![Alert Hello From VeLisp Image](/images/alert-hello-velisp.png)

```
$ echo '(alert (strcat "Hello from " (argv 1) "!"))' | ./velisp-0.7.0-3-linux-x64 -- Arg
```

![Alert Hello From Arg Image](/images/alert-hello-arg.png)

## Implemented functions

* [AutoLISP Functions](/AutoLISP-Functions.md)
* [DCL Functions](DCL-Functions.md)

## Known issues and limitations

* Integers are signed numbers with values ranging from -9,007,199,254,740,991 (-2<sup>53</sup>+1) to +9,007,199,254,740,991 (+2<sup>53</sup>-1)

## See also

* [Development](/DEVEL.md)
* [How To](/HOWTO.md)

## Versioning

The version reflects subjective percentage of completeness.
For example, the version 0.7.x should be read as **VeLisp** is 0.7x (7x%) ready.

## Contributing

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, shall be licensed as below, without any
additional terms or conditions.

## License

The project is licensed under the GNU General Public License v3.0 or later with the exception below.
See [LICENSE](LICENSE) or
https://spdx.org/licenses/GPL-3.0-or-later.html
for full license information.

The files in the [examples/](examples/) and [lib/](lib/) directories
are licensed under the BSD Zero Clause License.
See [LICENSE](examples/LICENSE) or [LICENSE](lib/LICENSE) or
https://spdx.org/licenses/0BSD.html for full license
information.
