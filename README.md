# VeLisp

**VeLisp** is AutoLISP / DCL compatible interpreter to run AutoCAD agnostic code.
The goal is to treat AutoLISP as a general purpose language and be able to run
AutoLISP programs outside of AutoCAD.

## Table of contents

- [Windows](#windows)
  - [Install](#install)
  - [Usage](#usage)
- [Linux](#linux)
  - [Install](#install-1)
  - [Usage](#usage-1)
- [MacOS](#macos)
  - [Install](#install-2)
  - [Usage](#usage-2)
- [Implemented functions](#implemented-functions)
- [Known issues and limitations](#known-issues-and-limitations)
- [Versioning](#versioning)
- [Contributing](#contributing)
- [License](#license)
- [See also](#see-also)

## Windows

### Install

#### Download and unarchive

1. Download https://github.com/ten0s/velisp/releases/download/0.7.0-3/velisp-0.7.0-3-win-x64.zip
2. Open Downloads folder
3. Select velisp-0.7.0-3-win-x64.zip
4. Press the right mouse button
5. Click 'Extract All...'
6. Enter C:\
7. Click 'Extract'

The steps below assume the archive was extracted to C:\velisp-0.7.0-3-win-x64
according to the steps above.

#### Add the folder to PATH

1. Press 'Windows + Break' to open the System Info
2. Click 'Advanced system settings'
3. Click 'Environment Variables...'
4. Select 'Path' inside the User variables block
5. Click 'Edit...'
6. Click 'New...'
7. Enter C:\velisp-0.7.0-3-win-x64
8. Click 'OK'
9. Click 'OK'

### Usage

Run Command Prompt (cmd.exe)

```
> cd C:\velisp-0.7.0-3-win-x64
```

#### Run REPL (Read–Eval–Print Loop)

```
> velisp
VeLisp 0.7.0-3 on win32
Type ".license" or ".help" for more information
> (+ 1 2)
3
> (defun add (a b) (+ a b))
ADD
> (mapcar '(lambda (x y) (itoa (add x y))) '(1 2 3) '(9 8 7))
("10" "10" "10")
> (quit)
```

#### Run code from file

Calculate 10th (by default) Fibonacci number

```
> velisp examples\fib.lsp
55
```

Calculate 11th Fibonacci number

```
> velisp examples\fib.lsp 11
89
```

Run the Calculator example

```
> velisp examples\calc.lsp
```

![App Calc Windows Image](/images/app-calc-windows.png)

Run the Minesweeper example

```
> velisp examples\mines.lsp
```

![App Mines Windows Image](/images/app-mines-windows.png)

Run the Fifteen Puzzle example

```
> velisp examples\fifteen.lsp
```

![App Fifteen Windows Image](/images/app-fifteen-windows.png)

### Run code from standard input

```
> cat examples\fib.lsp | velisp
55
```

```
> cat examples\fib.lsp | velisp -- 11
89
```

```
$ echo (alert "Hello from VeLisp!") | velisp
```

![Alert Hello From VeLisp Windows Image](/images/alert-hello-velisp-windows.png)

```
$ echo (alert (strcat "Hello from " (argv 1) "!")) | velisp -- Arg
```

![Alert Hello From Arg Windows Image](/images/alert-hello-arg-windows.png)

#### Run without DCL support (faster loading, good for scripting)

```
> velisp --no-dcl examples\fib.lsp
55
```

## Linux

### Install

#### Download and unarchive

```
$ wget https://github.com/ten0s/velisp/releases/download/0.7.0-3/velisp-0.7.0-3-linux-x64.tar.gz
$ tar xfz velisp-0.7.0-3-linux-x64.tar.gz
```

#### Add to .bashrc

```
export PATH=<PATH_TO>/velisp-0.7.0-3-linux-x64:$PATH
```

### Usage

```
$ cd velisp-0.7.0-3-linux-x64
```

#### Run REPL (Read–Eval–Print Loop)

```
$ velisp
VeLisp 0.7.0-3 on linux
Type ".license" or ".help" for more information
> (+ 1 2)
3
> (defun add (a b) (+ a b))
ADD
> (mapcar '(lambda (x y) (itoa (add x y))) '(1 2 3) '(9 8 7))
("10" "10" "10")
> (quit)
```

#### Run code from file

Calculate 10th (by default) Fibonacci number

```
$ velisp examples/fib.lsp
55
```

Calculate 11th Fibonacci number

```
$ velisp examples/fib.lsp 11
89
```

Run the Calculator example

```
$ velisp examples/calc.lsp
```

![App Calc Linux Image](/images/app-calc-linux.png)

Run the Minesweeper example

```
$ velisp examples/mines.lsp
```

![App Mines Linux Image](/images/app-mines-linux.png)

Run the Fifteen Puzzle example

```
$ velisp examples/fifteen.lsp
```

![App Fifteen Linux Image](/images/app-fifteen-linux.png)

#### Run code from standard input

```
$ cat examples/fib.lsp | velisp
55
```

```
$ cat examples/fib.lsp | velisp -- 11
89
```

```
$ echo '(alert "Hello from VeLisp!")' | velisp
```

![Alert Hello From VeLisp Linux Image](/images/alert-hello-velisp-linux.png)

```
$ echo '(alert (strcat "Hello from " (argv 1) "!"))' | velisp -- Arg
```

![Alert Hello From Arg Linux Image](/images/alert-hello-arg-linux.png)

#### Run without DCL support (faster loading, good for scripting)

```
$ velisp --no-dcl examples/fib.lsp
55
```
## MacOS

### Install

TODO

### Usage

TODO

## Implemented functions

* [AutoLISP Functions](/AutoLISP-Functions.md)
* [DCL Functions](DCL-Functions.md)

## Known issues and limitations

* Integers are signed numbers with values ranging from -9,007,199,254,740,991 (-2<sup>53</sup>+1) to +9,007,199,254,740,991 (+2<sup>53</sup>-1)

## Versioning

The version reflects subjective percentage of completeness.
For example, the version 0.7.x should be read as **VeLisp** is 0.7x (7x%) ready.

## Contributing

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, shall be licensed as below, without any
additional terms or conditions.

## License

The project is licensed under the GNU General Public License v3.0 or later
with the exception below.
See [LICENSE](LICENSE) or
https://spdx.org/licenses/GPL-3.0-or-later.html
for full license information.

The files in the [examples/](examples/) and [lib/](lib/) directories
are licensed under the BSD Zero Clause License.
See [LICENSE](examples/LICENSE) or [LICENSE](lib/LICENSE) or
https://spdx.org/licenses/0BSD.html for full license
information.

## See also

* [Development](/DEVEL.md)
* [How To](/HOWTO.md)
