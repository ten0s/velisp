[Русская версия](README-ru.md)

# VeLisp

**VeLisp** is AutoLISP interpreter with DCL support.
The idea is to treat AutoLISP as a general purpose programming language.
The goal is to run AutoLISP programs outside of AutoCAD.
Why? To learn basic AutoLISP and DCL programming, to write shell scripts, to
run DCL dialogs just like regular applications on Windows, MacOS and Linux.

## Table of contents

- [Windows](#windows)
  - [Install](#install)
  - [Usage](#usage)
- [MacOS](#macos)
  - [Install](#install-1)
  - [Usage](#usage-1)
- [Linux](#linux)
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

You can find the latest release [here](https://github.com/ten0s/velisp/releases).

#### Install via automatic setup

1. Download https://github.com/ten0s/velisp/releases/download/0.8.0/velisp-0.8.0-win-x64-setup.zip
2. Open Downloads folder
3. Double-click on velisp-0.8.0-win-x64-setup.zip to open the archive
4. Run velisp-0.8.0-win-x64-setup.exe
5. Follow the installation wizard. Default options are ok for most users

#### Install via binary archive

##### Download and unarchive

1. Download https://github.com/ten0s/velisp/releases/download/0.8.0/velisp-0.8.0-win-x64.zip
2. Open Downloads folder
3. Select velisp-0.8.0-win-x64.zip
4. Press the right mouse button
5. Click 'Extract All...'
6. Enter 'C:\'
7. Click 'Extract'

##### Add the folder to PATH

1. Press 'Windows + Break' to open the System Info
2. Click 'Advanced system settings'
3. Click 'Environment Variables...'
4. Select 'Path' inside the User variables block
5. Click 'Edit...'
6. Click 'New...'
7. Enter 'C:\velisp-0.8.0-win-x64'
8. Click 'OK'
9. Click 'OK'

### Usage

If you installed VeLisp via [automatic setup](#install-via-automatic-setup)
go to 'Start' -> 'VeLisp' -> 'VeLisp Command Prompt'.

If you installed Velisp via [binary archive](#install-via-binary-archive)
run the Command Prompt (cmd.exe) and move to the extraction directory:

```
> cd C:\velisp-0.8.0-win-x64
```

#### Run REPL (Read–Eval–Print Loop)

```
> velisp
VeLisp 0.8.0 on Windows
Type ".license" or ".help" for more information
_$ (+ 1 2)
3
_$ (defun add (a b) (+ a b))
ADD
_$ (mapcar '(lambda (x y) (itoa (add x y))) '(1 2 3) '(9 8 7))
("10" "10" "10")
_$ (quit)
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

Run the Demo example

```
> velisp examples\demo.lsp
```

![App Demo Windows Image](/images/app-demo-windows.png)

Run the Slides example

```
> velisp examples\slides.lsp
```

![App Slides Windows Image](/images/app-slides-windows.png)

### Run code from standard input

```
> type examples\fib.lsp | velisp
55
```

```
> type examples\fib.lsp | velisp -- 11
89
```

```
> echo (alert "Hello from VeLisp!") | velisp
```

![Alert Hello From VeLisp Windows Image](/images/alert-hello-velisp-windows.png)

```
> echo (alert (strcat "Hello from " (argv 2) "!")) | velisp -- Arg
```

![Alert Hello From Arg Windows Image](/images/alert-hello-arg-windows.png)

#### Run without DCL support (faster loading, good for scripting)

```
> velisp --no-dcl examples\fib.lsp
55
```

### Run the examples in AutoCAD

#### Add the examples folder to AutoCAD's search path

1. Click 'Tools' -> 'Options...'
2. Click 'Files' -> 'Support File Search Path'
3. Click 'Add...'
4. Click 'Browse...'
5. Select appropriately C:\Program Files\VeLisp\examples or C:\velisp-0.8.0-win-x64\examples
6. Click 'OK'
7. Click 'Apply'
7. Click 'OK'

#### Run the examples

1. Click 'Tools' -> 'AutoLISP' -> 'Visual LISP Editor'
2. Run in Visual LISP Console

```
_$ (load "calc")
_$ (load "mines")
_$ (load "fifteen")
_$ (load "demo")
_$ (load "slides")
```

### Self-extracting archive

How to create self-extracting archive
https://ten0s.github.io/blog/2024/10/05/velisp-self-extracting-archive-en

## MacOS

### Install

You can find the latest release [here](https://github.com/ten0s/velisp/releases).

#### Download and unarchive

**Warning**: Do not use Safari to download the archive, do not use Finder to
extract the archive and, most importantly, do not use Finder to start VeLisp
for the first time. Since VeLisp is not signed you will not be able to start
it this way due to
[Apple's Gatekeeper](https://support.apple.com/en-us/HT202491).
Instead, start the Terminal application (Finder -> Go -> Utilities -> Terminal) and run:

```
% cd $HOME
% curl -LJO https://github.com/ten0s/velisp/releases/download/0.8.0/velisp-0.8.0-macos-x64.tar.xz
% tar xfJ velisp-0.8.0-macos-x64.tar.xz
```

#### Add to ~/.zshrc

```
export PATH=<PATH_TO>/velisp-0.8.0-macos-x64:$PATH
```

### Usage

```
% cd velisp-0.8.0-macos-x64
```

#### Run REPL (Read–Eval–Print Loop)

```
% ./velisp
VeLisp 0.8.0 on MacOS
Type ".license" or ".help" for more information
_$ (+ 1 2)
3
_$ (defun add (a b) (+ a b))
ADD
_$ (mapcar '(lambda (x y) (itoa (add x y))) '(1 2 3) '(9 8 7))
("10" "10" "10")
_$ (quit)
```

#### Run code from file

Calculate 10th (by default) Fibonacci number

```
% ./velisp examples/fib.lsp
55
```

Calculate 11th Fibonacci number

```
% ./velisp examples/fib.lsp 11
89
```

Run the Calculator example

```
% ./velisp examples/calc.lsp
```

![App Calc MacOS Image](/images/app-calc-macos.png)

Run the Minesweeper example

```
% ./velisp examples/mines.lsp
```

![App Mines MacOS Image](/images/app-mines-macos.png)

Run the Fifteen Puzzle example

```
% ./velisp examples/fifteen.lsp
```

![App Fifteen MacOS Image](/images/app-fifteen-macos.png)

Run the Demo example

```
% ./velisp examples/demo.lsp
```

![App Demo MacOS Image](/images/app-demo-macos.png)

Run the Slides example

```
> ./velisp examples/slides.lsp
```

![App Slides MacOS Image](/images/app-slides-macos.png)

#### Run code from standard input

```
% cat examples/fib.lsp | ./velisp
55
```

```
% cat examples/fib.lsp | ./velisp -- 11
89
```

```
% echo '(alert "Hello from VeLisp!")' | ./velisp
```

![Alert Hello From VeLisp MacOS Image](/images/alert-hello-velisp-macos.png)

```
% echo '(alert (strcat "Hello from " (argv 2) "!"))' | ./velisp -- Arg
```

![Alert Hello From Arg MacOS Image](/images/alert-hello-arg-macos.png)

#### Run without DCL support (faster loading, good for scripting)

```
% ./velisp --no-dcl examples/fib.lsp
55
```

## Linux

### Install

You can find the latest release [here](https://github.com/ten0s/velisp/releases).

#### Download and unarchive

```
$ curl -LJO https://github.com/ten0s/velisp/releases/download/0.8.0/velisp-0.8.0-linux-x64.tar.xz
$ tar xfJ velisp-0.8.0-linux-x64.tar.xz
```

#### Add to ~/.bashrc

```
export PATH=<PATH_TO>/velisp-0.8.0-linux-x64:$PATH
```

### Usage

```
$ cd velisp-0.8.0-linux-x64
```

#### Run REPL (Read–Eval–Print Loop)

```
$ ./velisp
VeLisp 0.8.0 on Linux
Type ".license" or ".help" for more information
_$ (+ 1 2)
3
_$ (defun add (a b) (+ a b))
ADD
_$ (mapcar '(lambda (x y) (itoa (add x y))) '(1 2 3) '(9 8 7))
("10" "10" "10")
_$ (quit)
```

#### Run code from file

Calculate 10th (by default) Fibonacci number

```
$ ./velisp examples/fib.lsp
55
```

Calculate 11th Fibonacci number

```
$ ./velisp examples/fib.lsp 11
89
```

Run the Calculator example

```
$ ./velisp examples/calc.lsp
```

![App Calc Linux Image](/images/app-calc-linux.png)

Run the Minesweeper example

```
$ ./velisp examples/mines.lsp
```

![App Mines Linux Image](/images/app-mines-linux.png)

Run the Fifteen Puzzle example

```
$ ./velisp examples/fifteen.lsp
```

![App Fifteen Linux Image](/images/app-fifteen-linux.png)

Run the Demo example

```
$ ./velisp examples/demo.lsp
```

![App Demo Linux Image](/images/app-demo-linux.png)

Run the Slides example

```
> ./velisp examples/slides.lsp
```

![App Slides Linux Image](/images/app-slides-linux.png)

#### Run code from standard input

```
$ cat examples/fib.lsp | ./velisp
55
```

```
$ cat examples/fib.lsp | ./velisp -- 11
89
```

```
$ echo '(alert "Hello from VeLisp!")' | ./velisp
```

![Alert Hello From VeLisp Linux Image](/images/alert-hello-velisp-linux.png)

```
$ echo '(alert (strcat "Hello from " (argv 2) "!"))' | ./velisp -- Arg
```

![Alert Hello From Arg Linux Image](/images/alert-hello-arg-linux.png)

#### Run without DCL support (faster loading, good for scripting)

```
$ ./velisp --no-dcl examples/fib.lsp
55
```

## Implemented functions

* [AutoLISP Functions](/AutoLISP-Functions.md)
* [DCL Functions](DCL-Functions.md)

## Known issues and limitations

* Integers are signed numbers with values ranging from -9,007,199,254,740,991 (-2<sup>53</sup>+1) to +9,007,199,254,740,991 (+2<sup>53</sup>-1)

## Versioning

The version reflects subjective percentage of completeness.
For example, the version 0.7.3 should be read as **VeLisp** is 73% (0.73) ready.

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
