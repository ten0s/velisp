VeLisp is a simple AutoLISP compatible interpreter to run AutoCAD independent code.

```
$ npm install
$ make
$ make test
```

```
$ npm start
VeLisp 0.0.2 on linux
Type ".help" for more information
> (load "examples/fib.lsp")
55
55
```

```
$ npm start -- examples/fib.lsp
55
```

```
$ make pkgLinux
$ ./velisp-0.0.2-linux-x64 examples/fib.lsp
55
```
