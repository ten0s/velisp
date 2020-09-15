VeLisp is a simple AutoLISP compatible interpreter written in JavaScript to run outside AutoCAD.

```
$ npm install
$ make
$ make test
```

```
$ npm start -- examples/fib.lsp
55
```

```
$ make pkgLinux
$ ./velisp-0.0.1-linux-x64 examples/fib.lsp
55
```
