VeLisp is a AutoLISP interpreter written in JavaScript to run generic AutoLISP code outside AutoCAD.

```
$ npm install
$ make
$ make test
```

```
$ node src/index.js examples/fib.lsp
55
```

```
$ make pkg
$ ls velisp*
velisp-0.0.1  velisp-0.0.1.exe
$ ./velisp-0.0.1 examples/fib.lsp
55
```
