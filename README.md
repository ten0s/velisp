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
velisp  velisp.exe
$ ./velisp examples/fib.lsp
55
```
