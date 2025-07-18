## How to inspect types?

```
_$ (defun add (a b) (+ a b))
ADD
_$ .inspect 'add
Sym { sym: 'ADD' }
_$ .inspect add
UFun {
  name: 'ADD',
  params: [ 'A', 'B' ],
  locals: [],
  fun: [Function (anonymous)]
}
_$ .inspect (add 1 2)
Int { int: 3 }
```

## How to inspect global context?

```
_$ .context
```

## How to run debugger?

```
$ node --inspect-brk src/main.js
```

Now run Google Chrome, open Developer tools, open dedicated DevTools for Node.js

## How to see VeLisp extensions?

```
$ grep 'VeLisp Extension' AutoLISP-Functions.md DCL-Functions.md
```

## How to see [Glade](https://glade.gnome.org/) XML for DCL?

```
$ VELISP_DEBUG=glade node src/main.js examples/calc 2>calc.xml
$ glade calc.xml
```

## How to see full stacktrace?

```
$ node src/main.js --eval '("abc" 1 2)'
Error: "abc": function not defined
    at __EVAL__:1
```

```
$ VELISP_DEBUG=fulltrace node src/main.js --eval '("abc" 1 2)'
Error: "abc": function not defined
    at __EVAL__:1
    ...
```

## How to see parse tree?

```
$ VELISP_DEBUG=tree node src/main.js --eval '(+ 1 2)'
(file (expr ( (listExpr (expr +)) (listExpr (expr 1)) (listExpr (expr 2)) )))
```

```
$ VELISP_DEBUG=tree node src/main.js
_$ (+ 1 2)
(file (expr ( (listExpr (expr +)) (listExpr (expr 1)) (listExpr (expr 2)) )))
```

## Environment variables

Various behaviors can be customized using the following environment variables:

* VELISP_DEBUG=glade | fulltrace | tree | libs | help
* VELISP_REPL_HISTORY: When a valid path is given, persistent REPL history will be
saved to the specified file rather than .velisp_repl_history in the user's home
directory. Setting this value to '' (an empty string) will disable persistent
REPL history.
* VELISP_REPL_HISTORY_SIZE: Controls how many lines of history will be persisted
if history is available. Must be a positive number. Default: 1000.
