## How to inspect types?

```
> (defun add (a b) (+ a b))
ADD
> .inspect 'add
Sym { sym: 'ADD' }
> .inspect add
Fun { name: 'ADD', params: [ 'A', 'B' ], locals: [], fun: [Function] }
> .inspect (add 1 2)
Int { int: 3 }
```

## How to inspect global context?

```
> .context
```

## How to run debugger?

```
$ node --inspect-brk src/main.js
```

## How to see VeLisp extensions?

```
$ grep 'VeLisp Extension' AutoLISP-Functions.md DCL-Functions.md
```

## How to see parse tree?

```
$ node src/main.js --tree
> (+ 1 2)
(file (expr ( (listExpr (expr +)) (listExpr (expr 1)) (listExpr (expr 2)) )))
```

## Environment variables

Various behaviors can be customized using the following environment variables:

* VELISP_REPL_HISTORY: When a valid path is given, persistent REPL history will be
saved to the specified file rather than .velisp_repl_history in the user's home
directory. Setting this value to '' (an empty string) will disable persistent
REPL history.
* VELISP_REPL_HISTORY_SIZE: Controls how many lines of history will be persisted
if history is available. Must be a positive number. Default: 1000.
