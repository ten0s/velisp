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
$ npm start -- --run tree
> (+ 1 2)
(file (expr ( (listExpr (expr +)) (listExpr (expr 1)) (listExpr (expr 2)) )))
```
