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
