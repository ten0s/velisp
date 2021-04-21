## How to run debugger?

```
$ node --inspect-brk src/main.js
```

## How to see VeLisp extensions?

```
$ grep -A1 'VeLisp Extension' {lib,src}/**/*
```

## How to see parse tree?

```
$ npm start -- --run tree
> (+ 1 2)
(file (expr ( (listExpr (expr +)) (listExpr (expr 1)) (listExpr (expr 2)) )))
```
