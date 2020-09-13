all: compile

compile:
	antlr4 -Dlanguage=JavaScript -lib grammar -visitor -no-listener grammar/VeLisp.g4

test:
	npm run test

tree:
	npm run tree

pkg: pkgLinux pkgWin

pkgLinux:
	npm run pkg -- -t node10-linux-x64 -o velisp-`jq -r .version package.json`

pkgWin:
	npm run pkg -- -t node10-win-x64 -o velisp-`jq -r .version package.json`

cleanPkg:
	rm -f velisp*

compileJava:
	antlr4 -lib grammar -no-visitor -no-listener grammar/VeLisp.g4
	javac grammar/*.java

tokens: compileJava
	(cd grammar; grun VeLisp file -tokens)

gui: compileJava
	(cd grammar; grun VeLisp file -gui)

treeJava: compileJava
	(cd grammar; grun VeLisp file -tree)

cleanJava:
	rm -f grammar/*.class grammar/*.java

clean: cleanJava cleanPkg
