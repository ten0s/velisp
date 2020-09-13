all: compile

compile:
	antlr4 -Dlanguage=JavaScript -lib grammar -visitor -no-listener grammar/VeLisp.g4

test:
	npm run test

tree:
	npm run tree

compileJava:
	antlr4 -lib grammar -no-visitor -no-listener grammar/VeLisp.g4
	javac grammar/*.java

tokens: compileJava
	(cd grammar; grun VeLisp file -tokens)

gui: compileJava
	(cd grammar; grun VeLisp file -gui)

treeJava: compileJava
	(cd grammar; grun VeLisp file -tree)

clean:
	rm -f grammar/*.class grammar/*.java
