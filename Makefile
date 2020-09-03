all: compile

compile:
	antlr4 -Dlanguage=JavaScript -lib grammar -visitor -no-listener grammar/AutoLISP.g4

test:
	npm run test

eval:
	npm run eval

tree:
	npm run tree

compileJava:
	antlr4 -lib grammar -no-visitor -no-listener grammar/AutoLISP.g4
	javac grammar/*.java

tokens: compileJava
	(cd grammar; grun AutoLISP file -tokens)

gui: compileJava
	(cd grammar; grun AutoLISP file -gui)

treeJava: compileJava
	(cd grammar; grun AutoLISP file -tree)

clean:
	rm -f grammar/*.class grammar/*.java
