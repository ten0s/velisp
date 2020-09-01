all: compile

compile:
	antlr4 -Dlanguage=JavaScript -lib grammar -visitor -no-listener grammar/AutoLisp.g4

run: compile
	node eval.js

compileJava:
	antlr4 -lib grammar -no-visitor -no-listener grammar/AutoLisp.g4
	javac grammar/*.java

tokens: compileJava
	(cd grammar; grun AutoLisp module -tokens)

gui: compileJava
	(cd grammar; grun AutoLisp module -gui)

tree: compileJava
	(cd grammar; grun AutoLisp module -tree)

clean:
	rm -f grammar/*.class grammar/*.java
