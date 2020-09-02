all: compile

compile:
	antlr4 -Dlanguage=JavaScript -lib grammar -visitor -no-listener grammar/AutoLISP.g4

eval:
	npm run eval

treejs:
	npm run tree

compileJava:
	antlr4 -lib grammar -no-visitor -no-listener grammar/AutoLISP.g4
	javac grammar/*.java

tokens: compileJava
	(cd grammar; grun AutoLISP module -tokens)

gui: compileJava
	(cd grammar; grun AutoLISP module -gui)

tree: compileJava
	(cd grammar; grun AutoLISP module -tree)

clean:
	rm -f grammar/*.class grammar/*.java
