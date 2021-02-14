.PHONY: test

BRANCH := ${shell git branch --no-color | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'}
VERSION := ${shell jq -r .version package.json}

all: install compile

install:
	npm install

compile:
	antlr4 -Dlanguage=JavaScript -lib grammar -visitor -no-listener grammar/VeLisp.g4
	antlr4 -Dlanguage=JavaScript -lib grammar -no-visitor -listener grammar/VeDcl.g4

run:
	node src/main.js

tree:
	node src/main.js --run tree

test:
	npx qunit
	node src/main.js lib/test.lsp

lint:
	npx eslint {src,test}/{*,*/*}.js

rollMinor:
	npm --no-git-tag-version version minor
	$(MAKE) roll

rollPatch:
	npm --no-git-tag-version version patch
	$(MAKE) roll

roll:
	$(MAKE) readme
	git add package.json package-lock.json README.md
	git commit -m "Roll ${VERSION}"
	echo git tag -a "${VERSION}" -m "Roll ${VERSION}"
	echo git push origin master --tags

readme:
	sed -E -e "s/\{\{branch\}\}/${BRANCH}/g" -e "s/\{\{version\}\}/${VERSION}/g" README.template > README.md

pkgLinux:
	npx pkg -c package.json -t node10-linux-x64 -o velisp-${VERSION}-linux-x64 src/main.js

pkgWin86:
	# https://github.com/vercel/pkg-fetch/issues/68
	npx pkg -c package.json -t node10-win-x86 -o velisp-${VERSION}-win-x86 --no-bytecode --public --public-packages '*' src/main.js

pkgWin64:
	npx pkg -c package.json -t node10-win-x64 -o velisp-${VERSION}-win-x64 src/main.js

pkgMacOS:
	npx pkg -c package.json -t node10-macos -o velisp-${VERSION}-macos-x64 src/main.js

cleanPkg:
	rm -f velisp*

compileJava:
	antlr4 -lib grammar -no-visitor -no-listener grammar/VeLisp.g4
	antlr4 -lib grammar -no-visitor -no-listener grammar/VeDcl.g4
	javac grammar/*.java

tokensLisp: compileJava
	(cd grammar; grun VeLisp file -tokens)

tokensDcl: compileJava
	(cd grammar; grun VeDcl file -tokens)

guiLisp: compileJava
	(cd grammar; grun VeLisp file -gui)

guiDcl: compileJava
	(cd grammar; grun VeDcl file -gui)

treeLispJava: compileJava
	(cd grammar; grun VeLisp file -tree)

treeDclJava: compileJava
	(cd grammar; grun VeDcl file -tree)

cleanJava:
	rm -f grammar/*.class grammar/*.java

clean: cleanJava cleanPkg
