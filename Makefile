.PHONY: install test

BRANCH := ${shell git branch --no-color | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'}
VERSION := ${shell jq -r .version package.json}
NODE := ${shell node --version | sed -E 's/v([0-9]+)\..*/node\1/'}

all: install

install:
	npm install
	$(MAKE) applyPatches

installDev:
	npm clean-install
	$(MAKE) applyPatches

installProd:
	npm clean-install --omit=dev
	$(MAKE) applyPatches

applyPatches:
	patches/apply-patches.sh

rebuildNodeGtk:
	$(MAKE) applyPatches
	(cd node_modules/node-gtk; npx node-pre-gyp configure build --debug)

linuxPackage:
	$(MAKE) installDev
	$(MAKE) prePkg
	$(MAKE) installProd
	$(MAKE) rebuildNodeGtk
	$(MAKE) pkgLinux
	$(MAKE) tarLinux

windowsPackage:
	$(MAKE) installDev
	$(MAKE) prePkg
	$(MAKE) installProd
	$(MAKE) rebuildNodeGtk
	$(MAKE) pkgWindows

macosPackage:
	$(MAKE) installDev
	$(MAKE) prePkg
	$(MAKE) installProd
	$(MAKE) rebuildNodeGtk
	$(MAKE) pkgMacos
	$(MAKE) tarMacos

grammar:
	antlr4 -Dlanguage=JavaScript -lib grammar -visitor -no-listener grammar/VeLisp.g4
	antlr4 -Dlanguage=JavaScript -lib grammar -no-visitor -listener grammar/VeDcl.g4

run:
	node src/main.js

tree:
	node src/main.js --run tree

test: qunit expect-test vl-unit

qunit:
	npx qunit

vl-unit:
	node src/main.js --no-dcl lib/test.lsp

expect-test:
	test/expect-test.sh

lint:
	npx eslint src/{*,*/*}.js test/*.js

check: lint test

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
	git tag "${VERSION}"
	git push origin master --tags

readme:
	sed -E -e "s/\{\{branch\}\}/${BRANCH}/g" -e "s/\{\{version\}\}/${VERSION}/g" README.template > README.md

prePkg:
	mkdir -p pkg/src/
	sed -E -e "s/\{\{version\}\}/${VERSION}/g" package.json.template > pkg/package.json
	npx rollup -c

pkgLinux:
	# make prePkg MUST be run first
	npx pkg -c pkg.json -t ${NODE}-linux-x64 -o velisp pkg/src/main.js

pkgWindows:
	npx pkg -c pkg.json -t ${NODE}-win-x64 -o velisp pkg/src/main.js

pkgMacos:
	npx pkg -c pkg.json -t ${NODE}-macos -o velisp pkg/src/main.js

tarLinux:
	mkdir -p        velisp-${VERSION}-linux-x64/
	cp velisp       velisp-${VERSION}-linux-x64/
	cp -r lib/      velisp-${VERSION}-linux-x64/
	cp -r examples/ velisp-${VERSION}-linux-x64/
	tar cfz velisp-${VERSION}-linux-x64.tar.gz velisp-${VERSION}-linux-x64/


tarMacos:
	mkdir -p        velisp-${VERSION}-macos-x64/
	cp velisp       velisp-${VERSION}-macos-x64/
	cp -r lib/      velisp-${VERSION}-macos-x64/
	cp -r examples/ velisp-${VERSION}-macos-x64/
	tar cfz velisp-${VERSION}-macos-x64.tar.gz velisp-${VERSION}-macos-x64/

cleanPkg:
	rm -rf pkg/
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

cleanDeps:
	rm -rf node_modules/

clean: cleanDeps cleanJava cleanPkg
