BRANCH := ${shell git branch --no-color | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'}
VERSION := ${shell jq -r .version package.json}
NODE := ${shell node --version | sed -E 's/v([0-9]+)\..*/node\1/'}
ISCC := "/c/Program Files (x86)/Inno Setup 6/ISCC.exe"

all: install

.PHONY: install
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
	$(MAKE) prepareWindows
	$(MAKE) zipWindows
	$(MAKE) installerWindows

macosPackage:
	$(MAKE) installDev
	$(MAKE) prePkg
	$(MAKE) installProd
	$(MAKE) rebuildNodeGtk
	$(MAKE) pkgMacos
	$(MAKE) tarMacos

.PHONY: grammar
grammar:
	antlr4 -Dlanguage=JavaScript -lib grammar -visitor -no-listener grammar/VeLisp.g4
	antlr4 -Dlanguage=JavaScript -lib grammar -no-visitor -listener grammar/VeDcl.g4

run:
	node src/main.js

tree:
	node src/main.js --run tree

.PHONY: test
test: qunit expect-test vl-unit

qunit:
	npx qunit

vl-unit:
	node src/main.js --no-dcl lib/test.lsp

expect-test:
	test/expect-test.sh

lint:
	npx eslint src/{*,*/*}.js test/*.js util/*.js

check: lint test

rollPreRelease:
	npm --no-git-tag-version version prerelease
	$(MAKE) roll

rollMinor:
	npm --no-git-tag-version version minor
	$(MAKE) roll

rollPatch:
	npm --no-git-tag-version version patch
	$(MAKE) roll

roll:
	$(MAKE) readme
	git add package.json package-lock.json README.md README-ru.md
	@echo 'Now run: $$ make commit-push'

commit-push:
	git commit -m "Roll ${VERSION}"
	git tag "${VERSION}"
	git push origin `git branch --show-current 2>/dev/null` --tags

readme:
	sed -E -e "s/\{\{branch\}\}/${BRANCH}/g" -e "s/\{\{version\}\}/${VERSION}/g" README.template    > README.md
	sed -E -e "s/\{\{branch\}\}/${BRANCH}/g" -e "s/\{\{version\}\}/${VERSION}/g" README-ru.template > README-ru.md
	sed -E -e "s/\{\{version\}\}/${VERSION}/g" README-en-linux.template   > README-en-linux.md
	sed -E -e "s/\{\{version\}\}/${VERSION}/g" README-en-windows.template > README-en-windows.md
	sed -E -e "s/\{\{version\}\}/${VERSION}/g" README-en-macos.template   > README-en-macos.md
	sed -E -e "s/\{\{version\}\}/${VERSION}/g" README-ru-linux.template   > README-ru-linux.md
	sed -E -e "s/\{\{version\}\}/${VERSION}/g" README-ru-windows.template > README-ru-windows.md
	sed -E -e "s/\{\{version\}\}/${VERSION}/g" README-ru-macos.template   > README-ru-macos.md

prePkg:
	$(MAKE) cleanPkg
	mkdir -p pkg/src/
	rm -rf pkg/src/*
	sed -E -e "s/\{\{version\}\}/${VERSION}/g" package.json.template > pkg/package.json
	npx rollup -c

pkgLinux:
	# make prePkg MUST be run first
	npx pkg -c pkg.json -t ${NODE}-linux-x64 -o velisp pkg/src/main.js

pkgWindows:
	npx pkg -c pkg.json -t ${NODE}-win-x64 -o velisp pkg/src/main.js

pkgMacos:
	npx pkg -c pkg.json -t ${NODE}-macos-x64 -o velisp pkg/src/main.js

tarLinux:
	rm -rf          velisp-${VERSION}-linux-x64/
	mkdir -p        velisp-${VERSION}-linux-x64/
	cp velisp       velisp-${VERSION}-linux-x64/velisp.bin
	cp linux/velisp velisp-${VERSION}-linux-x64/
	cp -r lib/      velisp-${VERSION}-linux-x64/
	cp -r lib64/    velisp-${VERSION}-linux-x64/
	cp -r examples/ velisp-${VERSION}-linux-x64/
	cp LICENSE      velisp-${VERSION}-linux-x64/
	cp bin/slide-info velisp-${VERSION}-linux-x64/
	node util/notice-nodejs.js node_modules/ > velisp-${VERSION}-linux-x64/NOTICE
	sed -E -e "s/\{\{version\}\}/${VERSION}/g" README-en-linux.template > velisp-${VERSION}-linux-x64/README-en.md
	sed -E -e "s/\{\{version\}\}/${VERSION}/g" README-ru-linux.template > velisp-${VERSION}-linux-x64/README-ru.md
	tar cfJ velisp-${VERSION}-linux-x64.tar.xz velisp-${VERSION}-linux-x64/

prepareWindows:
	rm -rf                  velisp-${VERSION}-win-x64/
	mkdir -p                velisp-${VERSION}-win-x64/
	cp velisp.exe           velisp-${VERSION}-win-x64/
	cp windows/noprompt.vbs velisp-${VERSION}-win-x64/
	cp -r lib/              velisp-${VERSION}-win-x64/
	cp -r examples/         velisp-${VERSION}-win-x64/
	cp LICENSE              velisp-${VERSION}-win-x64/
	cp /mingw64/bin/slide-info.exe velisp-${VERSION}-win-x64/
	windows/copy-mingw64-deps.sh   velisp-${VERSION}-win-x64/
	node util/notice-nodejs.js node_modules/                            > velisp-${VERSION}-win-x64/NOTICE
	echo -e "\n--------------------------------\n"                     >> velisp-${VERSION}-win-x64/NOTICE
	node util/notice-mingw64.js velisp-${VERSION}-win-x64/mingw64/bin/ >> velisp-${VERSION}-win-x64/NOTICE
	sed -E -e "s/\{\{version\}\}/${VERSION}/g" README-en-windows.template > velisp-${VERSION}-win-x64/README-en.md
	sed -E -e "s/\{\{version\}\}/${VERSION}/g" README-ru-windows.template > velisp-${VERSION}-win-x64/README-ru.md

zipWindows:
	zip -r velisp-${VERSION}-win-x64.zip velisp-${VERSION}-win-x64/

installerWindows:
	sed -E -e "s/\{\{version\}\}/${VERSION}/g" windows/installer.iss | ${ISCC} //O"." //F"velisp-${VERSION}-win-x64-setup" -
	# Zip to upload to GH Releases
	zip velisp-${VERSION}-win-x64-setup.zip velisp-${VERSION}-win-x64-setup.exe

tarMacos:
	rm -rf          velisp-${VERSION}-macos-x64/
	mkdir -p        velisp-${VERSION}-macos-x64/
	cp velisp       velisp-${VERSION}-macos-x64/Velisp.bin
	cp macos/velisp velisp-${VERSION}-macos-x64/
	cp -R lib       velisp-${VERSION}-macos-x64/
	cp -R examples  velisp-${VERSION}-macos-x64/
	cp LICENSE      velisp-${VERSION}-macos-x64/
	cp /usr/local/bin/slide-info velisp-${VERSION}-macos-x64/
	macos/copy-homebrew-deps.sh  velisp-${VERSION}-macos-x64/
	node util/notice-nodejs.js node_modules/                                > velisp-${VERSION}-macos-x64/NOTICE
	echo "\n--------------------------------\n"                            >> velisp-${VERSION}-macos-x64/NOTICE
	node util/notice-homebrew.js velisp-${VERSION}-macos-x64/homebrew/lib/ >> velisp-${VERSION}-macos-x64/NOTICE
	sed -E -e "s/\{\{version\}\}/${VERSION}/g" README-en-macos.template > velisp-${VERSION}-macos-x64/README-en.md
	sed -E -e "s/\{\{version\}\}/${VERSION}/g" README-ru-macos.template > velisp-${VERSION}-macos-x64/README-ru.md
	tar cfJ velisp-${VERSION}-macos-x64.tar.xz velisp-${VERSION}-macos-x64/

sha256sum:
	sha256sum velisp-${VERSION}-linux-x64.tar.xz velisp-${VERSION}-macos-x64.tar.xz velisp-${VERSION}-win-x64.zip velisp-${VERSION}-win-x64-setup.zip | tee velisp-${VERSION}-sha256sum.txt

update-copyright:
	find . -name 'LICENSE' \
       -or -name '*.js'    \
       -or -name '*.lsp'   \
       -or -name '*.g4'    \
       | xargs -I{} sed -i 's/2022 Dmitry Klionsky/2022-2023 Dmitry Klionsky/' {}

cleanPkg:
	rm -rf pkg/
	rm -rf velisp*

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
