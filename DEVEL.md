## Development

### Build Linux package in Docker

In order to build **VeLisp** Linux package you need to have
[Docker](https://docs.docker.com/installation/#installation) with [non-root access](https://docs.docker.com/engine/installation/linux/linux-postinstall/)
installed.

```
$ ./build-linux-package.sh
...
Build Done
```


### Build Windows package in Vagrant

In order to build **VeLisp** Windows package you need to have
[VirtualBox](https://www.virtualbox.org/wiki/Downloads) and
[Vagrant](https://www.vagrantup.com/downloads)
installed.

#### Build Windows build deps Vagrant box

Download Windows 10 Vagrant box from
https://developer.microsoft.com/en-us/microsoft-edge/tools/vms/
and import it.

```
$ unzip MSEdge.Win10.Vagrant.zip
$ vagrant box add win10 'MSEdge - Win10.box'
```

#### Build Windows build deps box

```
$ export VAGRANT_VAGRANTFILE=Vagrantfile.win10.deps
$ vagrant up --provision
```

#### Build Windows package

```
$ ./build-windows-package.sh
...
Build Done
```

#### Upgrade MinGW64 deps

Inside the MSYS shell (Run as administrator)

```
# gflags -i node.exe +sls
# windows/find-mingw64-deps.sh node.exe src/main.js examples/calc.lsp
# gflags -i node.exe -sls
```

See https://ten0s.github.io/blog/2022/07/25/find-dlls-and-typelibs-dependencies-for-nodejs-gtk-application-on-windows for detail.


### Build MacOS package in Vagrant

In order to build **VeLisp** MacOS package you need to have
[VirtualBox](https://www.virtualbox.org/wiki/Downloads) and
[Vagrant](https://www.vagrantup.com/downloads)
installed.

#### Build MacOS build deps Vagrant box

Download MacOS Catalina Vagrant box from
https://app.vagrantup.com/jakubknejzlik/boxes/macos
and import it.

```
$ curl -s https://app.vagrantup.com/jakubknejzlik/boxes/macos | jq .
$ wget https://vagrantcloud.com/jakubknejzlik/boxes/macos/versions/11.2.3/providers/virtualbox.box
$ vagrant box add macos-catalina-11.2.3 virtualbox.box
```

#### Download Command Line Tools for Xcode

Download Command_Line_Tools_for_Xcode_12.5.1.dmg and copy it to macos/ directory

#### Build MacOS build deps box

```
$ export VAGRANT_VAGRANTFILE=Vagrantfile.macos.deps
$ export MACOS_NAME=big_sur
$ vagrant up
... some manual setup?
$ vagrant up --provision
```

#### Build MacOS package

```
$ ./build-macos-package.sh
...
Build Done
```

#### Upgrade Homebrew deps

Inside MacOS Terminal

```
% macos/find-homebrew-deps.sh ./velisp examples/calc.lsp
```


### Build on host (Linux only)

#### Build deps

```
$ sudo apt install jq make
```

#### [Node.js 18.x](https://nodejs.org/dist/latest-v18.x/)

```
$ curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.2/install.sh | bash
$ nvm install 18
```

#### [GTK+3](https://www.gtk.org/) deps

```
$ sudo apt-get install build-essential libgtk-3-dev gobject-introspection libgirepository1.0-dev libcairo2 libcairo2-dev
```

#### [ANTLR](https://www.antlr.org/)

[ANTLR](https://www.antlr.org/) is only needed for making changes in the grammars:

* [VeLisp.g4](/grammar/VeLisp.g4)
* [VeDcl.g4](/grammar/VeDcl.g4)

#### Build

```
$ make
```

#### Run tests

```
$ make test
```

#### Re-build grammars

```
$ make grammar
```
