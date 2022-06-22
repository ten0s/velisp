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

Download Windows 10 Vagrant box from https://developer.microsoft.com/en-us/microsoft-edge/tools/vms/
and import it.

```
$ unzip MSEdge.Win10.Vagrant.zip
$ vagrant box add win10 'MSEdge - Win10.box'
```

Build Windows build deps box

```
$ export VAGRANT_VAGRANTFILE=Vagrantfile.win10.deps
$ vagrant up --provision
$ vagrant package --output velisp-win10-build-deps.box
$ vagrant box add -f velisp-win10-build-deps ./velisp-win10-build-deps.box
$ vagrant destroy -f
$ rm -rf .vagrant/ *.box
```

#### Build Windows package

```
$ ./build-windows-package.sh
...
Build Done
```

### Build on host (Linux only)

#### Build deps

```
$ sudo apt install jq make
```

#### [Node.js 16.x](https://nodejs.org/dist/latest-v16.x/)

```
$ curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash
$ nvm install 16
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
