## Development

### Build in Docker

In order to build **VeLisp** you need to have
[Docker](https://docs.docker.com/installation/#installation) with [non-root access](https://docs.docker.com/engine/installation/linux/linux-postinstall/)
installed.

```
$ ./build-in-docker.sh
...
Build Done
```

### Build on host

#### Build deps

```
$ sudo apt install jq make
```

#### [Node.js 14.x](https://nodejs.org/dist/latest-v14.x/)

```
$ curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.0/install.sh | bash
$ nvm install 14
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
