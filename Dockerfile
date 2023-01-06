FROM ubuntu:22.04

ARG GID=1000
ARG UID=1000
ARG USER=user

ENV TZ=UTC
ENV DEBIAN_FRONTEND=noninteractive

RUN echo "Installing deps..."

RUN apt-get update         &&  \
    apt-get -y install         \
        build-essential        \
        curl                   \
        git                    \
        gobject-introspection  \
        jq                     \
        libgirepository1.0-dev \
        libgtk-3-dev           \
        libcairo2              \
        libcairo2-dev          \
        make

ENV NODE_VERSION=16

RUN echo "Installing Node.js v${NODE_VERSION}..."

# https://github.com/nodesource/distributions/blob/master/README.md#deb
RUN curl -fsSL https://deb.nodesource.com/setup_${NODE_VERSION}.x | bash -
RUN apt-get -y install nodejs
RUN npm install -g npm pkg@5.8.0

RUN groupadd --gid $GID $USER
RUN useradd --shell /bin/bash --create-home --gid $GID --uid $UID $USER
USER $USER
WORKDIR /home/$USER

RUN echo "Copying source code..."

RUN mkdir -p velisp
COPY examples/                velisp/examples/
COPY grammar/                 velisp/grammar/
COPY lib/                     velisp/lib/
COPY src/                     velisp/src/
COPY patches/                 velisp/patches/
COPY util/                    velisp/util/
COPY Makefile                 velisp/
COPY package.json             velisp/
COPY package-lock.json        velisp/
COPY package.json.template    velisp/
COPY pkg.json                 velisp/
COPY rollup.config.js         velisp/
COPY LICENSE                  velisp/
COPY README-en-linux.template velisp/
COPY README-ru-linux.template velisp/

RUN echo "Building VeLisp..."

WORKDIR velisp
RUN make linuxPackage
