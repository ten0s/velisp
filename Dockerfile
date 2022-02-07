FROM ubuntu:20.04

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

RUN echo "Installing Node.js v12..."

# https://github.com/nodesource/distributions/blob/master/README.md#deb
RUN curl -fsSL https://deb.nodesource.com/setup_12.x | bash -
RUN apt-get -y install nodejs

RUN useradd --shell /bin/bash --create-home user
USER user
WORKDIR /home/user

RUN echo "Copying source code..."

RUN mkdir -p velisp
COPY grammar/          velisp/grammar/
COPY lib/              velisp/lib/
COPY src/              velisp/src/
COPY Makefile          velisp/
COPY package.json      velisp/
COPY package-lock.json velisp/

RUN echo "Building VeLisp..."

WORKDIR velisp
RUN make production && \
    make pkgLinux
