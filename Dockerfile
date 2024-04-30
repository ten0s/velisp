FROM ubuntu:20.04

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
        libboost-all-dev       \
        libgirepository1.0-dev \
        libgtk-3-dev           \
        libcairo2              \
        libcairo2-dev          \
        make

ENV NODE_VERSION=18

RUN echo "Installing Node.js v${NODE_VERSION}..."

# https://github.com/nodesource/distributions/blob/master/README.md#deb
RUN curl -fsSL https://deb.nodesource.com/setup_${NODE_VERSION}.x | bash -
RUN apt-get -y install nodejs
RUN npm install -g npm pkg@5.8.1

RUN groupadd --gid $GID $USER
RUN useradd --shell /bin/bash --create-home --gid $GID --uid $UID $USER
USER $USER
WORKDIR /home/$USER

RUN echo "Copying source code..."

RUN mkdir -p velisp
COPY examples/                velisp/examples/
COPY grammar/                 velisp/grammar/
COPY lib/                     velisp/lib/
COPY linux/                   velisp/linux/
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

RUN mkdir -p velisp/bin
RUN mkdir -p velisp/lib64/girepository-1.0

RUN echo "Building slide..."
RUN git clone -b 0.10.1 https://github.com/ten0s/slide && \
    cd slide                                           && \
    make                                               && \
    make install                                       && \
    cp -r install/bin/* ../velisp/bin/                 && \
    cp -r install/lib/* ../velisp/lib64/               && \
    cd ..

RUN echo "Building VeLisp..."
WORKDIR velisp
RUN make linuxPackage
