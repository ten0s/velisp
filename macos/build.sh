#!/usr/bin/env bash
set -Eeuo pipefail

# Install deps
brew tap ten0s/velisp
brew install libgimacos
brew install slide
brew cache -u

make macosPackage

# velisp-*-*-*.tar.gz is rsynced back in ../build-macos-package.sh
