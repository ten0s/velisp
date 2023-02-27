#!/usr/bin/env bash
set -Eeuo pipefail

# Upgrade deps
brew update
brew upgrade libgimacos
brew upgrade slide
brew cache -u

make macosPackage

# velisp-*-*-*.tar.gz is rsynced back in ../build-macos-package.sh
