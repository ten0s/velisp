#!/usr/bin/env bash
set -Eeuo pipefail

# Upgrade deps
brew upgrade
brew cache -u

make macosPackage

# velisp-*-*-*.tar.gz is rsynced back in ../build-macos-package.sh
