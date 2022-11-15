#!/usr/bin/env bash
set -Eeuo pipefail

make macosPackage

# velisp-*-*-*.tar.gz is rsynced back in ../build-macos-package.sh
