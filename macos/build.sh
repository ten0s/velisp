#!/usr/bin/env bash
set -Eeuo pipefail

make macosPackage

cp velisp-*-*-*.tar.gz /Users/vagrant/Shared/
