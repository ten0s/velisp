#!/usr/bin/env bash

USER=vagrant
cd /Users/$USER/

CLT_VER=12.5.1
# Install Command Line Tools $CLT_VER
if ! clang --version; then
    hdiutil attach Command_Line_Tools_for_Xcode_$CLT_VER.dmg
    installer -package "/Volumes/Command Line Developer Tools/Command Line Tools.pkg" -target /
    hdiutil detach "/Volumes/Command Line Developer Tools"
fi
rm Command_Line_Tools_for_Xcode_$CLT_VER.dmg

# Install Homebrew
if ! brew --version; then
    curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh > install-brew.sh
    chmod +x install-brew.sh
    NONINTERACTIVE=1 su $USER -c ./install-brew.sh
    rm ./install-brew.sh
fi

# Install deps
brew install \
     jq \
     node@16 \
     gobject-introspection \
     gtk+3 \
     cairo
