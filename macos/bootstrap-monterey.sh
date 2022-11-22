#!/usr/bin/env bash

USER=vagrant
cd /Users/$USER/

CLT_VER=14.1
CLANG_VER=14.0.0

# Install Command Line Tools $CLT_VER
function install_clt() {
    hdiutil attach Command_Line_Tools_for_Xcode_$CLT_VER.dmg
    installer -package "/Volumes/Command Line Developer Tools/Command Line Tools.pkg" -target /
    hdiutil detach "/Volumes/Command Line Developer Tools"
}

if ! clang --version; then
    echo "Installing Command Line Tools $CLT_VER ..."
    install_clt
elif ! clang --version | grep "clang version $CLANG_VER"; then
    echo "Installing Command Line Tools $CLT_VER ..."
    install_clt
else
    echo "Command Line Tools $CLT_VER already installed"
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
su $USER -c "/bin/bash -s" <<'EOF'
   brew install node@16
   echo 'export PATH="/usr/local/opt/node@16/bin:$PATH"' >> ~/.zshrc
   echo 'export PATH="/usr/local/opt/node@16/bin:$PATH"' >> ~/.bashrc

   brew install jq
   brew install gobject-introspection gtk+3 cairo
EOF
