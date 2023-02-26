#!/usr/bin/env bash

USER=vagrant
cd /Users/$USER/

CLT_VER=${1?}
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

# Install build deps
su $USER -c "/bin/bash -s" <<'EOF'
   brew install               \
        cairo                 \
        coreutils             \
        gobject-introspection \
        gtk+3                 \
        jq                    \
        node@18
   brew tap ten0s/brew-cache
   brew tap ten0s/velisp
   brew install libgimacos
   brew install slide
   brew update
   brew upgrade
   brew cache -u

   echo 'alias ll="ls -l"'                                > ~/.zshrc
   echo 'export HOMEBREW_NO_AUTO_UPDATE=1'               >> ~/.zshrc
   echo 'export PATH="/usr/local/opt/node@18/bin:$PATH"' >> ~/.zshrc

   echo 'export HOMEBREW_NO_AUTO_UPDATE=1'                > ~/.bashrc
   echo 'export PATH="/usr/local/opt/node@18/bin:$PATH"' >> ~/.bashrc

   echo "Done"
EOF
