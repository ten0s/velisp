# Install Choco
#Set-ExecutionPolicy Bypass -Scope Process -Force; iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))

# Install VS 2017
choco install -y visualstudio2017-workload-vctools

# Install Inno Setup
choco install -y innosetup --version=6.2.2

# Install Node.js
choco install -y nodejs-lts --version=20.19.3
npm install -g npm @yao-pkg/pkg@6.5.1
$Env:PATH = [Environment]::GetEnvironmentVariable('PATH', 'Machine')

# Install MSYS2
choco install -y msys2 --params "/InstallDir:C:\\msys64"
C:\\msys64\\msys2_shell.cmd -defterm -no-start -mingw64 -c "pacman --noconfirm -Syu"
C:\\msys64\\msys2_shell.cmd -defterm -no-start -mingw64 -c "pacman --noconfirm -S base-devel expect git mingw-w64-x86_64-gcc mingw-w64-x86_64-jq mingw-w64-x86_64-gtk3 mingw-w64-x86_64-gobject-introspection mingw-w64-x86_64-cairo zip"
