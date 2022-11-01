# Install Choco
#Set-ExecutionPolicy Bypass -Scope Process -Force; iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))

# Install VS 2017
choco install -y visualstudio2017-workload-vctools

# Install Inno Setup
choco install -y innosetup --version=6.2.1

# Install Node.js
choco install -y nodejs-lts --version=16.18.0
$Env:PATH = [Environment]::GetEnvironmentVariable('PATH', 'Machine')
npm config set msvs_version 2017

# Install MSYS2
choco install -y msys2 --params "/InstallDir:C:\\msys64"
C:\\msys64\\msys2_shell.cmd -defterm -no-start -mingw64 -c "pacman --noconfirm -S base-devel expect git mingw-w64-x86_64-jq mingw-w64-x86_64-gtk3 mingw-w64-x86_64-gobject-introspection mingw-w64-x86_64-cairo zip"
[Environment]::SetEnvironmentVariable('PATH', [Environment]::GetEnvironmentVariable('PATH', 'User') + ';C:\\msys64\\mingw64\\bin', 'User')
