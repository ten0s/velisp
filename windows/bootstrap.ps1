# Enable SSH Server
Get-WindowsCapability -Online | ? Name -like "OpenSSH*"
Add-WindowsCapability -Online -Name "OpenSSH.Server~~~~0.0.1.0"
New-NetFirewallRule -Protocol TCP -LocalPort 22 -Direction Inbound -Action Allow -DisplayName ssh
Get-Service | ? Name -like '*ssh*'
Start-Service -Name sshd
Set-Service -Name sshd -StartupType Automatic

# Install Choco
#Set-ExecutionPolicy Bypass -Scope Process -Force; iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))

# Install VS 2017
choco install -y visualstudio2017-workload-vctools

# Install Node.js
choco install -y nodejs.install --version=16.15.1
$Env:PATH = [Environment]::GetEnvironmentVariable('PATH', 'Machine')
npm config set msvs_version 2017

# Install MSYS2
choco install -y msys2 --params "/InstallDir:C:\\msys64"
C:\\msys64\\msys2_shell.cmd -defterm -no-start -mingw64 -c "pacman --noconfirm -S base-devel expect git mingw-w64-x86_64-jq mingw-w64-x86_64-gtk3 mingw-w64-x86_64-gobject-introspection mingw-w64-x86_64-cairo"
[Environment]::SetEnvironmentVariable('PATH', [Environment]::GetEnvironmentVariable('PATH', 'User') + ';C:\\msys64\\mingw64\\bin', 'User')

# Make Mingw64 SSH default shell https://www.msys2.org/wiki/Setting-up-SSHd/
'@set HOME=' | Out-File -FilePath C:\\msys64\\sshd_default_shell.cmd -Encoding ASCII
'@C:\\msys64\\msys2_shell.cmd -defterm -full-path -no-start -mingw64' | Out-File -FilePath C:\\msys64\\sshd_default_shell.cmd -Encoding ASCII -Append
New-ItemProperty -Path "HKLM:\\SOFTWARE\\OpenSSH" -Name DefaultShell -Value 'C:\\msys64\\sshd_default_shell.cmd' -PropertyType String -Force
