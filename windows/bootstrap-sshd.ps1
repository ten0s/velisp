Get-WindowsCapability -Online | ? Name -like "OpenSSH*"
Add-WindowsCapability -Online -Name "OpenSSH.Server~~~~0.0.1.0"
New-NetFirewallRule -Protocol TCP -LocalPort 22 -Direction Inbound -Action Allow -DisplayName ssh
Get-Service | ? Name -like '*ssh*'
Start-Service -Name sshd
Set-Service -Name sshd -StartupType Automatic
