# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|

  config.vm.box = "win10"

  config.vm.guest = :windows
  config.vm.communicator = "winrm"
  config.vm.boot_timeout = 600 # 10 mins

  config.winrm.username = "IEUser"
  config.winrm.password = "Passw0rd!"
  config.winrm.timeout = 600 # 10 mins

  config.ssh.username="IEUser"
  config.ssh.password="Passw0rd!"
  config.ssh.insert_key = false

  config.vm.box_check_update = false

  config.vm.network :forwarded_port, guest: 5985, host: 5985, id: "winrm", auto_correct: true

  config.vm.provider "virtualbox" do |vb|
    vb.gui = true
    vb.cpus = 2
    vb.memory = 4096
  end

  config.vm.provision "shell",
    binary: true,
    privileged: true,
    path: "./windows/bootstrap.ps1"

  config.vm.provision "file", source: "./examples",              destination: "C:\\msys64\\home\\IEUser\\velisp\\examples"
  config.vm.provision "file", source: "./grammar",               destination: "C:\\msys64\\home\\IEUser\\velisp\\grammar"
  config.vm.provision "file", source: "./lib",                   destination: "C:\\msys64\\home\\IEUser\\velisp\\lib"
  config.vm.provision "file", source: "./patches",               destination: "C:\\msys64\\home\\IEUser\\velisp\\patches"
  config.vm.provision "file", source: "./src",                   destination: "C:\\msys64\\home\\IEUser\\velisp\\src"
  config.vm.provision "file", source: "./test",                  destination: "C:\\msys64\\home\\IEUser\\velisp\\test"
  config.vm.provision "file", source: "./Makefile",              destination: "C:\\msys64\\home\\IEUser\\velisp\\"
  config.vm.provision "file", source: "./package.json",          destination: "C:\\msys64\\home\\IEUser\\velisp\\"
  config.vm.provision "file", source: "./package-lock.json",     destination: "C:\\msys64\\home\\IEUser\\velisp\\"
  config.vm.provision "file", source: "./package.json.template", destination: "C:\\msys64\\home\\IEUser\\velisp\\"
  config.vm.provision "file", source: "./rollup.config.js",      destination: "C:\\msys64\\home\\IEUser\\velisp\\"
  config.vm.provision "file", source: "./windows/build.sh",      destination: "C:\\msys64\\home\\IEUser\\velisp\\"

end
