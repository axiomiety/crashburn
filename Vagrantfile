# -*- mode: ruby -*-
# vi: set ft=ruby :

# All Vagrant configuration is done below. The "2" in Vagrant.configure
# configures the configuration version (we support older styles for
# backwards compatibility). Please don't change it unless you know what
# you're doing.
Vagrant.configure(2) do |config|
  # The most common configuration options are documented and commented below.
  # For a complete reference, please see the online documentation at
  # https://docs.vagrantup.com.

  # Every Vagrant development environment requires a box. You can search for
  # boxes at https://atlas.hashicorp.com/search.
  config.vm.box = "ubuntu/wily64"

  # Disable automatic box update checking. If you disable this, then
  # boxes will only be checked for updates when the user runs
  # `vagrant box outdated`. This is not recommended.
  # config.vm.box_check_update = false

  # needed so i can access things like ipython notebook
  # and jekyll from the host!
  # note this shouldn't be necessary if your vm.network is public
  config.vm.network "forwarded_port", guest: 8080, host: 8080
  config.vm.network "forwarded_port", guest: 8088, host: 8088
  # for rdp
  config.vm.network "forwarded_port", guest: 9876, host: 9876

  # Create a private network, which allows host-only access to the machine
  # using a specific IP.
  # config.vm.network "private_network", ip: "192.168.33.10"

  # Create a public network, which generally matched to bridged network.
  # Bridged networks make the machine appear as another physical device on
  # your network.
  # config.vm.network "public_network"

  # Share an additional folder to the guest VM. The first argument is
  # the path on the host to the actual folder. The second argument is
  # the path on the guest to mount the folder. And the optional third
  # argument is a set of non-required options.
  config.vm.synced_folder "../shared", "/host_shared"

  config.vm.provider "virtualbox" do |vb|
    vb.gui = false
    vb.memory = "1024"
  end

  # i need this the first time around - otherwise vcsrepo in the puppet config file won't work
  # as for exercism, i should probably move that somewhere else!
  
  # also note that for exercism, you'll need to do an `exercism configure --key=API_KEY` which will create
  # .exercism.json in the home directory. more here: http://exercism.io/help
  config.vm.provision "shell", inline: <<-SHELL
    sudo puppet module install puppetlabs-vcsrepo
	mkdir bin
	cd bin
	wget https://raw.githubusercontent.com/exercism/cli-www/master/public/install
	chmod +x install
	DIR=/home/vagrant/bin ./install
	rm install
  SHELL
  
  #config.vm.provision "shell", path: "sh-provision.sh"
  config.vm.provision :puppet
end
