###
# stage defn

stage { 'prereqs':
	before => Stage['main'],
	}
  
stage { 'final':
  require => Stage['main'],
  }

###
# class defn
  
class dothisfirst {  
  exec { "apt-get update":
    path => "/usr/bin",
    }
  }

class packages {
  
  package { "tmux":
    ensure  => present,
    require => Exec["apt-get update"],
    }

  package { "git":
    ensure  => present,
    require => Exec["apt-get update"],
    }  
    
  package { "python3":
    ensure  => present,
    require => Exec["apt-get update"],
    }

  package { "python3-crypto":
    ensure  => present,
    require => Package["python3"],
    }
    
  package { "python3-gmpy2":
    ensure  => present,
    require => Package["python3"],
    }
    
  package { "python3-numpy":
    ensure  => present,
    require => Package["python3"],
    }

  package { "ipython3":
    ensure  => present,
    require => Package["python3"],
    }
    
  package { "ipython3-notebook":
    ensure  => present,
    require => Package["ipython3"],
    }
    
  package { "nodejs":
    ensure  => present,
    require => Exec["apt-get update"],
    }
    
  package { "ruby":
    ensure  => present,
    require => Exec["apt-get update"],
    }
    
  package { "rubygems-integration":
    ensure  => present,
    require => Package["ruby"],
    }

  package { 'jekyll':
      ensure   => 'installed',
      provider => 'gem',
      require => [Package["ruby"], Package["rubygems-integration"]],
    }
    
  package { "fish":
    ensure  => present,
    require => Exec["apt-get update"],
    }
    
  package { "erlang-base":
    ensure  => present,
    require => Exec["apt-get update"],
    }

  package { "golang":
    ensure  => present,
    require => Exec["apt-get update"],
    }
  }

class github {

  # maybe this should be cloned to a shared folder so
  # i can use something like pycharm?
  define fetch_github_repo($repo=undef) {
    vcsrepo { "/home/vagrant/${repo}":
      ensure   => latest,
      provider => git,
      require  => [ Package["git"] ],
      source   => "https://github.com/axiomiety/${repo}.git",
      revision => 'master',
      }
  }

  fetch_github_repo{'exercism-io':
    repo  => 'exercism-io',
    }
    
  fetch_github_repo{'axiomiety.github.io':
    repo  => 'axiomiety.github.io',
    }
    
  fetch_github_repo{'crashburn':
    repo  => 'crashburn',
    }
    
  fetch_github_repo{'setup':
    repo  => 'setup',
    }
  }

class dothislast {
  
	# copy the .dotfiles to where they should be
	exec {"copy_dot_files":
		command  => '/home/vagrant/setup/copy_dot_files.sh',
		path     => '/usr/local/bin/:/bin/',
    }

	# grab exercism cli
	#TODO: this requires golang 1.5!
	#exec {"exercism":
	#	command => 'export GOPATH=/home/vagrant/go-ws;go get github.com/exercism/cli/exercism',
	#	path    => '/usr/local/bin/:/bin/',
	#}

	# make sure ownership of everything in ~ is set to the vagrant user
	# it's root otherwise...
	exec {"chown_vagrant":
		command => 'chown -hR vagrant /home/vagrant/*',
		path    => '/usr/local/bin/:/bin/',
    }
  }

###
# assign stages to classes

class { 'dothisfirst':
  stage => prereqs,
  }

class { 'github':
  stage => main,
  }
  
class { 'dothislast':
  stage => final,
  }

include dothisfirst
include packages
include github
include dothislast
