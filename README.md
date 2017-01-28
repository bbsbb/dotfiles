# Configuration & Provisioning of an Ubuntu 16.04 Desktop/Devenv

I got annoyed with my mac enough to move back to linux. It felt about time to automate a part of the setup.
The repository contains the following sections.

## Development environment & tools.

It brings docker, git, jq, httpie, pip, ag, tmux, zsh, powerline, emacs and customized configuration for some of the above.

## Usage

All you need is ansible/ssh installed on an Ubuntu 16.04 target.
If you are familiar with ansible, just use the command line to run whatever you like(localhost etc).
If not, stick to the makefile.

* `git clone https://github.com/bbsbb/dotfiles.git` and cd to the root folder.
* Create a file called initial-hosts with the following contents
```
[desktop_machines]
<insert ip of the target machine running ssh+ansible>
```
* Open `vars/development-params.yml` and adjust as you like.
* run `make secure-ssh`
* Create another file, called secure-hosts with the following contents
```
[desktop_machines]
<same ip as above>  ansible_port=4423
```
* Run `make docker` to install docker.
* Run `make clojure` for openjdk + lein.
* Run `make golang` for go + some utils.

## License

Copyright © 2016
Distributed under the MIT License
