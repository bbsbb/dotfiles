devenv: docker devutils clojure golang

secure-ssh:
	ansible-playbook --ask-become-pass -i initial-hosts development.yml --tags ssh

docker:
	ansible-playbook --ask-become-pass -i secure-hosts development.yml --tags docker

devutils:
	ansible-playbook --ask-become-pass -i secure-hosts development.yml --tags devutils

clojure:
	ansible-playbook --ask-become-pass -i secure-hosts development.yml --tags clojure

golang:
	ansible-playbook --ask-become-pass -i secure-hosts development.yml --tags golang

desktop:
	ansible-playbook --ask-become-pass -i secure-hosts desktop.yml
