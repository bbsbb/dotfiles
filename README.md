# Dotfiles [++]

The repository is used to configure a development machine starting from a
minimal ubuntu server install. It includes gui and tooling as well as multiple roles for keeping
runtimes of various programming languages up to date.


## Requirements

* Clean, Ubuntu 20.04 **Server** minimal installation with ssh access, Ansible version >2.9 and GNU make.


## Usage

1. Create an inventories file called `live` in `inventories/` (see `examples/inventories/`)
2. Create a provisioning file `task`.yml at the root of the repository. (see `examples/template/`)
3. Execute:

```
    make with-target TARGET=<task>
```

For advanced usage lookup ansible var files.


## Available roles


### Utility

* **secure-ssh**
  - Trivial parameter changes to SSH according to configuration.
* **purge-snap**
  - Remove snap for good (ubuntu can still pull it via hard apt dependency)
* **zsh**
  - adjust default shell to zsh
* **gui-base**
  - xorg, openbox, lightdm, tint2 + utilities.
* **dev-base**
  - git, tmux, jq, ag, build-essential meta.
* **gui-extras**
  - firefox
* **docker**
  - Docker + docker-compose.


### Programming Languages

* **clojure**
  * OpenJDK - v11
  * Clojure Cli - v1.10.1.763
  * Lein - v2.9.4.
  * CLJ Kondo LSP - v2020.12.12

* **go**
  * Golang - v1.15.5
  * Gopls - Latest
  * Specifics - GOPATH set, gomodules on.

* **elixir**
  * Elixir - Latest packaged by ES

* **javascript**
  * NVM - v0.37.0

* **haskell**
  * GHC - 7.10.3
  * Cabal - 3.4

* **kotlin**
  * Kotlinc - 1.4.20

* **python**
  * Pipenv - Latest

* **racket**
  * Racket - v7.9

* **rust**
  * Rustup - v1.21.1
  * Rust analyzer - release 2020-11-13

## Editors

* **Emacs**
  * A Custom emacs configuration running on nightly.


## Disclaimer

This is a personal repository, synchronized between multiple machines and as such is often kept up to date
and there is no guarantee on the state of the master branch.


## License

MIT License
