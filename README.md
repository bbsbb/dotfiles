# Dotfiles [++]

The repository is used to configure a development machine starting from a
minimal ubuntu server install. It includes gui and tooling as well as multiple roles for keeping
runtimes of various programming languages up to date.


## Machines

The following repository currently synchronizes the development experience
between 3 machiens:

* Desktop 5600x, 64GB, RTX 3800, Ubuntu Server 20.04
* Laptop Macbook Air M1 16GB - 8GB Qemu VM, headless Ubuntu Server 20.04 aarch64


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
  - Trivial parameter changes to SSH according to configuration. Enables collecting remote forward binds.
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
  - Docker
* **docker-compose**
  - Docker-compose


### Programming Languages w/ Tooling

* **clojure**
  * [Clojure](https://clojure.org/) - v1.11.1.1182
  * [CLJ Kondo](https://github.com/clj-kondo/clj-kondo) LSP - v2022.11.02

* **go**
  * [Go](https://go.dev/) - v1.19.3
  * [Gopls](https://github.com/golang/tools/tree/master/gopls) LSP - Head

* **elixir**
  * [Elixir](https://elixir-lang.org/) - Latest packaged by ES

* **haskell**
  * [GHC](https://www.haskell.org/ghc/) - 8.8.2 (shipping with LTS)
  * [Cabal](https://www.haskell.org/cabal/) - 3.6

* **java**
  * JVM 11/13 LTS (packaged openjdk)
  * [jenv](https://github.com/jenv/jenv) - 0.5.4

* **javascript**
  * [nvm](https://github.com/nvm-sh/nvm) - 0.39.0

* **kotlin**
  * [Kotlin](https://kotlinlang.org/) - 1.6.0

* **python**
  *[pyenv](https://github.com/pyenv/pyenv)** - (HEAD, no release tracking)

* **racket**
  * [Racket](https://racket-lang.org/) - v7.9

* **rust**
  * [Rustup](https://rustup.rs/) - v1.25.1
  * [Rust analyzer](2021-11-22) - release 2022-11-07

* **scala**
  * [Scala 2](https://www.scala-lang.org/) - 2.13.5
  * [sbt](https://www.scala-sbt.org/) - 1.4.7

## Editors

* **Emacs**
  * A Custom emacs configuration w/ LSP running on nightly.


## Diverse

* [Yubikey](https://www.yubico.com/)

The zsh configuration contains commented out gpg section and sshd_config is pre-configured to accept gpg-agent
forwarding(useful for Host -> Guest only, otherwise insecure).


## Disclaimer

This is a personal repository, synchronized between multiple machines and as such is often kept up to date
and there is no guarantee on the state of the master branch.


## License

MIT License
