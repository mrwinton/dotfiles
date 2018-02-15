#!/bin/sh

## update and upgrage current packages
sudo apt-get update
sudo apt-get upgrade
sudo apt-get install build-essential

## set zsh as the shell of choice
sudo apt-get install zsh
chsh -s $(which zsh)
zsh
touch .zshrc

## symlink manager
sudo apt-get install stow

## code search tool
apt-get install silversearcher-ag

## front end package manager
curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -
echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list
sudo apt-get install yarn

## image editing tool
sudo apt-get install imagemagick

## dependency manager
git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.4.2
echo -e '\n. $HOME/.asdf/asdf.sh' >> ~/.zshrc
echo -e '\n. $HOME/.asdf/completions/asdf.bash' >> ~/.zshrc
# shellcheck disable=SC1090
source "$HOME/.asdf/asdf.sh"

## install asdf plugins
install_asdf_plugin() {
  local name="$1"
  local url="$2"

  if ! asdf plugin-list | grep -Fq "$name"; then
    asdf plugin-add "$name" "$url"
  fi
}
install_asdf_plugin "ruby" "https://github.com/asdf-vm/asdf-ruby.git"
install_asdf_plugin "nodejs" "https://github.com/asdf-vm/asdf-nodejs.git"

## install asdf languages
install_asdf_language() {
  local language="$1"
  local version
  version="$(asdf list-all "$language" | tail -1)"

  if ! asdf list "$language" | grep -Fq "$version"; then
    asdf install "$language" "$version"
    asdf global "$language" "$version"
  fi
}
install_asdf_language "ruby"
install_asdf_language "nodejs"

# install gems
gem_install_or_update() {
  if gem list "$1" --installed > /dev/null; then
    gem update "$@"
  else
    gem install "$@"
  fi
}
gem update --system
gem_install_or_update "bundler"
gem_install_or_update "awesome_print"
