# Dotfiles

## Install

- Install Nix from [Determinate Systems](https://determinate.systems/posts/determinate-nix-installer/)

```bash
# Clone the dotfiles
mkdir -p ~/src/github.com/mrwinton/ && cd "$_"
git clone git@github.com:mrwinton/dotfiles.git
nix run nix-darwin -- switch --flake ~/src/github.com/mrwinton/dotfiles
```

## Update

```bash
dotfiles-update
```

## Update Packages

```bash
# Update all
nix flake update

# Update <package>
nix flake update <package>
```

## Configure Local Git

```bash
cp ~/.config/git/config-local.sample ~/.config/git/config-local
sudo $EDITOR ~/.config/git/config-local
```

## Update Emacs Packages (straight)[https://github.com/radian-software/straight.el/issues/975]

``` bash
M-x straight-pull-recipe-repositories

# then reload init

M-x straight-normalize-all

M-x straight-pull-al
```
