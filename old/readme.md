# Stumpwm rc files #
> This was my old configuration from when I was using archlinux (instead of
NixOS). It dates back from 2014 if I remember correclty.

## Fetch some dependencies
```shell
git submodule update --init --recursive
sbcl --noinform --non-interactive --eval "(ql:quickload '(xembed swank))"
sudo pacman -S gnome-keyring
```

## How to use ##

Put this repo in ~/.stumpwm.d/
and
```shell
cd ~
ln -s .stumpwm.d/stumpwmrc .stumpwmrc
```

Optionally (for gdm for example)
```shell
sudo ln -s <PATH-TO-STUMPWM> /usr/bin/stumpwm
cd ~
sudo ln -s $(readlink -f .stumpwm/stumpwm.desktop) /usr/share/xsession/
```

## Notes ##

1. `start.sh` is called by xinit (if I remember correclty) and setups the
environment then starts stumpwm.
2. stumpwm loads `~/.stumpwmrc`
3. `~/.stumpwmrc` contains some code for error-handling and logging. It tries
to load `~/.stumpwm.d/main.lisp`.
4. `main.lisp` tries to load the rest.

## Links ##

http://thtump.blogspot.ca/2012/03/beginning-stumpwm-configuration.html
http://en.wikipedia.org/wiki/User:Gwern/.stumpwmrc

MUST READ!!
http://thtump.blogspot.ca/2012/03/sudo-stumpish-way.html

