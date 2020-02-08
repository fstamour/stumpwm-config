#!/bin/sh

# sudo ln -f $(readlink -f <THIS>) /usr/bin/stumpwm

GDK_CORE_DEVICE_EVENTS=1
export GDK_CORE_DEVICE_EVENTS

# eval `ssh-agent`
# OR
eval `gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh`
export GNOME_KEYRING_CONTROL
export SSH_AUTH_SOCK
export GPG_AGENT_INFO
export GNOME_KEYRING_PID

# The version from (ql:quickload :stumpwm)
# ~/quicklisp/dists/quicklisp/software/stumpwm-20140914-git/stumpwm

# MY version :P
~/quicklisp/local-projects/stumpwm/stumpwm

