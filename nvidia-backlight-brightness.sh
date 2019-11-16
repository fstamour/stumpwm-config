#!/bin/sh

current_brightness=$(nvidia-settings -n -q BacklightBrightness | grep Attribute | sed -re 's/.*: ([0-9]+)./\1/g')

case $1 in
  up)
    target_brightness=$(expr $current_brightness + 10)
    ;;
  down)
    target_brightness=$(expr $current_brightness - 10)
    ;;
  *)
    echo "invalid argument \"${1}\", must be one of \"up\" or \"down\"."
    exit 1
  ;;
esac

echo ===

min() {
  echo $(( $1 < $2 ? $1 : $2 ))
}

max() {
  echo $(( $1 > $2 ? $1 : $2 ))
}

target_brightness=$(min 100 $(max 0 $target_brightness))

nvidia-settings -n -a BacklightBrightness=$target_brightness
