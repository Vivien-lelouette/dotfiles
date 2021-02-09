#!/bin/sh
# Run the screen compositor
compton &

# Fire it up
exec dbus-launch --exit-with-session ~/Tools/emacs/src/emacs -mm --debug-init --eval "(exwm-enable)"
