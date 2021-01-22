#!/bin/sh
# Run the screen compositor
compton &

# Fire it up
exec dbus-launch --exit-with-session emacs -mm --debug-init -l ~/.emacs.d/desktop.el
