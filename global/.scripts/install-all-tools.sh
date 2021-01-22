#!/bin/sh
for script in $(find ~/.scripts/*/ -name install.sh) ; do sh $script ; done
