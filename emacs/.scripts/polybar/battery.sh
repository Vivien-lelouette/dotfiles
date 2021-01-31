#!/bin/sh
battery=$(emacsclient -e "(panel/polybar-battery)" | sed -e 's/^"\[//' -e 's/\..%\]"$//')

if [ $battery -le 100 ] && [ $battery -gt 95 ]
then
    echo "  ${battery}%"
elif [ $battery -le 95 ] && [ $battery -gt 50 ]
then
    echo "  ${battery}%"
elif [ $battery -le 50 ] && [ $battery -gt 25 ]
then
    echo "  ${battery}%"
elif [ $battery -le 25 ] && [ $battery -gt 2 ]
then
    echo "  ${battery}%"
else
    echo "  ${battery}%"
fi
