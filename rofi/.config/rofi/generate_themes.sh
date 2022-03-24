#!/usr/bin/env bash
mkdir -p ~/.config/rofi/themes
source ~/.colors
BACKGROUND=$background
BACKGROUND_ALT=$background_alt
FOREGROUND=$foreground
FOREGROUND_ALT=$foreground_alt
HIGHLIGHT=$highlight
SELECTED=$selected
ALERT=$alert

echo "* {
    background-color:           ${BACKGROUND};
    background-alt:             ${BACKGROUND_ALT};
    border-color:               ${BACKGROUND};
    text-color:                 ${FOREGROUND};
    selected-normal-background: ${SELECTED};
    selected-normal-foreground: ${FOREGROUND};
    height:                     22px;
    font:                       \"Cantarell 10\";
    prompt-font:                \"SauceCodePro Bold 16\";
    prompt-padding:             -2px 4px;
}
#window {
    anchor: north west;
    location: north west;
    width: calc(100% - 550px);
    padding: 0px 8px;
    children: [ horibox ];
}
#horibox {
    orientation: horizontal;
    children: [ prompt, entry, listview ];
}

#prompt {
    padding:          @prompt-padding;
    background-color: @prompt-background;
    text-color:       @prompt-foreground;
    font:             @prompt-font;
}
#listview {
    layout: horizontal;
    lines: 100;
}
#entry {
    background-color: @background-alt;
    padding: 2px 8px;
    margin: 0px 24px 0px 8px;
    expand: false;
    width: 224px;
}
#element {
    padding: 2px 10px 3px;
}
#element selected {
    background-color: @selected-normal-background;
    text-color:       @selected-normal-foreground;
}

element-text {
    background-color: inherit;
    text-color:       inherit;
}" > ~/.config/rofi/themes/dmenu.rasi

echo "* {
    background-color:           ${BACKGROUND};
    border-color:               ${BACKGROUND};
    text-color:                 ${FOREGROUND};
    prompt-background:          ${BACKGROUND_ALT};
    prompt-foreground:          ${FOREGROUND};
    selected-normal-background: ${SELECTED};
    selected-normal-foreground: ${FOREGROUND};
    height:                     22px;
    font:                       \"Source Code Pro Medium 10\";
    prompt-font:                \"Source Code Pro Bold 10\";
    prompt-padding:             2px;
}
#window {
    anchor: north;
    location: north;
    width: 100%;
    padding: 0px;
    children: [ horibox ];
}
#horibox {
    orientation: horizontal;
    children: [ prompt, entry, listview ];
}
#prompt {
    padding:          @prompt-padding;
    background-color: @prompt-background;
    text-color:       @prompt-foreground;
    font:             @prompt-font;
}
#listview {
    layout: horizontal;
    lines: 100;
}
#entry {
    padding: 2px;
    expand: false;
    width: 10em;
}
#element {
    padding: 2px 8px;
}
#element selected {
    background-color: @selected-normal-background;
    text-color:       @selected-normal-foreground;
}

element-text {
    background-color: inherit;
    text-color:       inherit;
}" > ~/.config/rofi/themes/center.rasi
