mkdir -p ~/.emacs.d/exwm/
echo "[Desktop Entry]
  Name=EXWM
  Comment=Emacs Window Manager
  Exec=sh $(pwd ~)/.emacs.d/exwm/start-exwm.sh
  TryExec=sh
  Type=Application
  X-LightDM-DesktopName=exwm
  DesktopNames=exwm" > ~/.emacs.d/exwm/EXWM.desktop

sudo ln -f ~/.emacs.d/exwm/EXWM.desktop /usr/share/xsessions/EXWM.desktop
