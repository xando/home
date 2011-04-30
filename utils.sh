gconftool-2 --set "/apps/gnome-terminal/profiles/Default/scrollback_lines" --type integer "4096"
gconftool-2 --set "/apps/gnome-terminal/profiles/Default/font" --type string "Monospace 11"
gconftool-2 --set "/apps/gnome-terminal/profiles/Default/use_system_font" --type boolean "false"
gconftool-2 --set "/apps/gnome-terminal/profiles/Default/default_show_menubar" --type boolean "false"
gconftool-2 --set "/apps/gnome-terminal/profiles/Default/use_theme_background" --type boolean "false"
gconftool-2 --set "/apps/gnome-terminal/profiles/Default/use_theme_colors" --type boolean "false"
gconftool-2 --set "/apps/gnome-terminal/profiles/Default/background_type" --type string "solid"
gconftool-2 --set "/apps/gnome-terminal/profiles/Default/palette" --type string "#2E2E34343636:#CCCC00000000:#4E4E9A9A0606:#C4C4A0A00000:#34346565A4A4:#757550507B7B:#060698209A9A:#D3D3D7D7CFCF:#555557575353:#EFEF29292929:#8A8AE2E23434:#FCFCE9E94F4F:#72729F9FCFCF:#ADAD7F7FA8A8:#3434E2E2E2E2:#EEEEEEEEECEC"
gconftool-2 --set "/apps/gnome-terminal/profiles/Default/background_color" --type string "#000000000000"
gconftool-2 --set "/apps/gnome-terminal/profiles/Default/foreground_color" --type string "#BCADDF3FB750"
gconftool-2 --set "/apps/gnome-terminal/global/use_mnemonics" --type boolean "false"
gconftool-2 --set "/apps/gnome-terminal/global/use_menu_accelerators" --type boolean "false"

sudo apt-get install -y xclip xchat xbindkeys wmctrl unrar vlc gstreamer0.10-ffmpeg chromium-browser flashplugin-installer p7zip
