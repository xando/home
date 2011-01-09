## elementary theme
sudo add-apt-repository ppa:elementaryart/ppa
sudo add-apt-repository ppa:am-monkeyd/nautilus-elementary-ppa
sudo apt-get update
sudo apt-get install -y elementary-theme elementary-icon-theme elementary-wallpapers

gconftool-2 --set "/apps/metacity/general/button_layout" --type string ":minimize,maximize,close"

