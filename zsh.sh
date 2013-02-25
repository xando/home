rm -rf $HOME/.zhistory
ln -f -s $(pwd)/.zhistory $HOME/.zhistory

rm -rf $HOME/.zsh_history
ln -f -s $(pwd)/.zsh_history $HOME/.zsh_history

rm -rf $HOME/.zshrc
ln -f -s $(pwd)/.zshrc $HOME/.zshrc

sudo apt-get install zsh

chsh -s /bin/zsh
