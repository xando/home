# emacs with emacs-starter-k
sudo add-apt-repository ppa:ubuntu-elisp/ppa
sudo apt-get install -y git-core
sudo apt-get install -y emacs

git clone https://github.com/gabrielelanaro/emacs-starter-kit.git $HOME/.emacs.d

# symbolic links
ln -f -s $(pwd)/emacs/custom.el $HOME/.emacs.d/custom.el
