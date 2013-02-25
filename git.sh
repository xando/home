sudo apt-get install -y git-core

# symbolic links
rm -rf $HOME/.gitconfig
ln -f -s $(pwd)/git/.gitconfig $HOME/.gitconfig
rm -rf $HOME/.gitignore
ln -f -s $(pwd)/git/.gitconfig $HOME/.gitignore
