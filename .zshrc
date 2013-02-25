setopt no_nomatch
setopt clobber
setopt extended_glob

export EDITOR=emacs

setopt INC_APPEND_HISTORY


export HISTSIZE=10000
export SAVEHIST=$HISTSIZE
export HISTFILE="$HOME/.history"
setopt hist_ignore_all_dups

[[ -e /usr/local/bin/virtualenvwrapper.sh ]] && source /usr/local/bin/virtualenvwrapper.sh

function emacs() {
    setsid /usr/bin/emacs-snapshot-gtk -mm $* 2> /dev/null &
}

function ticket() {
    python -c "import sys; print '$*'.lower().replace(' ','_')"
}


upsearch() {
    test / == "$PWD" && return || \
	test -f "$1" && echo "$PWD/$1" && return || \
	cd .. && upsearch "$1"
}

has_virtualenv() {
    VIRTUALENV_FILE=`upsearch .virtualenv`
    if [ $VIRTUALENV_FILE ]
    then
    	VIRTUALENV_NAME=$(cat $VIRTUALENV_FILE)
    	if [ -d "$WORKON_HOME/$VIRTUALENV_NAME" ]; then
	    if [ -z $VIRTUAL_ENV ] || [ $VIRTUAL_ENV -a $(basename "$VIRTUAL_ENV") != $VIRTUALENV_NAME ]; then
		workon $VIRTUALENV_NAME
	    fi
	else
	    VIRTUALENV_REQUIREMENTS="$PWD/requirements.txt"
	    if [ -f $VIRTUALENV_REQUIREMENTS ]; then
		mkvirtualenv $VIRTUALENV_NAME -r $VIRTUALENV_REQUIREMENTS
	    else
		mkvirtualenv $VIRTUALENV_NAME
	    fi
	    site_packages="`virtualenvwrapper_get_site_packages_dir`"
	    ln -s `virtualenvwrapper_get_site_packages_dir` "$PWD/.site-packages"
        fi
    else
	if [ $VIRTUAL_ENV ]; then
	    deactivate
	fi
    fi
}

function venv_cd () {
    builtin cd $@ && has_virtualenv
}
 
alias cd="venv_cd"

function git_branch {
    local ref="$(git symbolic-ref HEAD 2> /dev/null)"

    if [[ -n "$ref" ]]; then
	echo "%F{red}git%f:%F{green}${ref#refs/heads/}%f "
	return 0
    else
	return 1
    fi
}

function current_dir() {
    echo '%F{cyan}%~%f '
}

setopt PROMPT_SUBST
PROMPT=' $(current_dir)$(git_branch)> '

bindkey ';5D' emacs-backward-word
bindkey ';5C' emacs-forward-word
bindkey ';3D' emacs-backward-word
bindkey ';3C' emacs-forward-word
export WORDCHARS='*?[]~=&;!#$%^(){}'

# General Alias
alias o='xdg-open'

alias ln='nocorrect ln -i'
alias mv='nocorrect mv -i'
alias cp='nocorrect cp -i'

alias ls='ls --color --group-directories-first -v'
alias ll='ls -lA'
alias l='ll'

# Git Alias
alias g='git'

git_log_format_medium='--pretty=format:%C(bold)Commit:%C(reset) %C(green)%H%C(red)%d%n%C(bold)Author:%C(reset) %C(cyan)%an <%ae>%n%C(bold)Date:%C(reset)   %C(blue)%ai (%ar)%C(reset)%n%+B'
git_log_format_oneline='--pretty=format:%C(green)%h%C(reset) %s%n'
git_log_format_brief='--pretty=format:%C(green)%h%C(reset) %s%n%C(blue)(%ar by %an)%C(red)%d%C(reset)%n'

alias gl='git log --topo-order ${git_log_format_medium}'
alias gls='git log --topo-order --stat ${git_log_format_medium}'
alias gld='git log --topo-order --stat --patch --full-diff ${git_log_format_medium}'
alias glo='git log --topo-order ${git_log_format_oneline}'
alias glg='git log --topo-order --all --graph ${git_log_format_oneline}'
alias glb='git log --topo-order ${git_log_format_brief}'
alias glc='git shortlog --summary --numbered'

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

has_virtualenv


### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
