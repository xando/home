# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=10000
HISTFILESIZE=15000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[01;37m\]`git branch 2> /dev/null | grep -e ^* | sed -E s/^\\\\\*\ \(.+\)$/\(git:\\\\\1\)\/`\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w`git branch 2> /dev/null | grep -e ^* | sed -E  s/^\\\\\*\ \(.+\)$/\(git:\\\\\1\)\/`\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

alias o=xdg-open

#Python virtualenv, and virtualenv wrapper settings
VIRTUALENV_WRAPPER="/usr/local/bin/virtualenvwrapper.sh"

if [ -f $VIRTUALENV_WRAPPER ]; then
    export WORKON_HOME=$HOME/.virtualenvs

    if [ ! -d $WORKON_HOME ]; then
	mkdir $WORKON_HOME
    fi

   export PIP_VIRTUALENV_BASE=$WORKON_HOME
   export PIP_REQUIRE_VIRTUALENV=true
   export PIP_RESPECT_VIRTUALENV=true
   source $VIRTUALENV_WRAPPER
fi

# Default editor is emacs now!
EDITOR=/usr/bin/emacs

# Run emacs in background always!
emacs () {
    $EDITOR $* &
}

DROPBOX=$HOME/Dropbox

has_virtualenv() {
    if [ ! -n "$VIRTUAL_ENV" ]; then
        VIRTUALENV=${PWD##*/}
        if [ -d "$WORKON_HOME/$VIRTUALENV" ]; then
            workon $VIRTUALENV
        fi
    fi
}

venv_cd () {
    cd "$@" && has_virtualenv
}
 
alias cd="venv_cd"
has_virtualenv

# source $DROPBOX/home/rotate.sh

#
# xbindkeys
# wmctrl

# INFO=$(wmctrl -d)
# DW=$(echo "${INFO}"| awk '{sub(/x[0-9]+/, "", $4); print $4}')
# WW=$(echo "${INFO}"| awk '{sub(/x[0-9]+/, "", $9); print $9}')
# NF=$(($DW/$WW))
# CVPX=$(echo "${INFO}" |awk '{sub(/,[0-9]+/, "", $6); print $6}')
# CVPN=$(( ${CVPX} / ${WW} ))

# function workspace_next() {

#     ACT=$((${CVPN} + 1))
#     TVPN=$(( $ACT % ${NF} ))

#   # The X coordinate of the target viewport
#     TVPX=$(( ${TVPN} * ${WW} ))

#   # Change to the target viewport
#     wmctrl -o ${TVPX},0
# }

# function workspace_previous() {
#     ACT=$((${CVPN} - 1))
#     TVPN=$(( $ACT % ${NF} ))

#   # The X coordinate of the target viewport
#     TVPX=$(( ${TVPN} * ${WW} ))

#   # Change to the target viewport
#     wmctrl -o ${TVPX},0
# }

# xchat
# empathy
# .emacs

# pip bash completion start
_pip_completion()
{
    COMPREPLY=( $( COMP_WORDS="${COMP_WORDS[*]}" \
                   COMP_CWORD=$COMP_CWORD \
                   PIP_AUTO_COMPLETE=1 $1 ) )
}
complete -o default -F _pip_completion pip
# pip bash completion end

_django_completion()
{
    COMPREPLY=( $( COMP_WORDS="${COMP_WORDS[*]}" \
                   COMP_CWORD=$COMP_CWORD \
	               DJANGO_AUTO_COMPLETE=1 $1 ) )
}
complete -F _django_completion -o default django-admin.py manage.py django-admin

_python_django_completion()
{
    if [[ ${COMP_CWORD} -ge 2 ]]; then
        PYTHON_EXE=$( basename -- ${COMP_WORDS[0]} )
        echo $PYTHON_EXE | egrep "python([2-9]\.[0-9])?" >/dev/null 2>&1
        if [[ $? == 0 ]]; then
            PYTHON_SCRIPT=$( basename -- ${COMP_WORDS[1]} )
            echo $PYTHON_SCRIPT | egrep "manage\.py|django-admin(\.py)?" >/dev/null 2>&1
            if [[ $? == 0 ]]; then
                COMPREPLY=( $( COMP_WORDS="${COMP_WORDS[*]:1}" \
                               COMP_CWORD=$(( COMP_CWORD-1 )) \
                               DJANGO_AUTO_COMPLETE=1 ${COMP_WORDS[*]} ) )
            fi
        fi
    fi
}

# Support for multiple interpreters.
unset pythons
if command -v whereis &>/dev/null; then
    python_interpreters=$(whereis python | cut -d " " -f 2-)
    for python in $python_interpreters; do
        pythons="${pythons} $(basename -- $python)"
    done
    pythons=$(echo $pythons | tr " " "\n" | sort -u | tr "\n" " ")
else
    pythons=python
fi

complete -F _python_django_completion -o default $pythons

export GREP_OPTIONS="--color=always"
export GREP_COLOR='1;31'
