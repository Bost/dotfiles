#!/bin/bash

alias less='less -r'
#alias rm='rm -i'
alias whence='type -a'
alias ls='ls -F --color=tty'
alias dir='ls --color=auto --format=vertical'
alias vdir='ls --color=auto --format=long'
alias ll='ls -l'
alias la='ls -A'
alias l='ls -CF'

#alias cp='cp -i'
#alias mv='mv -i'
#alias grep='grep --color'

uname=`uname -a`
strMinGw32="MINGW32_NT-5.2 COR-PARIS-DEV1"
strCygwin_dev1="CYGWIN_NT-5.2 cor-paris-dev1"
strCygwin_wsles347="CYGWIN_NT-5.1 wsles347"
strLinux="Linux"
#echo uname: $uname

isMinGw32=`expr match "$uname" "$strMinGw32"`
isCygwin_dev1=`expr match "$uname" "$strCygwin_dev1"`
isCygwin_wsles347=`expr match "$uname" "$strCygwin_wsles347"`
isLinux=`expr match "$uname" "$strLinux"`

#echo isCygwin_dev1: $isCygwin_dev1
#echo isCygwin_wsles347: $isCygwin_wsles347
#echo isMinGw32: $isMinGw32

if [ "$isCygwin_dev1" -gt 0 ]; then
	echo "### Loading $strCygwin_dev1 aliases"
	base=/cygdrive
	DEV=$base/d/users/svoboda/dev
	VIMRC=$base/h/.vimrc
	alias reload='echo "reloading ~/.bash_profile"; . ~/.bash_profile'
elif [ "$isCygwin_wsles347" -gt 0 ]; then
	echo "### Loading $strCygwin_wsles347 aliases"
	base=/cygdrive
	DEV=$base/d/dev

	#VIMRC=$ASE/h/.vimrc.msgit
	VIMRC=$base/h/.vimrc.dark
	alias reload='echo "reloading ~/.bash_profile"; . ~/.bash_profile'
elif [ "$isMinGw32" -gt 0 ]; then
	echo "### Loading $strMinGw32 aliases"
	base=
	DEV=$base/d/users/svoboda/dev

	#VIMRC=$ASE/h/.vimrc.msgit
	VIMRC=$base/h/.vimrc.dark
	alias reload='echo "reloading ~/.bash_profile"; . ~/.bash_profile'
elif [ "$isLinux" -gt 0 ]; then
	echo "### Loading $strLinux aliases"
	base=
	DEV=/home/bost/dev

	VIMRC=/home/bost/.vimrc
	SUDO_UPDATE="sudo apt-get update"
	alias ll="ls -la"
	alias update=$SUDO_UPDATE
	alias upd=$SUDO_UPDATE

	SUDO_UPGRADE="sudo apt-get upgrade"
	alias upgrade=$SUDO_UPGRADE
	alias upg=$SUDO_UPGRADE

	alias inst="sudo apt-get install"

	alias reload='echo "reloading ~/.bashrc"; source ~/.bashrc'
else
	echo "ERROR No environment detected"
fi

export qDrive=$base/q
export rosv=$qDrive/transfer/rosv

alias c:='cd $base/c'
alias d:='cd $base/d'
alias q:='cd $qDrive'

alias rosv='cd $rosv'
alias conf='cd $rosv/mbs/conf'
alias scl='cd $DEV/scl-directory'

alias dem='cd $DEV/mbs/dem'
alias mce='cd $DEV/mbs/mce'
alias std='cd $DEV/mbs/std'
alias bmw='cd $DEV/mbs/bmw'
alias mbb='cd $DEV/mbs/mbb'
alias all='cd $DEV/mbs/all.git'

alias jacarta='cd $DEV/jacarta'
alias hre='cd $DEV/jacarta.hre'

alias services='cd $DEV/xml-services'
alias      xml='cd $DEV/xml-services'

alias dev='cd $DEV'

VIM_CMD='vim -u $VIMRC'
GVIM_CMD='gvim -u $VIMRC'

alias vim=$VIM_CMD
alias vi=$VIM_CMD
alias v=$VIM_CMD
alias g=$GVIM_CMD

alias cls='clear'

alias aliases='vim ~/.bash_aliases'

alias png='ping -c 5 www.google.com'

alias c='cat'

#alias prod='echo ssh rsvoboda@172.17.31.184; ssh rsvoboda@172.17.31.184'
#alias abnt='echo ssh rsvoboda@172.17.31.185; ssh rsvoboda@172.17.31.185'
alias prod='echo ssh rsvoboda@194.99.105.205; ssh rsvoboda@194.99.105.205'
alias abnt='echo ssh rsvoboda@194.99.105.206; ssh rsvoboda@194.99.105.206'

alias less='less -r'
#alias rm='rm -i'
alias whence='type -a'
alias ls='ls -F --color=tty'
#alias ls='ls --color=auto'
alias dir='ls --color=auto --format=vertical'
#alias dir='dir --color=auto'
alias vdir='ls --color=auto --format=long'
#alias vdir='vdir --color=auto'
alias ll='ls -l'
alias l='ls -CF'

#alias l='ls -lAh'
alias ..='cd ..'
alias ...='cd ../..'
alias cdd='cd -'
alias ls='ls -G'

GIT_ADD_ALL="echo 'git add .'; git add ."
alias ga=$GIT_ADD_ALL
alias gadd=$GIT_ADD_ALL
alias gdd=$GIT_ADD_ALL
alias gg='git gui &'
alias gtg='git tag'
alias gtag='git tag'
alias gbr='git branch'
alias grbs='git rebase'
#alias gdf='git diff'
#alias gp='git push'
#alias gl='git pull'
alias gst='git status'
alias gco='git checkout'
alias gci='git commit'
#alias gdm='git diff master'
#alias gd='git diff | mate'
#alias gnp='git-notpushed'
#alias grm='git status | grep deleted | awk '

alias sc='script/console'
alias h='history'

#alias tu='top -o cpu'
#alias tm='top -o vsize'
#alias r='rake'
#alias g='git status'
#alias mkdir='nocorrect mkdir'
#alias cp='nocorrect cp'
alias md='mkdir -p'
#alias mv='nocorrect mv'
#alias e='exit'
#alias where='command -v'
#alias ff='open -a Firefox'
#alias o='open . &'
#alias ungit='find . -name '
#alias less='less -R'

#alias cp='cp -i'
#alias mv='mv -i'

# example:
# alias cvs-reset='cvs update -C -l -d -P "mbsgui/src/de/alldata/mbsgui/base/plugins/CreditKeyList.java" "mbsgui/src/de/alldata/mbsgui/base/plugins/CreditKeyList.java"'
alias cvs-reset='cvs update -C -l -d -P '

if [ "$isLinux" -eq 0 ]; then

	fDefvars=$rosv/mbs/deployments/defvars.sh
	#echo "fDefvars=$fDefvars"
	if [ -f "${fDefvars}" ]; then
		. "${fDefvars}"
		#echo "File loaded: ${fDefvars}"
	else
		echo "ERROR File not found: ${fDefvars}"
	fi
fi
#echo "HOME=$HOME"

