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
strMinGw32_dev1="MINGW32_NT-5.2 COR-PARIS-DEV1"
strCygwin_dev1="CYGWIN_NT-5.2 cor-paris-dev1"
strCygwin_wsles347="CYGWIN_NT-5.1 wsles347"
strMinGw32_wsles347="MINGW32_NT-5.1 WSLES347"
strMinGw32_app1="MINGW32_NT-5.2 COR-ALLDM-APP1"
strLinux="Linux"
strCygwin_155="CYGWIN_NT-6.1-WOW64 LE-C-155"
strMinGw32_155="MINGW32_NT-6.1 LE-C-155"
#echo uname: $uname

isMinGw32_dev1=`expr match "$uname" "$strMinGw32_dev1"`
isCygwin_dev1=`expr match "$uname" "$strCygwin_dev1"`
isCygwin_wsles347=`expr match "$uname" "$strCygwin_wsles347"`
isLinux=`expr match "$uname" "$strLinux"`
isMinGw32_wsles347=`expr match "$uname" "$strMinGw32_wsles347"`
isMinGw32_app1=`expr match "$uname" "$strMinGw32_app1"`
isCygwin_155=`expr match "$uname" "$strCygwin_155"`
isMinGw32_155=`expr match "$uname" "$strMinGw32_155"`

#echo isMinGw32_155: $isMinGw32_155
#echo isCygwin_dev1: $isCygwin_dev1
#echo isCygwin_wsles347: $isCygwin_wsles347
#echo isMinGw32_dev1: $isMinGw32_dev1
#echo isMinGw32_wsles347: $isMinGw32_wsles347
#echo isMinGw32_app1: $strMinGw32_app1

alias reload='echo "reloading ~/.bash_profile"; . ~/.bash_profile'

if [ "$isCygwin_dev1" -gt 0 ]; then
	echo "### Loading $strCygwin_dev1 aliases"
	base=/cygdrive
	dev=$base/d/users/svoboda/dev
	export winapp=$base/d/users/svoboda/winapp
	VIMRC=$base/h/.vimrc

	alias clojure='java -cp $winapp/clojure-1.4.0/clojure-1.4.0.jar clojure.main'
elif [ "$isCygwin_155" -gt 0 ]; then
	echo "### Loading $strCygwin_155"
	base=/cygdrive
	dev=$base/c/cygwin/home/svo02896/dev
	export winapp=$base/c/winapp
	#VIMRC=$base/h/.vimrc.dark

	#alias clojure='$winapp/clojure-1.4.0/clojure.sh'
elif [ "$isMinGw32_155" -gt 0 ]; then
	echo "### Loading $strMinGw32_155"
	base=
	dev=$base/c/cygwin/home/svo02896/dev
	export winapp=$base/c/winapp
	#VIMRC=$base/h/.vimrc.dark

	#alias clojure='$winapp/clojure-1.4.0/clojure.sh'
elif [ "$isCygwin_wsles347" -gt 0 ]; then
	echo "### Loading $strCygwin_wsles347 aliases"
	base=/cygdrive
	dev=$base/d/dev
	export winapp=$base/d/winapp
	VIMRC=$base/h/.vimrc.dark

	alias clojure='$winapp/clojure-1.4.0/clojure.sh'
elif [ "$isMinGw32_dev1" -gt 0 ]; then
	echo "### Loading $strMinGw32_dev1 aliases"
	base=
	dev=$base/d/users/svoboda/dev
	export winapp=$base/d/users/svoboda/winapp
	#VIMRC=$ASE/h/.vimrc.msgit
	#VIMRC=$base/h/.vimrc.dark
	VIMRC=$base/h/.vimrc

	alias clojure='java -cp $winapp/clojure-1.4.0/clojure-1.4.0.jar clojure.main'
elif [ "$isMinGw32_app1" -gt 0 ]; then
	echo "### Loading $strMinGw32_app1 aliases"
	base=
	dev=$base/d/users/svoboda/dev
	export winapp=$base/d/users/svoboda/winapp
	#VIMRC=$ASE/h/.vimrc.msgit
	# this works just well
	VIMRC=/d/winapp/Vim/_vimrc

	alias clojure='java -cp $winapp/clojure-1.4.0/clojure-1.4.0.jar clojure.main'
elif [ "$isMinGw32_wsles347" -gt 0 ]; then
	echo "### Loading $strMinGw32_wsles347 aliases"
	base=
	dev=$base/d/dev
	export winapp=$base/d/winapp
	VIMRC=$base/h/.vimrc.dark

	#alias clojure='$winapp/clojure-1.4.0/clojure.sh'
elif [ "$isLinux" -gt 0 ]; then
	echo "### Loading $strLinux aliases"
	base=
	dev=$HOME/dev
	VIMRC=$HOME/.vimrc
	SUDO_UPDATE="sudo apt-get update"
	alias ll="ls -la"
	alias update=$SUDO_UPDATE
	alias upd=$SUDO_UPDATE

	SUDO_UPGRADE="sudo apt-get upgrade"
	alias upgrade=$SUDO_UPGRADE
	alias upg=$SUDO_UPGRADE

	alias inst="sudo apt-get install"
	alias reload='echo "reloading ~/.bashrc"; source ~/.bashrc'
	alias clojure='java -cp $dev/clojure-1.4.0/clojure-1.4.0.jar clojure.main'
	alias kk="krusader --left . --right . &"
else
	echo "ERROR No environment detected"
fi

if [ "$isLinux" -eq 0 ]; then
	export qDrive=$base/q
	export rosv=$qDrive/transfer/rosv

	alias c:='cd $base/c'
	alias d:='cd $base/d'
	alias q:='cd $qDrive'

	alias rosv='cd $rosv'
	alias conf='cd $rosv/mbs/conf'
	alias scl='cd $dev/scl-directory'

	alias dem='cd $dev/mbs/dem'
	alias mce='cd $dev/mbs/mce'
	alias std='cd $dev/mbs/std'
	alias bmw='cd $dev/mbs/bmw'
	alias mbb='cd $dev/mbs/mbb'
	alias all='cd $dev/mbs/all.git'

	alias jacarta='cd $dev/jacarta'
	alias hre='cd $dev/jacarta.hre'
	alias pfandbrief='cd $dev/pfandbrief'

	alias services='cd $dev/xml-services'
	alias xml='cd $dev/xml-services'

	#alias prod='echo ssh rsvoboda@172.17.31.184; ssh rsvoboda@172.17.31.184'
	#alias abnt='echo ssh rsvoboda@172.17.31.185; ssh rsvoboda@172.17.31.185'
	alias prod='echo ssh rsvoboda@194.99.105.205; ssh rsvoboda@194.99.105.205'
	alias abnt='echo ssh rsvoboda@194.99.105.206; ssh rsvoboda@194.99.105.206'
fi

alias webcli='cd $dev/webcli'
alias cheatsheet='cd $dev/cheatsheet'
alias dev='cd $dev'

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
alias gau='git add --update'
alias gss='git status --short'
alias gst='git status'
alias gco='git checkout'
alias gci='git commit'
#alias gdm='git diff master'
#alias gd='git diff | mate'
#alias gnp='git-notpushed'
#alias grm='git status | grep deleted | awk '

alias sc='script/console'
alias h='history'
alias hrep='history | grep '

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

function take {
	mkdir $1
	cd $1
}

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

