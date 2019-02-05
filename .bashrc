# .bashrc

export TERM=xterm-256color

# User specific aliases and functions

#alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

alias gits='git status'

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

source /opt/intel/bin/ifortvars.sh intel64
source /opt/intel/composer_xe_2013_sp1/bin/compilervars.sh intel64
export PATH=$PATH:/opt/intel/bin/
export PATH=$PATH:/opt/mpich2-install/bin
export PATH=$PATH:/opt/packmol

export DISPLAY=192.168.113.201:0.0

#export g09root=/home/mori/g09/src
#export g09root=/opt
#    . $g09root/g09/bsd/g09.profile
#export GAUSS_SCRDIR=/work/naito
#export PATH=$PATH:${g09root}/g09

#export QSAC_DIR=/home/naito/Qsac/
#export PATH=$PATH:/home/mori/bin/

# Set colors for ls command.
eval `dircolors ~/.dir_colors -b`
