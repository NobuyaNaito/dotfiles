# .bashrc

export TERM=xterm-256color

# User specific aliases and functions

#alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

alias gits='git status'

nas() {
  if [ "$#" -eq 0 ]
  then
    cd /mnt/share/save/
    cd naito
  else
    cp -r $1 /mnt/share/save/naito/$2
  fi
}

home2() {
  if [ "$#" -eq 0 ]
  then
    cd /mnt/home2/naito
  else
    cp -r $1 /mnt/home2/naito/$2
  fi
}

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

# TeX
export MANPATH=/usr/local/texlive/2018/texmf-dist/doc/man:$MANPATH
export INFOPATH=/usr/local/texlive/2018/texmf-dist/doc/info:$INFOPATH
export PATH=/usr/local/texlive/2018/bin/x86_64-linux:$PATH
#Add /usr/local/texlive/2018/texmf-dist/doc/man to MANPATH.
#Add /usr/local/texlive/2018/texmf-dist/doc/info to INFOPATH.
#Most importantly, add /usr/local/texlive/2018/bin/x86_64-linux
#to your PATH for current and future sessions.

# Set colors for ls command.
eval `dircolors ~/.dir_colors -b`
