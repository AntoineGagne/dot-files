# Programs

alias python='python3.4'


# Processes

alias htop='htop -u twain'
alias meminfo='free -m -l -t'
alias cpuinfo='lscpu'

## Get top processes eating memory

alias psmem='ps auxf | sort -nr -k 4'
alias psmem10='ps auxf | sort -nr -k 4 | head -10'

## Get top processes eating cpu

alias pscpu='ps auxf | sort -nr -k 3'
alias pscpu10='ps auxf | sort -nr -k 3 | head -10'


# Storage

alias df='df -hT'
alias du='du -sh'


# Movements

alias cdu='cd ~/Documents/Universite'
alias cdp='cd ~/Documents/Programmation'
alias ..='cd ../'
alias ...='cd ../../'
alias ....='cd ../../../'
alias .....='cd ../../../../'


# Terminal History

alias h='history'


# Vim

alias vim='nvim'
alias vimd='nvim -dR'
alias svi='sudo nvim'
alias edit='nvim'


# Laptop Power

alias shutdown='sudo shutdown -h now'
alias reboot='sudo shutdown -r now'
alias hibernate='sudo pm-hibernate'
alias suspend='sudo pm-suspend'


# Time

alias cal='cal -A 11'
alias now='date +"%T"'
alias nowtime=now
alias nowdate='date +"%d-%m-%Y"'


# Networking

alias ping='ping -c 5'
alias fastping='ping -c 100 -s.2'
alias browser=firefox

alias ports='netstat -tulanp'

## Firewall

alias ipt='sudo /sbin/iptables'
 
alias iptlist='sudo /sbin/iptables -L -n -v --line-numbers'
alias iptlistin='sudo /sbin/iptables -L INPUT -n -v --line-numbers'
alias iptlistout='sudo /sbin/iptables -L OUTPUT -n -v --line-numbers'
alias iptlistfw='sudo /sbin/iptables -L FORWARD -n -v --line-numbers'
alias firewall=iptlist

## Web Download

alias wget='wget -c'


# Files

alias sdiff='sdiff -ZEB'
alias rm='rm -I --preserve-root'
alias rsync='rsync --ignore-existing --progress -rz'


# Git

alias oneline='git log --pretty=oneline'
alias graph='git log --pretty=full --date=relative --graph --color=auto --numstat'
alias gdiff='git diff --color=auto --summary --stat'
alias grrm='git remote rm'
alias grshow='git remote -v'
alias gradd='git remote add' # To add an existing repository, `git remote add origin <url>`


# Text

alias fold='fold -w 80 -s'              # Formats a file to respect 80 character
alias format='fmt -w 80 -u -s -t -c'    # Formats a file to respect 80 character
alias rmtabs='expand -t 4'              # Replaces tabs by spaces
alias rmspaces='unexpand -a -t 4'       # Replaces spaces by tabs
alias look='look -f'                    # Finds a word and ignores its letter case


# Superuser

alias sudo='sudo '


# PDF

alias pdfrmencryption='qpdf --decrypt'


# Packages
alias showuserpkg='comm -23 <(apt-mark showmanual | sort -u) <(gzip -dc /var/log/installer/initial-status.gz | sed -n "s/^Package: //p" | sort -u)'
