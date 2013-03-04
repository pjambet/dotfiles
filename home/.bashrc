#aliases
alias l="ls -l"
alias ll="ls -la"
alias v240="ssh pierre.nicolas@v240.ig.polytech.univ-montp2.fr"
alias rm="rm -i"
alias v240scp="/Users/pierre/.scripts/v240scp"
alias gitci="/Users/pierre/.scripts/gitci"
alias ls="ls -G"
alias namae="cd /Users/pierre/Documents/namae-bnf/"
alias sshfsmet="sshfs-static-leopard pierrej@shell.metropolia.fi:/users5/p/pierrej remote/ -oauto_cache,reconnect,volname=metropolia
"

alias mysql=/usr/local/mysql/bin/mysql
alias mysqladmin=/usr/local/mysql/bin/mysqladmin

export SVN_EDITOR=nano

#Renvoie la liste des commandes les plus utilisées
#alias profileme="history | awk '{print $2}' | awk 'BEGIN{FS="|"}{print $1}' | sort | uniq -c | sort -n | tail -n 20 | sort -nr"


export PS1='\u:\w$ '
umask 022

export CLICOLOR=1

alias la='ls -A'
alias l='ls -CF'
alias vi='vim'

function cyan_red_prompt
{
  local CYAN="[\033[0;36m]"
  local GRAY="[\033[0;37m]"
  local RED="[\033[0;31m]"

  PS1="${CYAN}[\u ${RED}w${CYAN}]${GRAY} "
}


c_cyan=`tput setaf 6`
c_red=`tput setaf 1`
c_green=`tput setaf 2`
c_sgr0=`tput sgr0`


parse_git_branch ()
{
  if git rev-parse --git-dir >/dev/null 2>&1
  then
          gitver=$(git branch 2>/dev/null| sed -n '/^\*/s/^\* //p')
  else
          return 0
  fi
  echo -e $gitver
}

branch_color ()
{
        if git rev-parse --git-dir >/dev/null 2>&1
        then
                color=""
                if git diff --quiet 2>/dev/null >&2
                then
                        color="${c_green}"
                else
                        color=${c_red}
                fi
        else
                return 0
        fi
        echo -ne $color
}

PS1='[\[$(branch_color)\]$(parse_git_branch)\[${c_sgr0}\]] \[${c_red}\]\w\[${c_sgr0}\] → \[\033[00m\]'
#cyan_red_prompt

mkcd () { mkdir -p "$@" && eval cd "\"\$$#\""; }

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
