# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="afowler"

# Example aliases
alias zshconfig="mvim ~/.zshrc"
alias ohmyzsh="mvim ~/.oh-my-zsh"


# Set to this to use case-sensitive completion
CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugis would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails3 git textmate ruby lighthouse)
plugins=(git osx zsh-syntax-highlighting zsh-history-substring-search heroku npm brew vagrant ssh-agent)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...


# Custome path
PATH=$PATH:~/.bin


# Custome functions
mkcd () { mkdir -p "$@" && eval cd "\"\$$#\""; }
function mcd() {
  mkdir -p "$1" && cd "$1";
}

plall() {
for f in com_crm com_family com_social com_users frame_standard31
do
  cd $f && master && git pull && cd ..
done
}

eval "$(rbenv init -)"

# hub config
# eval "$(hub alias -s)"

if [ -f ~/.aliases ]; then
  . ~/.aliases
fi

if [ -f ~/.env ]; then
  . ~/.env
fi

if [ -f ~/.private_env ]; then
  . ~/.private_env
fi

# [ -s $HOME/.nvm/nvm.sh ] && . $HOME/.nvm/nvm.sh  # This loads NVM

for file in ~/.shrc/*.sh; do
  source "$file"
done

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

source ~/.bin/tmuxinator.zsh

