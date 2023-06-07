source /usr/local/share/antigen/antigen.zsh
antigen init ~/.antigenrc

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block, everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
# export ZSH="/Users/agarbuno/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
# ZSH_THEME="robbyrussell"
# ZSH_THEME="maran"
ZSH_THEME="powerlevel10k/powerlevel10k"
POWERLEVEL9K_MODE="awesome-patched"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in ~/.oh-my-zsh/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS=true

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
# plugins=(git)

# source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# To activate the syntax highlighting, add the following at the end of your .zshrc:
#   source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
#
# If you receive "highlighters directory not found" error message,
# you may need to add the following to your .zshenv:
#   export ZSH_HIGHLIGHT_HIGHLIGHTERS_DIR=/usr/local/share/zsh-syntax-highlighting/highlighters

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/agarbuno/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/agarbuno/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/agarbuno/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/agarbuno/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

export PATH="/usr/local/sbin:$PATH"
# export PATH="/Users/agarbuno/.opm/opm-simulators/build/bin:$PATH"
export PATH="/Users/agarbuno/.opm-mpi/opm-simulators/build/bin:$PATH"

export RUBY_CONFIGURE_OPTS="--with-openssl-dir=$(brew --prefix openssl@1.1)"
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
# eval "$(rbenv init -)"

# export PATH="/usr/local/bin:$PATH"
# export PATH="/usr/local/opt/ruby/bin:$PATH"
# export PATH="$HOME/.gem/ruby/2.7.0/bin:$PATH"

# SDK Header files for R
# XCBASE=`xcrun --show-sdk-path`
# export C_INCLUDE_PATH=$XCBASE/usr/include
# export CPLUS_INCLUDE_PATH=$XCBASE/usr/include
# export LIBRARY_PATH=$XCBASE/usr/lib

. /usr/local/opt/asdf/libexec/asdf.sh

export PATH="/usr/local/opt/expat/bin:$PATH"
export LDFLAGS="-L/usr/local/opt/expat/lib${LDFLAGS:+ }${LDFLAGS}"
export CPPFLAGS="-I/usr/local/opt/expat/include${CPPFLAGS:+ }${CPPFLAGS}"
export PKG_CONFIG_PATH="/usr/local/opt/expat/lib/pkgconfig${PKG_CONFIG_PATH:+:}${PKG_CONFIG_PATH}"
export LDFLAGS="-L/usr/local/opt/expat/lib"
export CPPFLAGS="-I/usr/local/opt/expat/include"
export PKG_CONFIG_PATH="/usr/local/opt/expat/lib/pkgconfig"
export C_INCLUDE_PATH="/opt/X11/include:${PKG_CONFIG_PATH:+:}${PKG_CONFIG_PATH}"
export LDFLAGS="-L/usr/local/opt/libffi/lib${LDFLAGS:+ }${LDFLAGS}"
export CPPFLAGS="-I/usr/local/opt/libffi/include${CPPFLAGS:+ }${CPPFLAGS}"
export PKG_CONFIG_PATH="/usr/local/opt/libffi/lib/pkgconfig${PKG_CONFIG_PATH:+:}${PKG_CONFIG_PATH}"
export PATH="/usr/local/opt/openssl@1.1/bin:$PATH"
export LDFLAGS="-L/usr/local/opt/openssl@1.1/lib${LDFLAGS:+ }${LDFLAGS}"
export CPPFLAGS="-I/usr/local/opt/openssl@1.1/include${CPPFLAGS:+ }${CPPFLAGS}"
export PKG_CONFIG_PATH="/usr/local/opt/openssl@1.1/lib/pkgconfig${PKG_CONFIG_PATH:+:}${PKG_CONFIG_PATH}"
export PATH="/usr/local/opt/sqlite/bin:$PATH"
export LDFLAGS="-L/usr/local/opt/sqlite/lib${LDFLAGS:+ }${LDFLAGS}"
export CPPFLAGS="-I/usr/local/opt/sqlite/include${CPPFLAGS:+ }${CPPFLAGS}"
export PKG_CONFIG_PATH="/usr/local/opt/sqlite/lib/pkgconfig${PKG_CONFIG_PATH:+:}${PKG_CONFIG_PATH}"
export PATH="/usr/local/opt/python@3.8/bin:$PATH"
export PKG_CONFIG_PATH="/usr/local/opt/python@3.8/lib/pkgconfig${PKG_CONFIG_PATH:+:}${PKG_CONFIG_PATH}"
export PATH="/usr/local/opt/icu4c/bin:$PATH"
export PATH="/usr/local/opt/icu4c/sbin:$PATH"
export LDFLAGS="-L/usr/local/opt/icu4c/lib${LDFLAGS:+ }${LDFLAGS}"
export CPPFLAGS="-I/usr/local/opt/icu4c/include${CPPFLAGS:+ }${CPPFLAGS}"
export PKG_CONFIG_PATH="/usr/local/opt/icu4c/lib/pkgconfig${PKG_CONFIG_PATH:+:}${PKG_CONFIG_PATH}"
export LDFLAGS="-L/usr/local/opt/libffi/lib${LDFLAGS:+ }${LDFLAGS}"
export CPPFLAGS="-I/usr/local/opt/libffi/include${CPPFLAGS:+ }${CPPFLAGS}"
export PKG_CONFIG_PATH="/usr/local/opt/libffi/lib/pkgconfig${PKG_CONFIG_PATH:+:}${PKG_CONFIG_PATH}"
export PATH="/usr/local/opt/openssl@1.1/bin:$PATH"
export LDFLAGS="-L/usr/local/opt/openssl@1.1/lib${LDFLAGS:+ }${LDFLAGS}"
export CPPFLAGS="-I/usr/local/opt/openssl@1.1/include${CPPFLAGS:+ }${CPPFLAGS}"
export PKG_CONFIG_PATH="/usr/local/opt/openssl@1.1/lib/pkgconfig${PKG_CONFIG_PATH:+:}${PKG_CONFIG_PATH}"
export PATH="/usr/local/opt/sqlite/bin:$PATH"
export LDFLAGS="-L/usr/local/opt/sqlite/lib${LDFLAGS:+ }${LDFLAGS}"
export CPPFLAGS="-I/usr/local/opt/sqlite/include${CPPFLAGS:+ }${CPPFLAGS}"
export PKG_CONFIG_PATH="/usr/local/opt/sqlite/lib/pkgconfig${PKG_CONFIG_PATH:+:}${PKG_CONFIG_PATH}"
export PATH="/usr/local/opt/python@3.8/bin:$PATH"
export PKG_CONFIG_PATH="/usr/local/opt/python@3.8/lib/pkgconfig${PKG_CONFIG_PATH:+:}${PKG_CONFIG_PATH}"
export PATH="/usr/local/opt/icu4c/bin:$PATH"
export PATH="/usr/local/opt/icu4c/sbin:$PATH"
export LDFLAGS="-L/usr/local/opt/icu4c/lib${LDFLAGS:+ }${LDFLAGS}"
export CPPFLAGS="-I/usr/local/opt/icu4c/include${CPPFLAGS:+ }${CPPFLAGS}"
export PKG_CONFIG_PATH="/usr/local/opt/icu4c/lib/pkgconfig${PKG_CONFIG_PATH:+:}${PKG_CONFIG_PATH}"

# To install R 4.1.1 with asdf 
# export PATH="/usr/local/opt/curl/bin:$PATH"
# export LDFLAGS="-L/usr/local/opt/curl/lib"
# export CPPFLAGS="-I/usr/local/opt/curl/include"
# export PKG_CONFIG_PATH="/usr/local/opt/curl/lib/pkgconfig"

# To install R 3.6.2 with asdf -------------------------------------------------
# export PATH="/usr/local/opt/bzip2/bin:$PATH"
# export LDFLAGS="-L/usr/local/opt/bzip2/lib"
# export CPPFLAGS="-I/usr/local/opt/bzip2/include"


# Aliases ----------------------------------------------------------------------
alias cat=bat --paging=never
alias ls='exa -l --group-directories-first --color=auto --git --icons --no-permissions --no-user -U'
alias ll='exa -lahF --group-directories-first --color=auto --git --icons'
export PATH="/usr/local/opt/openjdk/bin:$PATH"
