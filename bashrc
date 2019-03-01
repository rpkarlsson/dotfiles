# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific environment
PATH="$HOME/.local/bin:$HOME/bin:$PATH"
export PATH

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

export VISUAL="emacsclient"
export EDITOR=$VISUAL

# Source aliases
if [ -f ~/.aliases ]; then
	. ~/.aliases
fi

# Natural scrolling
xinput set-prop 11 295 1
