# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific environment
export PATH="$HOME/.local/bin:$HOME/bin:$PATH"
export VISUAL="emacsclient"
export EDITOR=$VISUAL

# Source aliases
if [ -f ~/.aliases ]; then
	. ~/.aliases
fi

parse_hostname () {
 name=`hostname -s`
 if [ "${name}" == "localhost" ]
 then
   echo ""
 else
   echo "${name}"
 fi
}

#export PS1="`parse_hostname`\w\`parse_git_branch\` "
export PS1="`parse_hostname`:\w "

# If there are multiple matches for completion, Tab should cycle through them
bind "TAB:menu-complete"

# Display a list of the matching files
bind "set show-all-if-ambiguous on"

# Perform partial completion on the first Tab press,
# only start cycling full results on the second Tab press
bind "set menu-complete-display-prefix on"

# Source profile
if [ -f ~/.profile ]; then
	. ~/.profile
fi
