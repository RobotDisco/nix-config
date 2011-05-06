# Add user-supplied programs to our environment
add-to-path () {
	if [[ -d $1 ]]; then
		typeset -Ug path
		path+=$1
	fi
}

add-to-path $HOME/bin


# load profiles from /etc/profile.d
# (to disable a profile, just remove execute permission from it)
for profile in /etc/profile.d/*.sh; do
	if [[ -x $profile ]]; then
		. $profile
	fi
done

# Don't pollute our environment
unfunction add-to-path
unset profile

