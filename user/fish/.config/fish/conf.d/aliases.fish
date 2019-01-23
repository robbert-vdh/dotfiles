# Enable colours and readable file sizes wherever applicable
alias df "df -h"
alias du "du -h"
alias grep "grep --color=auto"
alias info "info --vi-keys"
alias less "less -R"

# Always copy and paste from the system clipboard
alias xsel "xsel -b"
alias xpaste "xsel -o"

# Open emacsclient inside terminal (compliment to ~/.dotfiles/bin/em)
alias en "emacsclient -nw -a ''"
alias restart-emacs "systemctl --user restart emacs.service"

# More modern ls replacement
alias ls "exa --group-directories-first"
alias ll "ls --long --binary --git"
alias la "ll -a"
alias lt "la -T"
alias l "ls -a"

# Copy files
alias rsync-copy "rsync --archive --hard-links --one-file-system  --acls --xattrs --info=progress2 --human-readable"
alias rsync-move "rsync-copy --remove-source-files"
alias rsync-update "rsync-copy --update"
alias rsync-sync "rsync-copy --update --delete"
alias rsync-copy-compress "rsync-copy --compress"
alias rsync-sync-compress "rsync-sync --compress"

# The dotnet CLI is broken right now https://github.com/dotnet/sdk/issues/1916
alias dotnet "env TERM=xterm dotnet"
alias dotnet-dev "env ASPNETCORE_ENVIRONMENT=Development dotnet"
