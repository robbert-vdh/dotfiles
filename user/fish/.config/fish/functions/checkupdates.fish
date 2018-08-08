function checkupdates -w checkupdates -d 'Check and highlight both repo and AUR updates'
    set -l IFS # Don't split lines

    set -l aur_updates (yay -Pu | __version_diff)
    set -l repo_updates (command checkupdates | __version_diff)

    echo $repo_updates
    if not test -z $repo_updates
        echo -e "\n\x1b[01mAur updates\x1b[0m:"
        echo $aur_updates
    end
end

function __version_diff -d 'Highlight lines with changed major/minor ver'
    # Yay always prints ANSI color codes, so we'll have to strip those first.
    perl -pe 's/\x1b\[[^\x1b]*?m//g' | gawk '{
            # If a package changes its major/minor version, highlight it in
            # green
            match($2, /^[^.]+\.[^.[:alpha:]]+/, old_version)
            match($4, /^[^.]+\.[^.[:alpha:]]+/, new_version)

            if (old_version[0] != new_version[0]) {
                print "\x1b[32m" $0 "\x1b[0m"
            } else {
                print $0
            }
        }'
end
