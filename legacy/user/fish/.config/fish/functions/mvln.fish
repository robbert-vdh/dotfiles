function mvln -d "Move file, leaving a symlink to the new location"
    if test -z $argv[2]
        echo (set_color --bold)"Usage"(set_color normal)":"
        echo "mvln <source> <destination>"
        return 1
    end
    if not test -e $argv[1]
        echo "File or directory '$argv[1]' does not exist"
        return 1
    end
    if not test -e $argv[2]
        echo "File or directory '$argv[2]' does not exist"
        return 1
    end

    mv $argv[1] $argv[2]
    ln -s (realpath $argv[2]) $argv[1]
end
