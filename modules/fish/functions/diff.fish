function diff -d "Use Git's diffing engine instead of GNU diffutils"
    if test -z $argv[2]
        echo "Missing argument(s)"
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

    # Right now git's diff does not work well with symlinks
    set file1 (realpath $argv[1])
    set file2 (realpath $argv[2])
    if command -sq git
        git diff --color=auto --no-ext-diff --no-index $argv[3..-1] -- $file1 $file2
    else
        diff --color=auto $argv[3..-1] -- $file1 $file2
    end
end
