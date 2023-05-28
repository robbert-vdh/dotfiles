function wineprefix -d "Sets the wineprefix to /mnt/data/wine/<prefix>"
    if test -z $argv[1]
        set -gxe WINEPREFIX
        echo "Using default prefix..."
    else
        set -gx WINEPREFIX "/mnt/data/wine/$argv[1]"
        echo "Prefix set to '$WINEPREFIX'."
    end
end
