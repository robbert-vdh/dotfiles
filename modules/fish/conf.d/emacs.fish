# Titles get printed inline in Emacs' ansi-term
if test $TERM = eterm-color
    function fish_title
        true
    end
end
