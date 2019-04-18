function jitsi -d "Open a Jitsi call in a new chromium instance"
    set url "https://meet.jit.si/$argv[1]"

    echo $url
    chromium --app=$url > /dev/null 2>/dev/null &; disown
end
