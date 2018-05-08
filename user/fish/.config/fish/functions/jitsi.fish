function jitsi -d "Open a Jitsi call in a new chromium instance"
    chromium --app=https://meet.jit.si/$argv[1] > /dev/null 2>/dev/null &
end
