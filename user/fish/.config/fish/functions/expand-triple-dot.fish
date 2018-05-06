function expand-triple-dot -d 'expand ... to ../..'
    set cmd (commandline --cut-at-cursor)

    switch $cmd[-1]
        case '*..'
            commandline --insert '/..'
        case '*'
            commandline --insert '.'
    end
end
