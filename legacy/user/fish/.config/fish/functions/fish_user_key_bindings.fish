function fish_user_key_bindings
    # Expand three dots to ../..
    bind . expand_triple_dot

    ### fzf ###
    if test "$FZF_LEGACY_KEYBINDINGS" -eq 1
        bind \ct '__fzf_find_file'
        bind \cr '__fzf_reverse_isearch'
        bind \ec '__fzf_cd'
        bind \eC '__fzf_cd --hidden'
        bind \cg '__fzf_open'
        bind \co '__fzf_open --editor'
        if bind -M insert >/dev/null 2>/dev/null
            bind -M insert \ct '__fzf_find_file'
            bind -M insert \cr '__fzf_reverse_isearch'
            bind -M insert \ec '__fzf_cd'
            bind -M insert \eC '__fzf_cd --hidden'
            bind -M insert \cg '__fzf_open'
            bind -M insert \co '__fzf_open --editor'
        end
    else
        bind \cf '__fzf_find_file'
        bind \cr '__fzf_reverse_isearch'
        bind \eo '__fzf_cd'
        bind \eO '__fzf_cd --hidden'
        bind \cg '__fzf_open'
        bind \co '__fzf_open --editor'
        if bind -M insert >/dev/null 2>/dev/null
            bind -M insert \cf '__fzf_find_file'
            bind -M insert \cr '__fzf_reverse_isearch'
            bind -M insert \eo '__fzf_cd'
            bind -M insert \eO '__fzf_cd --hidden'
            bind -M insert \cg '__fzf_open'
            bind -M insert \co '__fzf_open --editor'
        end
    end
    if set -q FZF_COMPLETE
        bind \t '__fzf_complete'
    end
    ### fzf ###
end
