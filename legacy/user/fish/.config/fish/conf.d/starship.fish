set -g fish_greeting ""

if command -vq starship
    starship init fish | source
end
