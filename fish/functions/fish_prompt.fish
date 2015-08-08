# function fish_prompt
#     echo (pwd) '$ '
# end
function fish_prompt -d "Write out the prompt"
        printf '%s@%s%s%s%s> ' (whoami) (hostname|cut -d . -f 1) (set_color $fish_color_cwd) (prompt_pwd) (set_color normal)
end
