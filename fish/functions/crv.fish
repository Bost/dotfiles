function crv
    set f1 $dev/dotfiles/.vimrc
    set files $f1
    cheat-grep $argv $lispline $files
end
