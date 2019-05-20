function crep
    set pth $dev/cheatsheet/cmds
    cheat-grep $argv $pth/shells.sh $pth/linux.sh $pth/systemd.sh $pth/rest.sh
end
