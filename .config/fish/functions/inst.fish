# -*- mode: fish -*-

## fish -n inst.fish
## fish_indent --check inst.fish

function inst --description "Install a .deb file or apt package (dpkg/apt)"
    set prm $argv
    if test -f $prm; and test (string match --regex "\.deb\$" $prm)
        trace sudo dpkg --install $prm
    else
        # trace sudo snap install $prm
        # if test $status != 0
        #     trace sudo apt install --yes $prm
        # end
        trace sudo apt install --yes $prm
    end
end
