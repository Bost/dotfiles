function inst
    set prm (string escape -- $argv)
    if test -f $prm; and test (string match --regex "\.deb\$" $prm)
        set cmd sudo dpkg --install $prm
        echo $cmd
        eval $cmd
    else
        # set cmd sudo snap install $prm
        # echo $cmd
        # eval $cmd
        # if test $status != 0
        #     set cmd sudo apt install --yes $prm
        #     echo $cmd
        #     eval $cmd
        # end
        set cmd sudo apt install --yes $prm
        echo $cmd
        eval $cmd
    end
end
