function e
    # TODO systemctl --user enable emacs
    set cnt (count $argv)
    if test $cnt = 0
        set prms "./"
    else
        set prms $argv
    end

    set prms $prms \& disown
    # Don't use the --daemon switch. It producess a mess in the shell

    set emacsCmd emacs
    # set emacsCmd /usr/local/bin/emacs

    set pids (pgrep --exact emacs)
    # echo "pids:" $pids
    for pid in $pids
        set procUser (ps -o user= -p $pid)
        # printf "pid: %s; procUser: %s; USER: %s\n" $pid $procUser $USER
        # $procUser may not be defined emacs previously crashed or was killed
        if test -n "$procUser"; and test $USER = $procUser
            set procCmd (ps -o command= -p $pid)
            # echo "procCmd:" $procCmd
            # does $procCmd contain the "*defunct*" substring?
            # [zombie] <defunct>
            if string match --quiet -- "*defunct*" $procCmd
                # echo "pid:" $pid "is defunct"
            else
                # TODO test the --no-wait parameter in `emacsclient --no-wait`
                set emacsCmd emacsclient
                # set emacsCmd /usr/local/bin/emacsclient
                break
            end
        end
    end
    set cmd $emacsCmd $prms
    echo $cmd
    eval $cmd
end
