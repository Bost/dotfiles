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

    # set pids (pgrep --exact emacs) # --exact doesn't work on Guix
    set pids (pgrep emacs)
    # echo "pids:" $pids
    for pid in $pids
        set procUser (ps -o user= -p $pid)
        # printf "pid: %s; procUser: %s; USER: %s\n" $pid $procUser $USER
        # $procUser may not be defined if emacs previously crashed or was killed
        if test -n "$procUser"; and test $USER = $procUser
            set procCmd (ps -o command= -p $pid)
            # echo "procCmd:" $procCmd
            # does $procCmd contain the "*defunct*" substring?
            # [zombie] <defunct>
            if string match --quiet -- "*defunct*" $procCmd
                # echo "pid:" $pid "is defunct"
            else
                # --no-wait suppresses the question:
                #   This Emacs session has clients; exit anyway?
                set emacsCmd emacsclient --no-wait
                break
            end
        end
    end
    set cmd $emacsCmd $prms
    echo $cmd
    eval $cmd
end
