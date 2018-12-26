function prep --description "See glances, cputool"
    # https://www.tecmint.com/cputool-limit-linux-process-cpu-usage-load/
    # https://www.tecmint.com/glances-an-advanced-real-time-system-monitoring-tool-for-linux/

    # set cmd pgrep --list-full (string escape -- $argv)
    # echo $cmd
    # set ret (eval $cmd)
    # set pid (string split "/" -- $ret)[0]

    set cmd pidof (string escape -- $argv)
    echo $cmd
    set pid (eval $cmd)

    if test $pid
        # Process 'niceness' vs. 'priority' https://askubuntu.com/a/656787

        # set cmd ps -o euser,ruser,suser,fuser,f,comm,label -p $pid
        # set cmd ps -o pid,tid,class,rtprio,ni,pri,psr,pcpu,stat,wchan:14,comm -p $pid
        set cmd ps -o ppid,pid,user,nice,%cpu,%mem,command --sort ppid -p $pid
        echo $cmd
        eval $cmd
    else
        echo "No PID found: pidof status/retCode $status"
    end
end
