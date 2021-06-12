function touch --description "Create missing directories & touch"
    set file (string escape -- $argv)
    set filedir (dirname $file)

    set cmd mkdir --parents $filedir
    echo $cmd
    eval $cmd

    set cmd /usr/bin/touch $file
    echo $cmd
    eval $cmd
end
