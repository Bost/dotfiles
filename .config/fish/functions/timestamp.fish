function timestamp --description "Human readable date"
    # date +"%T" - prints just %H:%M:%S
    set cmd date "+%F_%H-%M-%S" # same as "+%Y-%m-%d_%H-%M-%S"
    echo $cmd
    eval $cmd
end
