function gk --description "gitk --all … &"
    echo "# Try: tig / tig --all / tig status / tig log / tig grep"
    set cmd gitk --all (string escape -- $argv) \&
    echo $cmd
    eval $cmd
end
