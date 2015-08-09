function u -d "UpdateUpgradeClean"
    set cmd "sudo apt-get update; and sudo apt-get upgrade --yes; and sudo apt-get clean; and sudo checkrestart"
    echo $cmd
    eval $cmd
end
