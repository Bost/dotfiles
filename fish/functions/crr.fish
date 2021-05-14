function crr --description="Search through the Racket cheatsheet"
    rg --with-filename $argv $cheat/cmds/racket.org
end
