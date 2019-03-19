function crn
    set f1 $dev/notes/category-theory.org
    set f2 $dev/notes/computer-sciences.org
    set f3 $dev/notes/logics.org
    set f4 $dev/notes/math.org
    set f5 $dev/notes/notes.org
    set files $f1 $f2 $f3 $f4 $f5
    cheat-grep $argv $files
end
