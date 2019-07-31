function crn
    set d $dev/notes
    cheat-grep $argv $d/category-theory.org \
                     $d/computer-sciences.org \
                     $d/logics.org \
                     $d/math.org \
                     $d/notes.org \
                     $d/math-structures.org
end
