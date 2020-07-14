(TeX-add-style-hook
 "cat-7-sketches"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("newpxmath" "varg" "bigdelims") ("xcolor" "usenames" "dvipsnames") ("circuitikz" "siunitx") ("inputenc" "utf8") ("cleveref" "capitalize") ("xy" "all") ("mdframed" "framemethod=tikz") ("geometry" "margin=2cm")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "tikz-stuff"
    "mathtools"
    "amsthm"
    "amssymb"
    "accents"
    "newpxtext"
    "newpxmath"
    "eucal"
    "xcolor"
    "tikz"
    "circuitikz"
    "graphicx"
    "outline"
    "varwidth"
    "enumitem"
    "ifthen"
    "footnote"
    "inputenc"
    "hyperref"
    "cleveref"
    "makeidx"
    "xy"
    "mdframed"
    "todonotes"
    "tablefootnote"
    "geometry")
   (TeX-add-symbols
    '("po" ["argument"] 0)
    '("pb" ["argument"] 0)
    '("hide" ["argument"] 1)
    '("erase" ["argument"] 1)
    '("boxCD" ["argument"] 1)
    '("adjphantom" ["argument"] 2)
    '("Tto" ["argument"] 2)
    '("LTO" ["argument"] 1)
    '("LMO" ["argument"] 1)
    '("adjr" ["argument"] 4)
    '("adj" ["argument"] 4)
    '("sol" ["argument"] 3)
    '("abs" 1)
    '("wt" 1)
    '("wh" 1)
    '("ul" 1)
    '("ubar" 1)
    '("tpow" 1)
    '("tn" 1)
    '("strict" 1)
    '("showhide" 2)
    '("rleg" 1)
    '("restrict" 2)
    '("prt" 1)
    '("overtime" 1)
    '("outp" 1)
    '("ord" 1)
    '("ol" 1)
    '("mob" 1)
    '("lleg" 1)
    '("inp" 1)
    '("grph" 1)
    '("fun" 1)
    '("funr" 1)
    '("funn" 1)
    '("ffunr" 1)
    '("corel" 1)
    '("const" 1)
    '("conj" 1)
    '("comp" 1)
    '("col" 1)
    '("ccat" 1)
    '("cat" 1)
    '("apex" 1)
    '("Unit" 1)
    '("To" 1)
    '("Too" 1)
    '("Surj" 1)
    '("Set" 1)
    '("Ldots" 1)
    '("mono" 1)
    '("inj" 1)
    '("Fun" 1)
    '("Funr" 1)
    '("From" 1)
    '("Fromm" 1)
    '("Cospan" 1)
    '("Cdots" 1)
    '("Cat" 1)
    '("CCat" 1)
    "printvalues"
    "creflastconjunction"
    "finishSolutionChapter"
    "nolisttopbreak"
    "fatsemi"
    "widecheck"
    "colim"
    "Hom"
    "Mor"
    "Ob"
    "Tr"
    "Typ"
    "cod"
    "coker"
    "dju"
    "dom"
    "id"
    "im"
    "inc"
    "ob"
    "addgen"
    "copyopgen"
    "delaygen"
    "discardopgen"
    "scalargen"
    "twist"
    "zerogen"
    "BB"
    "Bool"
    "Bx"
    "CC"
    "cc"
    "Cob"
    "Cost"
    "DNE"
    "Feas"
    "FinSet"
    "Grph"
    "IR"
    "List"
    "NN"
    "OO"
    "Op"
    "Prof"
    "Prop"
    "Psh"
    "QQ"
    "RR"
    "Rel"
    "SMC"
    "Shv"
    "SmCat"
    "SmSet"
    "TFS"
    "Time"
    "ZZ"
    "after"
    "battery"
    "bb"
    "beh"
    "cmap"
    "cocolon"
    "cp"
    "dd"
    "down"
    "elec"
    "expr"
    "false"
    "foo"
    "free"
    "frinj"
    "from"
    "frsurj"
    "imp"
    "inst"
    "inv"
    "iso"
    "light"
    "lollipop"
    "mat"
    "nn"
    "oo"
    "oprdcospan"
    "oprdset"
    "op"
    "pgin"
    "pgout"
    "poset"
    "powset"
    "pp"
    "pr"
    "qand"
    "qqand"
    "qq"
    "rr"
    "sfg"
    "singleton"
    "smf"
    "ssmc"
    "surj"
    "epi"
    "switch"
    "then"
    "tofrom"
    "too"
    "true"
    "tto"
    "underscore"
    "union"
    "upclose"
    "upset"
    "wavyto"
    "zero"
    "zz"
    "minus"
    "initObj"
    "termObj"
    "ladj"
    "radj"
    "qedsymbol")
   (LaTeX-add-environments
    '("exercise" LaTeX-env-args ["argument"] 0)
    "altikz")
   (LaTeX-add-counters
    "solcounterlocal"
    "solcounterglobal")
   (LaTeX-add-mathtools-DeclarePairedDelimiters
    '("ceil" "")
    '("church" "")
    '("classify" "")
    '("copair" "")
    '("corners" "")
    '("floor" "")
    '("pair" "")
    '("unary" ""))
   (LaTeX-add-amsthm-newtheorems
    "construction"
    "notation"
    "axiom"
    "remark"
    "warning")
   (LaTeX-add-amsthm-newtheoremstyles
    "plain")
   (LaTeX-add-xcolor-definecolors
    "theoremcolor"
    "definitioncolor"
    "examplecolor")
   (LaTeX-add-mdframed-mdfdefinestyles
    "theoremframe"
    "definitionframe"
    "exampleframe")
   (LaTeX-add-mdframed-mdtheorems
    '("theorem" "new")
    '("proposition" "new")
    '("corollary" "new")
    '("lemma" "new")
    '("definition" "new")
    '("roughDef" "new")
    '("example" "new")))
 :latex)

