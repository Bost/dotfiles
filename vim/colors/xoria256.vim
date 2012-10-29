" Vim color file - xoria256
" Generated by http://bytefluent.com/vivify 2012-09-02
set background=dark
if version > 580
	hi clear
	if exists("syntax_on")
		syntax reset
	endif
endif

set t_Co=256
let g:colors_name = "xoria256"

hi IncSearch guifg=#000000 guibg=#ffdfaf guisp=#ffdfaf gui=NONE ctermfg=NONE ctermbg=223 cterm=NONE
hi WildMenu guifg=#000000 guibg=#dfdf00 guisp=#dfdf00 gui=bold ctermfg=NONE ctermbg=184 cterm=bold
hi SignColumn guifg=#a8a8a8 guibg=#202020 guisp=#202020 gui=NONE ctermfg=248 ctermbg=234 cterm=NONE
hi SpecialComment guifg=#df8787 guibg=#202020 guisp=#202020 gui=NONE ctermfg=174 ctermbg=234 cterm=NONE
hi Typedef guifg=#afafdf guibg=#202020 guisp=#202020 gui=NONE ctermfg=146 ctermbg=234 cterm=NONE
hi Title guifg=#000000 guibg=#dfdf00 guisp=#dfdf00 gui=NONE ctermfg=NONE ctermbg=184 cterm=NONE
hi Folded guifg=#eeeeee guibg=#5f5f87 guisp=#5f5f87 gui=NONE ctermfg=255 ctermbg=60 cterm=NONE
hi PreCondit guifg=#dfafdf guibg=#202020 guisp=#202020 gui=NONE ctermfg=182 ctermbg=234 cterm=NONE
hi Include guifg=#dfafdf guibg=#202020 guisp=#202020 gui=NONE ctermfg=182 ctermbg=234 cterm=NONE
hi TabLineSel guifg=#d0d0d0 guibg=#202020 guisp=#202020 gui=bold ctermfg=252 ctermbg=234 cterm=bold
hi StatusLineNC guifg=#d0d0d0 guibg=#3a3a3a guisp=#3a3a3a gui=NONE ctermfg=252 ctermbg=237 cterm=NONE
"hi CTagsMember -- no settings --
hi NonText guifg=#a8a8a8 guibg=#202020 guisp=#202020 gui=bold ctermfg=248 ctermbg=234 cterm=bold
"hi CTagsGlobalConstant -- no settings --
"hi DiffText -- no settings --
hi ErrorMsg guifg=#ffffff guibg=#df0000 guisp=#df0000 gui=bold ctermfg=15 ctermbg=160 cterm=bold
hi Ignore guifg=#444444 guibg=#202020 guisp=#202020 gui=NONE ctermfg=238 ctermbg=234 cterm=NONE
hi Debug guifg=#df8787 guibg=#202020 guisp=#202020 gui=NONE ctermfg=174 ctermbg=234 cterm=NONE
hi PMenuSbar guifg=#d0d0d0 guibg=#767676 guisp=#767676 gui=NONE ctermfg=252 ctermbg=243 cterm=NONE
hi Identifier guifg=#afdf87 guibg=#202020 guisp=#202020 gui=NONE ctermfg=150 ctermbg=234 cterm=NONE
hi SpecialChar guifg=#df8787 guibg=#202020 guisp=#202020 gui=NONE ctermfg=174 ctermbg=234 cterm=NONE
hi Conditional guifg=#afafdf guibg=#202020 guisp=#202020 gui=NONE ctermfg=146 ctermbg=234 cterm=NONE
hi StorageClass guifg=#afafdf guibg=#202020 guisp=#202020 gui=NONE ctermfg=146 ctermbg=234 cterm=NONE
hi Todo guifg=#000000 guibg=#dfdf00 guisp=#dfdf00 gui=NONE ctermfg=NONE ctermbg=184 cterm=NONE
hi Special guifg=#df8787 guibg=#202020 guisp=#202020 gui=NONE ctermfg=174 ctermbg=234 cterm=NONE
hi LineNr guifg=#a8a8a8 guibg=NONE guisp=NONE gui=NONE ctermfg=248 ctermbg=NONE cterm=NONE
hi StatusLine guifg=#d0d0d0 guibg=#4e4e4e guisp=#4e4e4e gui=bold ctermfg=252 ctermbg=239 cterm=bold
hi Normal guifg=#d0d0d0 guibg=#202020 guisp=#202020 gui=NONE ctermfg=252 ctermbg=234 cterm=NONE
hi Label guifg=#afafdf guibg=#202020 guisp=#202020 gui=NONE ctermfg=146 ctermbg=234 cterm=NONE
"hi CTagsImport -- no settings --
hi PMenuSel guifg=#000000 guibg=#767676 guisp=#767676 gui=NONE ctermfg=NONE ctermbg=243 cterm=NONE
hi Search guifg=#000000 guibg=#afdf5f guisp=#afdf5f gui=NONE ctermfg=NONE ctermbg=149 cterm=NONE
"hi CTagsGlobalVariable -- no settings --
hi Delimiter guifg=#df8787 guibg=#202020 guisp=#202020 gui=NONE ctermfg=174 ctermbg=234 cterm=NONE
hi Statement guifg=#5fafdf guibg=#202020 guisp=#202020 gui=NONE ctermfg=74 ctermbg=234 cterm=NONE
"hi SpellRare -- no settings --
"hi EnumerationValue -- no settings --
hi Comment guifg=#808080 guibg=#202020 guisp=#202020 gui=NONE ctermfg=8 ctermbg=234 cterm=NONE
hi Character guifg=#dfdfaf guibg=#202020 guisp=#202020 gui=NONE ctermfg=187 ctermbg=234 cterm=NONE
hi Float guifg=#dfaf87 guibg=#202020 guisp=#202020 gui=NONE ctermfg=180 ctermbg=234 cterm=NONE
hi Number guifg=#dfaf87 guibg=#202020 guisp=#202020 gui=NONE ctermfg=180 ctermbg=234 cterm=NONE
hi Boolean guifg=#5fafdf guibg=#202020 guisp=#202020 gui=NONE ctermfg=74 ctermbg=234 cterm=NONE
hi Operator guifg=#afafdf guibg=#202020 guisp=#202020 gui=NONE ctermfg=146 ctermbg=234 cterm=NONE
hi CursorLine guifg=NONE guibg=#444444 guisp=#444444 gui=NONE ctermfg=NONE ctermbg=238 cterm=NONE
"hi Union -- no settings --
hi TabLineFill guifg=#d0d0d0 guibg=#666666 guisp=#666666 gui=underline ctermfg=252 ctermbg=241 cterm=underline
hi Question guifg=#87df7f guibg=#202020 guisp=#202020 gui=bold ctermfg=114 ctermbg=234 cterm=bold
hi WarningMsg guifg=#ff0000 guibg=#202020 guisp=#202020 gui=bold ctermfg=196 ctermbg=234 cterm=bold
hi VisualNOS guifg=#9e9e9e guibg=#202020 guisp=#202020 gui=bold,underline ctermfg=247 ctermbg=234 cterm=bold,underline
"hi DiffDelete -- no settings --
hi ModeMsg guifg=#d0d0d0 guibg=#202020 guisp=#202020 gui=bold ctermfg=252 ctermbg=234 cterm=bold
hi CursorColumn guifg=NONE guibg=#444444 guisp=#444444 gui=NONE ctermfg=NONE ctermbg=238 cterm=NONE
hi Define guifg=#dfafdf guibg=#202020 guisp=#202020 gui=NONE ctermfg=182 ctermbg=234 cterm=NONE
hi Function guifg=#afdf87 guibg=#202020 guisp=#202020 gui=NONE ctermfg=150 ctermbg=234 cterm=NONE
hi FoldColumn guifg=#a8a8a8 guibg=#202020 guisp=#202020 gui=NONE ctermfg=248 ctermbg=234 cterm=NONE
hi PreProc guifg=#dfafdf guibg=#202020 guisp=#202020 gui=NONE ctermfg=182 ctermbg=234 cterm=NONE
"hi EnumerationName -- no settings --
hi Visual guifg=#005f87 guibg=#afdfff guisp=#afdfff gui=NONE ctermfg=24 ctermbg=153 cterm=NONE
hi MoreMsg guifg=#bcbcbc guibg=#202020 guisp=#202020 gui=bold ctermfg=250 ctermbg=234 cterm=bold
"hi SpellCap -- no settings --
hi VertSplit guifg=#d0d0d0 guibg=#3a3a3a guisp=#3a3a3a gui=NONE ctermfg=252 ctermbg=237 cterm=NONE
hi Exception guifg=#afafdf guibg=#202020 guisp=#202020 gui=NONE ctermfg=146 ctermbg=234 cterm=NONE
hi Keyword guifg=#afafdf guibg=#202020 guisp=#202020 gui=NONE ctermfg=146 ctermbg=234 cterm=NONE
hi Type guifg=#afafdf guibg=#202020 guisp=#202020 gui=NONE ctermfg=146 ctermbg=234 cterm=NONE
"hi DiffChange -- no settings --
hi Cursor guifg=#202020 guibg=#ffaf00 guisp=#ffaf00 gui=NONE ctermfg=234 ctermbg=214 cterm=NONE
"hi SpellLocal -- no settings --
hi Error guifg=#ff0000 guibg=#202020 guisp=#202020 gui=NONE ctermfg=196 ctermbg=234 cterm=NONE
hi PMenu guifg=#000000 guibg=#949494 guisp=#949494 gui=NONE ctermfg=NONE ctermbg=246 cterm=NONE
hi SpecialKey guifg=#5fdf5f guibg=#202020 guisp=#202020 gui=NONE ctermfg=77 ctermbg=234 cterm=NONE
hi Constant guifg=#dfdfaf guibg=#202020 guisp=#202020 gui=NONE ctermfg=187 ctermbg=234 cterm=NONE
"hi DefinedName -- no settings --
hi Tag guifg=#df8787 guibg=#202020 guisp=#202020 gui=NONE ctermfg=174 ctermbg=234 cterm=NONE
hi String guifg=#dfdfaf guibg=#202020 guisp=#202020 gui=NONE ctermfg=187 ctermbg=234 cterm=NONE
hi PMenuThumb guifg=#202020 guibg=#d0d0d0 guisp=#d0d0d0 gui=NONE ctermfg=234 ctermbg=252 cterm=NONE
hi MatchParen guifg=#dfdfdf guibg=#5f87df guisp=#5f87df gui=bold ctermfg=254 ctermbg=68 cterm=bold
"hi LocalVariable -- no settings --
hi Repeat guifg=#afafdf guibg=#202020 guisp=#202020 gui=NONE ctermfg=146 ctermbg=234 cterm=NONE
"hi SpellBad -- no settings --
"hi CTagsClass -- no settings --
hi Directory guifg=#00afff guibg=#202020 guisp=#202020 gui=NONE ctermfg=39 ctermbg=234 cterm=NONE
hi Structure guifg=#afafdf guibg=#202020 guisp=#202020 gui=NONE ctermfg=146 ctermbg=234 cterm=NONE
hi Macro guifg=#dfafdf guibg=#202020 guisp=#202020 gui=NONE ctermfg=182 ctermbg=234 cterm=NONE
hi Underlined guifg=#00afff guibg=#202020 guisp=#202020 gui=underline ctermfg=39 ctermbg=234 cterm=underline
"hi DiffAdd -- no settings --
hi TabLine guifg=#d0d0d0 guibg=#666666 guisp=#666666 gui=underline ctermfg=252 ctermbg=241 cterm=underline
hi mbenormal guifg=#cfbfad guibg=#2e2e3f guisp=#2e2e3f gui=NONE ctermfg=187 ctermbg=237 cterm=NONE
hi perlspecialstring guifg=#c080d0 guibg=#404040 guisp=#404040 gui=NONE ctermfg=176 ctermbg=238 cterm=NONE
hi doxygenspecial guifg=#fdd090 guibg=NONE guisp=NONE gui=NONE ctermfg=222 ctermbg=NONE cterm=NONE
hi mbechanged guifg=#eeeeee guibg=#2e2e3f guisp=#2e2e3f gui=NONE ctermfg=255 ctermbg=237 cterm=NONE
hi mbevisiblechanged guifg=#eeeeee guibg=#4e4e8f guisp=#4e4e8f gui=NONE ctermfg=255 ctermbg=60 cterm=NONE
hi doxygenparam guifg=#fdd090 guibg=NONE guisp=NONE gui=NONE ctermfg=222 ctermbg=NONE cterm=NONE
hi doxygensmallspecial guifg=#fdd090 guibg=NONE guisp=NONE gui=NONE ctermfg=222 ctermbg=NONE cterm=NONE
hi doxygenprev guifg=#fdd090 guibg=NONE guisp=NONE gui=NONE ctermfg=222 ctermbg=NONE cterm=NONE
hi perlspecialmatch guifg=#c080d0 guibg=#404040 guisp=#404040 gui=NONE ctermfg=176 ctermbg=238 cterm=NONE
hi cformat guifg=#c080d0 guibg=#404040 guisp=#404040 gui=NONE ctermfg=176 ctermbg=238 cterm=NONE
hi lcursor guifg=#000000 guibg=#00df00 guisp=#00df00 gui=NONE ctermfg=NONE ctermbg=40 cterm=NONE
hi cursorim guifg=#E6E6FA guibg=#FF0000 guisp=#FF0000 gui=NONE ctermfg=189 ctermbg=196 cterm=NONE
hi doxygenspecialmultilinedesc guifg=#ad600b guibg=NONE guisp=NONE gui=NONE ctermfg=130 ctermbg=NONE cterm=NONE
hi taglisttagname guifg=#000000 guibg=#008700 guisp=#008700 gui=NONE ctermfg=NONE ctermbg=28 cterm=NONE
hi doxygenbrief guifg=#fdab60 guibg=NONE guisp=NONE gui=NONE ctermfg=215 ctermbg=NONE cterm=NONE
hi mbevisiblenormal guifg=#cfcfcd guibg=#4e4e8f guisp=#4e4e8f gui=NONE ctermfg=252 ctermbg=60 cterm=NONE
hi user2 guifg=#00ff00 guibg=#0000df guisp=#0000df gui=NONE ctermfg=10 ctermbg=20 cterm=NONE
hi user1 guifg=#ffffff guibg=#0000df guisp=#0000df gui=NONE ctermfg=15 ctermbg=20 cterm=NONE
hi doxygenspecialonelinedesc guifg=#ad600b guibg=NONE guisp=NONE gui=NONE ctermfg=130 ctermbg=NONE cterm=NONE
hi doxygencomment guifg=#ad7b20 guibg=NONE guisp=NONE gui=NONE ctermfg=130 ctermbg=NONE cterm=NONE
hi cspecialcharacter guifg=#c080d0 guibg=#404040 guisp=#404040 gui=NONE ctermfg=176 ctermbg=238 cterm=NONE
"hi clear -- no settings --
hi underline guifg=#5faf00 guibg=NONE guisp=NONE gui=underline ctermfg=70 ctermbg=NONE cterm=underline
hi function guifg=#0055cc guibg=#f0f2f0 guisp=#f0f2f0 gui=NONE ctermfg=26 ctermbg=194 cterm=NONE
hi titled guifg=#000000 guibg=#fffdfa guisp=#fffdfa gui=NONE ctermfg=NONE ctermbg=230 cterm=NONE
hi cssboxattr guifg=#92AF72 guibg=NONE guisp=NONE gui=NONE ctermfg=107 ctermbg=NONE cterm=NONE
hi cssgeneratedcontentattr guifg=#92AF72 guibg=NONE guisp=NONE gui=NONE ctermfg=107 ctermbg=NONE cterm=NONE
hi htmlarg guifg=#CBC983 guibg=NONE guisp=NONE gui=NONE ctermfg=186 ctermbg=NONE cterm=NONE
hi phpcomparison guifg=#ffffff guibg=NONE guisp=NONE gui=NONE ctermfg=15 ctermbg=NONE cterm=NONE
hi javascriptnumber guifg=#B3EBBF guibg=NONE guisp=NONE gui=NONE ctermfg=151 ctermbg=NONE cterm=NONE
hi htmltagn guifg=#F3F2CC guibg=NONE guisp=NONE gui=NONE ctermfg=230 ctermbg=NONE cterm=NONE
hi cssimportant guifg=#EB5D49 guibg=NONE guisp=NONE gui=NONE ctermfg=203 ctermbg=NONE cterm=NONE
hi diffcomment guifg=#6B6B6B guibg=NONE guisp=NONE gui=NONE ctermfg=242 ctermbg=NONE cterm=NONE
hi cssfontprop guifg=#F3F2CC guibg=NONE guisp=NONE gui=NONE ctermfg=230 ctermbg=NONE cterm=NONE
hi phpidentifier guifg=#7895B7 guibg=NONE guisp=NONE gui=NONE ctermfg=67 ctermbg=NONE cterm=NONE
hi cssauralprop guifg=#F3F2CC guibg=NONE guisp=NONE gui=NONE ctermfg=230 ctermbg=NONE cterm=NONE
hi difffile guifg=#ffffff guibg=NONE guisp=NONE gui=NONE ctermfg=15 ctermbg=NONE cterm=NONE
hi csscommonattr guifg=#92AF72 guibg=NONE guisp=NONE gui=NONE ctermfg=107 ctermbg=NONE cterm=NONE
hi rubyfunction guifg=#CBC983 guibg=NONE guisp=NONE gui=NONE ctermfg=186 ctermbg=NONE cterm=NONE
hi cssbraces guifg=#ffffff guibg=NONE guisp=NONE gui=NONE ctermfg=15 ctermbg=NONE cterm=NONE
hi cssfontattr guifg=#92AF72 guibg=NONE guisp=NONE gui=NONE ctermfg=107 ctermbg=NONE cterm=NONE
hi phpvarselector guifg=#F3F2CC guibg=NONE guisp=NONE gui=NONE ctermfg=230 ctermbg=NONE cterm=NONE
hi cssrenderprop guifg=#F3F2CC guibg=NONE guisp=NONE gui=NONE ctermfg=230 ctermbg=NONE cterm=NONE
hi diffadded guifg=#ffffff guibg=#7D9662 guisp=#7D9662 gui=NONE ctermfg=15 ctermbg=101 cterm=NONE
hi htmltagname guifg=#F3F2CC guibg=NONE guisp=NONE gui=NONE ctermfg=230 ctermbg=NONE cterm=NONE
hi cssgeneratedcontentprop guifg=#F3F2CC guibg=NONE guisp=NONE gui=NONE ctermfg=230 ctermbg=NONE cterm=NONE
hi pythoncomment guifg=#6B6B6B guibg=NONE guisp=NONE gui=NONE ctermfg=242 ctermbg=NONE cterm=NONE
hi csspagingprop guifg=#F3F2CC guibg=NONE guisp=NONE gui=NONE ctermfg=230 ctermbg=NONE cterm=NONE
hi htmlspecialtagname guifg=#F3F2CC guibg=NONE guisp=NONE gui=NONE ctermfg=230 ctermbg=NONE cterm=NONE
hi htmlstring guifg=#92AF72 guibg=NONE guisp=NONE gui=NONE ctermfg=107 ctermbg=NONE cterm=NONE
hi csscolor guifg=#B3EBBF guibg=NONE guisp=NONE gui=NONE ctermfg=151 ctermbg=NONE cterm=NONE
hi rubyconstant guifg=#F3F2CC guibg=NONE guisp=NONE gui=NONE ctermfg=230 ctermbg=NONE cterm=NONE
hi csscolorattr guifg=#92AF72 guibg=NONE guisp=NONE gui=NONE ctermfg=107 ctermbg=NONE cterm=NONE
hi rubyinstancevariable guifg=#7895B7 guibg=NONE guisp=NONE gui=NONE ctermfg=67 ctermbg=NONE cterm=NONE
hi phpspecialfunction guifg=#CBC983 guibg=NONE guisp=NONE gui=NONE ctermfg=186 ctermbg=NONE cterm=NONE
hi csstableprop guifg=#F3F2CC guibg=NONE guisp=NONE gui=NONE ctermfg=230 ctermbg=NONE cterm=NONE
hi rubyclassvariable guifg=#7895B7 guibg=NONE guisp=NONE gui=NONE ctermfg=67 ctermbg=NONE cterm=NONE
hi htmltag guifg=#F3F2CC guibg=NONE guisp=NONE gui=NONE ctermfg=230 ctermbg=NONE cterm=NONE
hi cssuiattr guifg=#92AF72 guibg=NONE guisp=NONE gui=NONE ctermfg=107 ctermbg=NONE cterm=NONE
hi cssuiprop guifg=#F3F2CC guibg=NONE guisp=NONE gui=NONE ctermfg=230 ctermbg=NONE cterm=NONE
hi rubymodule guifg=#EB5D49 guibg=NONE guisp=NONE gui=NONE ctermfg=203 ctermbg=NONE cterm=NONE
hi diffline guifg=#7895B7 guibg=NONE guisp=NONE gui=NONE ctermfg=67 ctermbg=NONE cterm=NONE
hi colorcolumn guifg=NONE guibg=#444444 guisp=#444444 gui=NONE ctermfg=NONE ctermbg=238 cterm=NONE
hi rubyclass guifg=#EB5D49 guibg=NONE guisp=NONE gui=NONE ctermfg=203 ctermbg=NONE cterm=NONE
hi rubydefine guifg=#EB5D49 guibg=NONE guisp=NONE gui=NONE ctermfg=203 ctermbg=NONE cterm=NONE
hi csstextattr guifg=#92AF72 guibg=NONE guisp=NONE gui=NONE ctermfg=107 ctermbg=NONE cterm=NONE
hi cssfunctionname guifg=#CBC983 guibg=NONE guisp=NONE gui=NONE ctermfg=186 ctermbg=NONE cterm=NONE
"hi htmllink -- no settings --
hi diffnoeol guifg=#cccccc guibg=NONE guisp=NONE gui=NONE ctermfg=252 ctermbg=NONE cterm=NONE
hi cssidentifier guifg=#F3F2CC guibg=NONE guisp=NONE gui=NONE ctermfg=230 ctermbg=NONE cterm=NONE
hi csstextprop guifg=#F3F2CC guibg=NONE guisp=NONE gui=NONE ctermfg=230 ctermbg=NONE cterm=NONE
hi csscolorprop guifg=#F3F2CC guibg=NONE guisp=NONE gui=NONE ctermfg=230 ctermbg=NONE cterm=NONE
hi diffremoved guifg=#ffffff guibg=#D65340 guisp=#D65340 gui=NONE ctermfg=15 ctermbg=167 cterm=NONE
hi phpc1top guifg=#ffffff guibg=NONE guisp=NONE gui=NONE ctermfg=15 ctermbg=NONE cterm=NONE
hi rubyregexp guifg=#E8A75C guibg=NONE guisp=NONE gui=NONE ctermfg=179 ctermbg=NONE cterm=NONE
hi rubysymbol guifg=#E8A75C guibg=NONE guisp=NONE gui=NONE ctermfg=179 ctermbg=NONE cterm=NONE
hi csstagname guifg=#CBC983 guibg=NONE guisp=NONE gui=NONE ctermfg=186 ctermbg=NONE cterm=NONE
hi cssclassname guifg=#CBC983 guibg=NONE guisp=NONE gui=NONE ctermfg=186 ctermbg=NONE cterm=NONE
hi phpmemberselector guifg=#ffffff guibg=NONE guisp=NONE gui=NONE ctermfg=15 ctermbg=NONE cterm=NONE
hi cssfunction guifg=#CBC983 guibg=NONE guisp=NONE gui=NONE ctermfg=186 ctermbg=NONE cterm=NONE
hi cssboxprop guifg=#F3F2CC guibg=NONE guisp=NONE gui=NONE ctermfg=230 ctermbg=NONE cterm=NONE
hi htmlitalic guifg=#ffff00 guibg=NONE guisp=NONE gui=italic ctermfg=11 ctermbg=NONE cterm=NONE
hi htmlboldunderlineitalic guifg=#ffffff guibg=NONE guisp=NONE gui=bold,italic,underline ctermfg=15 ctermbg=NONE cterm=bold,underline
hi djangostatement guifg=#005f00 guibg=#ddffaa guisp=#ddffaa gui=NONE ctermfg=22 ctermbg=193 cterm=NONE
hi htmlbolditalic guifg=#ffffff guibg=NONE guisp=NONE gui=bold,italic ctermfg=15 ctermbg=NONE cterm=bold
hi doctrans guifg=#ffffff guibg=#ffffff guisp=#ffffff gui=NONE ctermfg=15 ctermbg=15 cterm=NONE
hi helpnote guifg=#000000 guibg=#ffd700 guisp=#ffd700 gui=NONE ctermfg=NONE ctermbg=220 cterm=NONE
hi htmlunderlineitalic guifg=#ffffff guibg=NONE guisp=NONE gui=bold,italic,underline ctermfg=15 ctermbg=NONE cterm=bold,underline
hi doccode guifg=#00aa00 guibg=NONE guisp=NONE gui=NONE ctermfg=34 ctermbg=NONE cterm=NONE
hi docspecial guifg=#4876ff guibg=NONE guisp=NONE gui=NONE ctermfg=69 ctermbg=NONE cterm=NONE
hi htmlbold guifg=#ffffff guibg=NONE guisp=NONE gui=bold ctermfg=15 ctermbg=NONE cterm=bold
hi htmlboldunderline guifg=#ffffff guibg=NONE guisp=NONE gui=bold,underline ctermfg=15 ctermbg=NONE cterm=bold,underline
hi htmlunderline guifg=#8b008b guibg=NONE guisp=NONE gui=underline ctermfg=90 ctermbg=NONE cterm=underline
hi htmlstatement guifg=#af5f87 guibg=NONE guisp=NONE gui=NONE ctermfg=132 ctermbg=NONE cterm=NONE
hi spellerrors guifg=#ffffff guibg=#7f0000 guisp=#7f0000 gui=NONE ctermfg=15 ctermbg=3 cterm=NONE
hi debug guifg=#ffffff guibg=#006400 guisp=#006400 gui=NONE ctermfg=15 ctermbg=22 cterm=NONE
hi warningmsg guifg=#ffffff guibg=#00008b guisp=#00008b gui=NONE ctermfg=15 ctermbg=18 cterm=NONE
hi ifdefifout guifg=#a9a9a9 guibg=NONE guisp=NONE gui=NONE ctermfg=248 ctermbg=NONE cterm=NONE
hi menu guifg=#000000 guibg=#bebebe guisp=#bebebe gui=NONE ctermfg=NONE ctermbg=7 cterm=NONE
hi scrollbar guifg=#ae8857 guibg=#deb887 guisp=#deb887 gui=NONE ctermfg=137 ctermbg=180 cterm=NONE
hi keyword guifg=#FFDE00 guibg=NONE guisp=NONE gui=NONE ctermfg=220 ctermbg=NONE cterm=NONE
"hi rubystringdelimiter -- no settings --
hi type guifg=#84A7C1 guibg=NONE guisp=NONE gui=NONE ctermfg=110 ctermbg=NONE cterm=NONE
hi normal guifg=#ffffff guibg=#0B1022 guisp=#0B1022 gui=NONE ctermfg=15 ctermbg=235 cterm=NONE
hi constant guifg=#CAFE1E guibg=NONE guisp=NONE gui=NONE ctermfg=190 ctermbg=NONE cterm=NONE
hi vimmodeline guifg=#5fd75f guibg=NONE guisp=NONE gui=NONE ctermfg=77 ctermbg=NONE cterm=NONE
hi pmenum guifg=#585858 guibg=#1c1c1c guisp=#1c1c1c gui=NONE ctermfg=240 ctermbg=234 cterm=NONE
hi browsedirectory guifg=#97ffff guibg=NONE guisp=NONE gui=NONE ctermfg=123 ctermbg=NONE cterm=NONE
hi char guifg=#77dd88 guibg=#354535 guisp=#354535 gui=NONE ctermfg=114 ctermbg=238 cterm=NONE
hi browsesuffixes guifg=#cdc8b1 guibg=#1F3055 guisp=#1F3055 gui=NONE ctermfg=187 ctermbg=17 cterm=NONE
hi subtitle guifg=#000000 guibg=NONE guisp=NONE gui=NONE ctermfg=NONE ctermbg=NONE cterm=NONE
hi match guifg=#0000FF guibg=#FFFF00 guisp=#FFFF00 gui=bold ctermfg=21 ctermbg=11 cterm=bold
hi user4 guifg=#00ffdf guibg=#0000df guisp=#0000df gui=NONE ctermfg=50 ctermbg=20 cterm=NONE
hi user3 guifg=#00ff00 guibg=#0000df guisp=#0000df gui=NONE ctermfg=10 ctermbg=20 cterm=NONE
hi taglistcomment guifg=#000000 guibg=#008700 guisp=#008700 gui=NONE ctermfg=NONE ctermbg=28 cterm=NONE
hi taglisttitle guifg=#ff00af guibg=#000000 guisp=#000000 gui=NONE ctermfg=199 ctermbg=NONE cterm=NONE
hi user5 guifg=#00ff00 guibg=#0000df guisp=#0000df gui=NONE ctermfg=10 ctermbg=20 cterm=NONE
hi taglistfilename guifg=#ffffff guibg=#870087 guisp=#870087 gui=NONE ctermfg=15 ctermbg=90 cterm=NONE
hi taglisttagscope guifg=#000000 guibg=#008700 guisp=#008700 gui=NONE ctermfg=NONE ctermbg=28 cterm=NONE
hi preproc guifg=#0000ff guibg=NONE guisp=NONE gui=NONE ctermfg=21 ctermbg=NONE cterm=NONE
