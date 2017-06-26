function e
  set cnt (count $argv)
  if test $cnt = 0
    set prms "./"
  else
    set prms $argv
  end

  # DYI violation because variables may not be used as commands
  if pgrep --exact emacs
    echo emacsclient $prms &
         emacsclient $prms &
  else
    # Don't use the --daemon switch. It producess a mess in the shell
    echo emacs $prms &
         emacs $prms &
  end
end
