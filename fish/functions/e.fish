function e
  # TODO create e25: cd $dev/emacs-25; and rm elpa; and ln -s elpa-emacs-25 elpa
  # TODO create e26: cd $dev/emacs-25; and rm elpa; and ln -s elpa-emacs-26 elpa
  set cnt (count $argv)
  if test $cnt = 0
    set prms "./"
  else
    set prms $argv
  end

  # DYI violation because variables may not be used as commands
  if pgrep --exact emacs
    set cmd emacsclient $prms \& disown
  else
    # Don't use the --daemon switch. It producess a mess in the shell
    set cmd emacs $prms \& disown
  end
  echo $cmd
  eval $cmd
end
