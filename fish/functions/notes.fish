function notes
  set cmd cd $dev/notes
  echo $cmd
  eval $cmd

  if test $status = 0
      set dir ~/anaconda3/bin
      if test $PATH[1] != $dir
          set cmd set --export PATH $dir $PATH
          echo $cmd
          eval $cmd
      else
          # remove $dir from $PATH
          # set PATH (string match -v $dir $PATH)
      end
  end
end
