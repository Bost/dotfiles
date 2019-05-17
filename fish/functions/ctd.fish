function ctd -d "Compile $dev/td/example/java"
    # set cmd cd $dev/td
    # echo $cmd
    # eval $cmd

    # set cmd mkcd jnibuild
    # echo $cmd
    # eval $cmd

    set build_type -DCMAKE_BUILD_TYPE=Release
    set install_prefix -DCMAKE_INSTALL_PREFIX:PATH=..
    set $jni -DTD_ENABLE_JNI=ON
    # set cmd cmake $build_type $jni $install_prefix"/example/java/td" ..
    # echo $cmd
    # eval $cmd

    # set cmd cmake --build . --target install
    # echo $cmd
    # eval $cmd

    set cmd cd $dev/td/example/java
    echo $cmd
    eval $cmd
    set cmd mkcd build
    echo $cmd
    eval $cmd
    if test $status = 0
        # set cmd pwd
        # echo $cmd
        # eval $cmd
        set td_dir -DTd_DIR=$dev/td/example/java/td/lib/cmake/Td
        set cmd cmake $build_type $td_dir $install_prefix ..
        echo $cmd
        eval $cmd
        if test $status = 0
            set cmd cmake --build . --target install
            echo $cmd
            eval $cmd
            if test $status = 0
                set cmd cd $dev/td/example/java/bin
                echo $cmd
                eval $cmd
                set cmd jar cf td.jar *
                echo $cmd
                eval $cmd
                set cmd cp td.jar $dec/coconut/lib
                echo $cmd
                eval $cmd

                set mainClass org/drinkless/tdlib/example/Example
                echo "#################"
                set cmd java '-Djava.library.path=.' $mainClass
                echo $cmd
            end
        end
    end
end
