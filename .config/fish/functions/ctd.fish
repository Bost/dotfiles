function ctd -d "Compile td/example/java"
    set currWorkingDir (pwd)
    set coconutHome $dec/coconut
    set tdHome $dev/td
    set tdJavaHome $tdHome/example/java

    set cmd cd $tdHome
    echo $cmd
    eval $cmd

    set cmd mkcd jnibuild
    echo $cmd
    eval $cmd

    set build_type -DCMAKE_BUILD_TYPE=Release
    set install_prefix -DCMAKE_INSTALL_PREFIX:PATH=..
    set jni -DTD_ENABLE_JNI=ON
    set -x CMAKE_BUILD_PARALLEL_LEVEL (nproc)
    set cmd cmake $build_type $jni $install_prefix"/example/java/td" ..
    echo $cmd
    eval $cmd

    set cmd cmake --build . --target install
    echo $cmd
    eval $cmd

    set cmd cd $tdJavaHome
    echo $cmd
    eval $cmd
    set cmd mkcd build
    echo $cmd
    eval $cmd
    if test $status = 0
        # set cmd pwd
        # echo $cmd
        # eval $cmd
        set td_dir -DTd_DIR=$tdJavaHome/td/lib/cmake/Td
        set cmd cmake $build_type $td_dir $install_prefix ..
        echo $cmd
        eval $cmd
        if test $status = 0
            set cmd cmake --build . --target install
            echo $cmd
            eval $cmd
            if test $status = 0
                # set cmd ln -s $tdJavaHome/bin $coconutHome/lib
                # echo $cmd
                # eval $cmd

                set cmd cd $tdJavaHome/bin
                echo $cmd
                eval $cmd

                # set cmd jar cf td.jar *
                # echo $cmd
                # eval $cmd

                # set cmd mkdir $coconutHome/lib/
                # echo $cmd
                # eval $cmd

                # set cmd cp td.jar $coconutHome/lib/
                # echo $cmd
                # eval $cmd

                set mainClass org/drinkless/tdlib/example/Example
                echo "#################"
                set cmd cd $tdJavaHome/bin
                echo $cmd
                set cmd java '-Djava.library.path=.' $mainClass
                echo $cmd
                echo "#################"
            end
        end
    end
    set cmd cd $currWorkingDir
    echo $cmd
    eval $cmd
end
