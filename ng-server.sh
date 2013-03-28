#! /bin/bash

leinCp=$(lein classpath)

if [ ! $leinCp ]; then
    echo "Warning! Unable to get classpath from lein, just using existing classpath, expecting clojure jars to be available"
fi

# clojureJarFile must be in the path, otherwise the VimClojure server doesn't start
ngCp=$VIMCLOJURE_SERVER_JAR:$leinCp:$clojureJarFile
#ngCp=$VIMCLOJURE_SERVER_JAR:$leinCp:$CLASSPATH:$clojureJarFile

if [[ "$isLinux" -eq 0 ]]; then
    ngCp="`cygpath -p -w $ngCp`"
fi

echo java -server -cp "$ngCp" vimclojure.nailgun.NGServer \&
     java -server -cp "$ngCp" vimclojure.nailgun.NGServer  &
