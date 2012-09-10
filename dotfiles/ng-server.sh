#! /bin/bash

LEIN_CP=$(lein classpath)
 
if [ ! $LEIN_CP ]; then
    echo "Warning! Unable to get classpath from lein, just using existing classpath, expecting clojure jars to be available"
fi

echo "JAVA_CP_SEP: $JAVA_CP_SEP"

NG_CP=""
NG_CP=$NG_CP$VIMCLOJURE_SERVER_JAR$JAVA_CP_SEP
NG_CP=$NG_CP$LEIN_CP$JAVA_CP_SEP
NG_CP=$NG_CP$CLASSPATH
echo java -server -cp "$NG_CP" vimclojure.nailgun.NGServer &
     java -server -cp "$NG_CP" vimclojure.nailgun.NGServer &
