#! /bin/sh
set -eux
java -version
# gu install native-image
# sbt 'set test in assembly := {}' clean assembly
# cp target/scala-2.13/release.jar .
$GRAALVM_HOME/bin/native-image --no-fallback -cp ./release.jar -H:Name=release.bin -H:Class=release.Starter -H:+ReportUnsupportedElementsAtRuntime --allow-incomplete-classpath
