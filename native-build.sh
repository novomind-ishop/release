#! /bin/sh
set -eux
java -version
# gu install native-image
# sbt 'set test in assembly := {}' clean assembly
# cp target/scala-2.13/release.jar .
$GRAALVM_HOME/bin/native-image --no-fallback --no-server -cp ./release.jar \
  -H:Name=release.bin \
  -H:IncludeResourceBundles=com.sun.org.apache.xml.internal.res.XMLErrorResources \
  -H:Class=release.Starter -H:+ReportUnsupportedElementsAtRuntime --allow-incomplete-classpath \
  --initialize-at-build-time='scala.runtime.Statics$VM'
# https://www.scala-sbt.org/sbt-native-packager/gettingstarted.html#setup
# https://github.com/scala/bug/issues/11634#issuecomment-580613538
