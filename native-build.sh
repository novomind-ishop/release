#!/bin/sh
set -eu

# Generate reachability metadata and use it to build release.bin.
# Usage: ./native-build.sh
# Generate metadata and display the Native Image command: ./native-build.sh --dry-run
# Set GRAALVM_HOME to use a different installation.
# The Scala test class requires sbt (SBT_BIN, .temp/bin/sbt, or PATH).
cd "$(dirname "$0")"
GRAALVM_HOME="${GRAALVM_HOME:-$PWD/.temp/graalvm/graalvm-25.4.4.1.1+1.1}"

if [ ! -x "$GRAALVM_HOME/bin/native-image" ]; then
  echo "E: native-image not found in $GRAALVM_HOME/bin; please set GRAALVM_HOME" >&2
  exit 1
fi
if [ ! -f release.jar ]; then
  echo "E: release.jar not found; build the assembly JAR first" >&2
  exit 1
fi

"$GRAALVM_HOME/bin/native-image" --version
build_dir="$PWD/.temp/native-build"
metadata_dir="$build_dir/resources/META-INF/native-image/release/generated"
mkdir -p "$metadata_dir" "$build_dir/home"

if [ -n "${SBT_BIN:-}" ]; then
  sbt_bin="$SBT_BIN"
elif [ -x "$PWD/.temp/bin/sbt" ]; then
  sbt_bin="$PWD/.temp/bin/sbt"
elif command -v sbt >/dev/null 2>&1; then
  sbt_bin=sbt
else
  echo "E: sbt not found; please install sbt or set SBT_BIN" >&2
  exit 1
fi

# The sbt task compiles the test class and writes its Java classpath.
# Run sbt without the agent to avoid recording its own accesses.
JAVA_HOME="$GRAALVM_HOME" "$sbt_bin" --error nativeTestClasspath
test_classpath=$(cat "$build_dir/test-classpath.txt")

# Use the existing release.jar without rebuilding or modifying it.
# The agent records reflection accesses and resources used during JVM startup.
# config-output-dir replaces the metadata on each build; config-merge-dir then
# adds the configurators that were not used during normal startup.
# A separate test home directory protects the user's configuration.
"$GRAALVM_HOME/bin/java" \
  -Duser.home="$build_dir/home" \
  "-agentlib:native-image-agent=config-output-dir=$metadata_dir" \
  -jar release.jar \
  "$PWD" "$PWD" Linux dumb 80 false "$build_dir/home" --help \
  > "$build_dir/trace-help.log" 2>&1

# The Scala test class checks the appenders from logback.xml and the Basic fallback.
"$GRAALVM_HOME/bin/java" \
  -Duser.home="$build_dir/home" \
  "-agentlib:native-image-agent=config-merge-dir=$metadata_dir" \
  -cp "$test_classpath" release.TraceLogbackTest \
  ch.qos.logback.classic.util.DefaultJoranConfigurator \
  ch.qos.logback.classic.BasicConfigurator \
  > "$build_dir/trace-logback.log" 2>&1

# The additional classpath contains only generated metadata, without a second
# logback.xml. Ignore old Logback metadata embedded in release.jar if the JAR
# has not been rebuilt yet; use the current recording instead.
# Other library metadata, such as JLine's, remains active.
# The trace covers startup and Logback, not all release features.
# -Ob is GraalVM's fast build mode for development and functional tests.
# BrowserSupport detects the native build during initialization.
# This makes the AWT browser branch unreachable, avoiding additional .so files.
# release.bin is placed next to the release launcher and used automatically.
set -x
"$GRAALVM_HOME/bin/native-image" \
  -Ob \
  '--initialize-at-build-time=release.BrowserSupport$' \
  --exclude-config '.*release.jar' 'META-INF/native-image/release/logback/.*' \
  -cp "$build_dir/resources:release.jar" \
  release.Starter release.bin \
  "$@"

# Verified startup command using a writable test home directory:
# mkdir -p .temp/graalvm/logback-native-home
# ./release.bin \
#   -Duser.home="$PWD/.temp/graalvm/logback-native-home" \
#   "$PWD" "$PWD" Linux dumb 80 false "$PWD/.temp/graalvm/logback-native-home" --help
# Log output is written to .temp/graalvm/logback-native-home/.release.log.
