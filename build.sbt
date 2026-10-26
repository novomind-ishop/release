name := "release"

version := "1.0-SNAPSHOT"

scalaVersion := "3.10.0"

scalacOptions ++= Seq("-unchecked", "-deprecation", "--release:21",
  "-feature", "-language:implicitConversions", "-Werror",
  "-Wconf:cat=deprecation&origin=release\\..*:i")

logLevel := Level.Warn

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.6.5"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.6"

libraryDependencies += "javax.xml.bind" % "jaxb-api" % "2.3.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.5.0"

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0"

libraryDependencies += "org.apache.maven.resolver" % "maven-resolver-connector-basic" % "1.9.27"

libraryDependencies += "org.apache.maven.resolver" % "maven-resolver-transport-file" % "1.9.27"

libraryDependencies += "org.apache.maven.resolver" % "maven-resolver-transport-http" % "1.9.27"

libraryDependencies += "org.apache.maven.resolver" % "maven-resolver-supplier" % "1.9.27"

// libraryDependencies += "org.codehaus.plexus" % "plexus-utils" % "4.0.0"

libraryDependencies += "org.apache.maven" % "maven-resolver-provider" % "3.9.16"

//libraryDependencies += "io.get-coursier" %% "coursier" % "2.1.7"

resolvers += "Gradle releases" at "https://repo.gradle.org/gradle/libs-releases"

libraryDependencies += "org.gradle" % "gradle-tooling-api" % "9.7.0"

libraryDependencies += "org.jline" % "jline-terminal" % "4.3.1"

libraryDependencies += "org.jline" % "jline-reader" % "4.3.1"

libraryDependencies += "com.github.siom79.japicmp" % "japicmp" % "0.26.3"

libraryDependencies += "com.typesafe" % "config" % "1.4.9"

libraryDependencies += "com.google.googlejavaformat" % "google-java-format" % "1.36.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.20" % "test"

libraryDependencies += "org.scalatestplus" %% "junit-4-13" % "3.2.20.0" % Test

libraryDependencies += "org.mockito" % "mockito-core" % "5.23.0" % Test

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

libraryDependencies += "org.yaml" % "snakeyaml" % "2.7"

assembly / assemblyMergeStrategy := {
  // Keep Native Image metadata, including reflection registrations for logback.xml.
  case PathList("META-INF", "native-image", _*) => MergeStrategy.deduplicate
  case PathList("META-INF", "services", _*) => MergeStrategy.first
  case PathList("META-INF", _*) => MergeStrategy.discard
  case PathList("module-info.class") => MergeStrategy.discard
  case PathList("NOTICE") => MergeStrategy.concat
  case PathList("LICENSE") => MergeStrategy.concat
  case PathList("javax", "xml", _*) => MergeStrategy.first
  case PathList("javax", "activation", _*) => MergeStrategy.first
  case x => MergeStrategy.singleOrError
}

assembly / logLevel := Level.Warn

assembly / mainClass := Some("release.Starter")

assembly / assemblyJarName := "../../../../release.jar"

val writeGitHead = taskKey[String]("Write the current checkout commit for the Docker image")

writeGitHead := Def.uncached {
  val head = scala.sys.process.Process(Seq("git", "rev-parse", "--verify", "HEAD"), baseDirectory.value).!!.trim
  IO.write(baseDirectory.value / "target" / "git.HEAD", head + "\n")
  head
}

assembly := assembly.dependsOn(writeGitHead).value

assembly / packageOptions += Package.ManifestAttributes("Git-Commit" -> writeGitHead.value)

publish / skip := true

val java9Options = Seq(
  "-XX:+IgnoreUnrecognizedVMOptions",
  "--add-modules=jdk.compiler",
  "--add-exports=jdk.compiler/com.sun.tools.javac.api=ALL-UNNAMED",
  "--add-exports=jdk.compiler/com.sun.tools.javac.code=ALL-UNNAMED",
  "--add-exports=jdk.compiler/com.sun.tools.javac.file=ALL-UNNAMED",
  "--add-exports=jdk.compiler/com.sun.tools.javac.parser=ALL-UNNAMED",
  "--add-exports=jdk.compiler/com.sun.tools.javac.tree=ALL-UNNAMED",
  "--add-exports=jdk.compiler/com.sun.tools.javac.util=ALL-UNNAMED"
)

Test / javaOptions ++= java9Options
run / javaOptions ++= java9Options

val nativeTestClasspath = taskKey[Unit]("Write the Scala test classpath for the native tracing JVM")

nativeTestClasspath := Def.uncached {
  val converter = fileConverter.value
  val classpath = (Test / fullClasspath).value
    .map(entry => converter.toPath(entry.data).toString)
    .mkString(java.io.File.pathSeparator)
  IO.write(baseDirectory.value / ".temp" / "native-build" / "test-classpath.txt", classpath)
}


// https://github.com/sbt/sbt-dependency-graph
// sbt 'Compile / dependencyTree / toFile dep.tree -f'
// export COURSIER_TTL=0s # https://get-coursier.io/docs/ttl
