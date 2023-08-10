name := "release"

version := "1.0"

scalaVersion := "2.13.10"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings", "--release:11",
  "-feature", "-language:implicitConversions")

logLevel := Level.Warn

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.11"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"

libraryDependencies += "javax.xml.bind" % "jaxb-api" % "2.3.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"

libraryDependencies += "org.apache.maven.resolver" % "maven-resolver-connector-basic" % "1.6.3"

libraryDependencies += "org.apache.maven.resolver" % "maven-resolver-transport-file" % "1.6.3"

libraryDependencies += "org.apache.maven.resolver" % "maven-resolver-transport-http" % "1.6.3"

libraryDependencies += "org.apache.maven" % "maven-resolver-provider" % "3.8.6"

libraryDependencies += "org.jline" % "jline-terminal" % "3.21.0"

libraryDependencies += "org.jline" % "jline-reader" % "3.21.0"

libraryDependencies += "com.github.siom79.japicmp" % "japicmp" % "0.17.1"

libraryDependencies += "com.typesafe" % "config" % "1.4.2"

libraryDependencies += "com.google.googlejavaformat" % "google-java-format" % "1.15.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"

libraryDependencies += "org.scalatestplus" %% "junit-4-13" % "3.2.15.0" % Test

libraryDependencies += "org.mockito" %% "mockito-scala" % "1.17.12" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

assembly / assemblyMergeStrategy := {
  case PathList("META-INF", _@_*) => MergeStrategy.discard
  case x => MergeStrategy.first
}

assembly / logLevel := Level.Warn

assembly / mainClass := Some("release.Starter")

assembly / assemblyJarName := "release.jar"

publish / skip := true

// https://github.com/sbt/sbt-dependency-graph
// sbt 'dependencyTree::toFile dep.tree -f'
// export COURSIER_TTL=0s # https://get-coursier.io/docs/ttl
