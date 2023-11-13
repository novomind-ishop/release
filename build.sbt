name := "release"

version := "1.0"

scalaVersion := "2.13.12"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings", "--release:11",
  "-feature", "-language:implicitConversions")

logLevel := Level.Warn

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.11"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"

libraryDependencies += "javax.xml.bind" % "jaxb-api" % "2.3.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0"

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"

libraryDependencies += "org.apache.maven.resolver" % "maven-resolver-connector-basic" % "1.9.16"

libraryDependencies += "org.apache.maven.resolver" % "maven-resolver-transport-file" % "1.9.16"

libraryDependencies += "org.apache.maven.resolver" % "maven-resolver-transport-http" % "1.9.16"

libraryDependencies += "org.apache.maven.resolver" % "maven-resolver-supplier" % "1.9.16"

// libraryDependencies += "org.codehaus.plexus" % "plexus-utils" % "4.0.0"

libraryDependencies += "org.apache.maven" % "maven-resolver-provider" % "3.9.5"

//libraryDependencies += "io.get-coursier" %% "coursier" % "2.1.7"

libraryDependencies += "org.jline" % "jline-terminal" % "3.24.1"

libraryDependencies += "org.jline" % "jline-reader" % "3.24.1"

libraryDependencies += "com.github.siom79.japicmp" % "japicmp" % "0.18.3"

libraryDependencies += "com.typesafe" % "config" % "1.4.3"

libraryDependencies += "com.google.googlejavaformat" % "google-java-format" % "1.15.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"

libraryDependencies += "org.scalatestplus" %% "junit-4-13" % "3.2.15.0" % Test

libraryDependencies += "org.mockito" %% "mockito-scala" % "1.17.12" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

assembly / assemblyMergeStrategy := {
  case PathList("META-INF", _@_*) => MergeStrategy.discard
  case PathList("module-info.class") => MergeStrategy.discard
  case PathList("NOTICE") => MergeStrategy.concat
  case PathList("LICENSE") => MergeStrategy.concat
  case PathList("javax", "xml", _@_*) => MergeStrategy.first
  case PathList("javax", "activation", _@_*) => MergeStrategy.first
  case x => MergeStrategy.singleOrError
}

assembly / logLevel := Level.Warn

assembly / mainClass := Some("release.Starter")

assembly / assemblyJarName := "release.jar"

publish / skip := true

// https://github.com/sbt/sbt-dependency-graph
// sbt 'dependencyTree::toFile dep.tree -f'
// export COURSIER_TTL=0s # https://get-coursier.io/docs/ttl
