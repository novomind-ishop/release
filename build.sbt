name := "release"

version := "1.0"

scalaVersion := "2.13.7"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings")

logLevel := Level.Warn

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.7" // 283 k

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4" // 30 k

libraryDependencies += "javax.xml.bind" % "jaxb-api" % "2.3.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0"

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"

libraryDependencies += "org.apache.maven.resolver" % "maven-resolver-connector-basic" % "1.7.2"

libraryDependencies += "org.apache.maven.resolver" % "maven-resolver-transport-file" % "1.7.2"

libraryDependencies += "org.apache.maven.resolver" % "maven-resolver-transport-http" % "1.7.2"

libraryDependencies += "org.apache.maven" % "maven-resolver-provider" % "3.8.4"

libraryDependencies += "org.jline" % "jline-terminal" % "3.21.0"

libraryDependencies += "org.jline" % "jline-reader" % "3.21.0"

libraryDependencies += "com.github.siom79.japicmp" % "japicmp" % "0.15.4"

libraryDependencies += "com.typesafe" % "config" % "1.4.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"

libraryDependencies += "org.scalatestplus" %% "junit-4-13" % "3.2.10.0" % Test

libraryDependencies += "org.mockito" %% "mockito-scala" % "1.16.46" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

assembly / assemblyMergeStrategy := {
  case PathList("META-INF", _ @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}

assembly / logLevel := Level.Warn

assembly / mainClass := Some("release.Starter")

assembly / assemblyJarName := "release.jar"

publish / skip:= true

// https://github.com/sbt/sbt-dependency-graph
// sbt 'dependencyTree::toFile dep.tree -f'
// export COURSIER_TTL=0s # https://get-coursier.io/docs/ttl
