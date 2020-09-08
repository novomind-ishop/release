name := "release"

version := "1.0"

scalaVersion := "2.13.2"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings")

logLevel := Level.Warn

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3" // 283 k

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2" // 30 k

libraryDependencies += "redis.clients" % "jedis" % "3.3.0" // 540 k

libraryDependencies += "javax.xml.bind" % "jaxb-api" % "2.3.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"

libraryDependencies += "org.apache.maven.resolver" % "maven-resolver-connector-basic" % "1.4.2"

libraryDependencies += "org.apache.maven.resolver" % "maven-resolver-transport-file" % "1.4.2"

libraryDependencies += "org.apache.maven.resolver" % "maven-resolver-transport-http" % "1.4.2"

libraryDependencies += "org.apache.maven" % "maven-resolver-provider" % "3.6.3"

libraryDependencies += "org.jline" % "jline-terminal" % "3.15.0"

libraryDependencies += "org.jline" % "jline-reader" % "3.15.0"

libraryDependencies += "com.github.siom79.japicmp" % "japicmp" % "0.14.3"

libraryDependencies += "com.typesafe" % "config" % "1.4.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.2" % "test"

libraryDependencies += "org.scalatestplus" %% "junit-4-12" % "3.1.2.0" % Test

libraryDependencies += "org.mockito" %% "mockito-scala" % "1.14.3" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

unmanagedSourceDirectories in Compile := (scalaSource in Compile).value :: Nil

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", _ @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}

logLevel in assembly := Level.Warn

mainClass in assembly := Some("release.Starter")

assemblyJarName in assembly := "release.jar"

skip in publish := true
