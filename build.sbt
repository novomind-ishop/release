name := "release"

version := "1.0"

scalaVersion := "2.12.4" // 5000 k

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3" // 283 k

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2" // 30 k

libraryDependencies += "redis.clients" % "jedis" % "2.9.0" // 540 k

libraryDependencies += "org.apache.maven.resolver" % "maven-resolver-connector-basic" % "1.1.0" // 41 k

libraryDependencies += "org.apache.maven.resolver" % "maven-resolver-transport-file" % "1.1.0" // 12 k

libraryDependencies += "org.apache.maven.resolver" % "maven-resolver-transport-http" % "1.1.0" // 40 k

libraryDependencies += "org.apache.maven" % "maven-resolver-provider" % "3.5.2" // 66 k

libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.4" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

unmanagedSourceDirectories in Compile := (scalaSource in Compile).value :: Nil

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}

mainClass in assembly := Some("release.Starter")

assemblyJarName in assembly := "release.jar"

