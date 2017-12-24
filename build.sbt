name := "release"

version := "1.0"

scalaVersion := "2.12.3"

libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "4.8.0.201706111038-r"

libraryDependencies += "org.jdom" % "jdom2" % "2.0.6"

libraryDependencies += "jaxen" % "jaxen" % "1.1.6"

libraryDependencies += "xerces" % "xercesImpl" % "2.11.0"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2"

libraryDependencies += "redis.clients" % "jedis" % "2.9.0"

libraryDependencies += "org.apache.httpcomponents" % "httpclient" % "4.5.3"

libraryDependencies += "org.apache.maven.resolver" % "maven-resolver-api" % "1.1.0"

libraryDependencies += "org.apache.maven.resolver" % "maven-resolver-spi" % "1.1.0"

libraryDependencies += "org.apache.maven.resolver" % "maven-resolver-util" % "1.1.0"

libraryDependencies += "org.apache.maven.resolver" % "maven-resolver-impl" % "1.1.0"

libraryDependencies += "org.apache.maven.resolver" % "maven-resolver-connector-basic" % "1.1.0"

libraryDependencies += "org.apache.maven.resolver" % "maven-resolver-transport-file" % "1.1.0"

libraryDependencies += "org.apache.maven.resolver" % "maven-resolver-transport-http" % "1.1.0"

libraryDependencies += "org.apache.maven" % "maven-resolver-provider" % "3.5.0"

libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.4" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

unmanagedSourceDirectories in Compile := (scalaSource in Compile).value :: Nil

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}

mainClass in assembly := Some("release.Starter")

assemblyJarName in assembly := "release.jar"

