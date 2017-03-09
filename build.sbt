name := "release"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies += "io.reactivex" % "rxscala_2.11" % "0.26.4"

libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "4.5.0.201609210915-r"

libraryDependencies += "org.jdom" % "jdom2" % "2.0.6"

libraryDependencies += "com.google.guava" % "guava" % "20.0"

libraryDependencies += "jaxen" % "jaxen" % "1.1.6"

libraryDependencies += "xerces" % "xercesImpl" % "2.11.0"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.8"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"

libraryDependencies += "com.github.stefanbirkner" % "system-rules" % "1.15.1" % "test"

libraryDependencies += "com.typesafe" % "config" % "1.0.0"

libraryDependencies += "com.novocode" % "junit-interface" % "0.8" % "test->default"

libraryDependencies += "com.github.scala-incubator.io" % "scala-io-core_2.10" % "0.4.2"

libraryDependencies += "org.apache.httpcomponents" % "httpclient" % "4.3.6"

libraryDependencies += "org.eclipse.aether" % "aether-api" % "1.1.0"

libraryDependencies += "org.eclipse.aether" % "aether-spi" % "1.1.0"

libraryDependencies += "org.eclipse.aether" % "aether-util" % "1.1.0"

libraryDependencies += "org.eclipse.aether" % "aether-impl" % "1.1.0"

libraryDependencies += "org.eclipse.aether" % "aether-connector-basic" % "1.1.0"

libraryDependencies += "org.eclipse.aether" % "aether-transport-file" % "1.1.0"

libraryDependencies += ("org.eclipse.aether" % "aether-transport-http" % "1.1.0")

libraryDependencies += "org.apache.maven" % "maven-aether-provider" % "3.1.0"

libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.1"

unmanagedSourceDirectories in Compile := (scalaSource in Compile).value :: Nil

assemblyMergeStrategy in assembly := {
    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}


mainClass in assembly := Some("release.Starter")

assemblyJarName in assembly := "release.jar"
