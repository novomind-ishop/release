package release

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import com.typesafe.scalalogging.LazyLogging
import release.ProjectMod.{Dep, SelfRef}
import release.Starter.Opts

import scala.jdk.CollectionConverters._
import scala.util.parsing.combinator.RegexParsers

case class SbtMod(file: File, aether: Aether, opts: Opts) extends ProjectMod {

  val selfVersion: String = "n/a" // TODO

  val listDependecies: Seq[ProjectMod.Dep] = Some(file)
    .map(f => Files.readAllLines(f.toPath, StandardCharsets.UTF_8).asScala.mkString("\n"))
    .map(SbtMod.SimpleParser.doParse)
    .getOrElse(Nil)

  val listPluginDependencies: Seq[ProjectMod.PluginDep] = Nil // TODO

  val listProperties: Map[String, String] = Map("value-for-check" -> "TODO") // TODO

  def isShop: Boolean = false

  def selfDepsMod: Seq[ProjectMod.Dep] = Nil // TODO

  def suggestReleaseVersion(branchNames: Seq[String]): Seq[String] = throw new UnsupportedOperationException()

  def suggestNextRelease(releaseVersion: String): String = throw new UnsupportedOperationException()

  def listSnapshotsDistinct: Seq[ProjectMod.Dep] = throw new UnsupportedOperationException()

  def writeTo(targetFolder: File): Unit = throw new UnsupportedOperationException()

  def changeVersion(newVersion: String): Unit = throw new UnsupportedOperationException()

  def depTreeFilenameList(): Seq[String] = throw new UnsupportedOperationException()
}

object SbtMod {
  def buildSbt(file: File): File = {
    new File(file, "build.sbt")
  }

  def ofAether(workfolder: File, opts: Opts, aether: Aether): SbtMod = {
    SbtMod(buildSbt(workfolder), aether, opts)
  }

  object SimpleParser extends RegexParsers with LazyLogging {
    override val skipWhitespace = false

    def doParse(in: String): Seq[ProjectMod.Dep] = {

      def nl = "\n" ^^ (term => "NL")

      def ignore = ".*".r <~ nl ^^ (term => term)

      def sep = "[%]{1,2}".r ^^ (term => term)

      def qWord = "[ ]+\"".r ~> "[^\"]+".r ~ "\"" ~ opt("[ ]+".r ~ sep) ^^ (term => {
        val a = term._2.map(_._2)
        val b = term._1._1
        if (a.contains("%%")) {
          (b, Some(()))
        } else {
          (b, None)
        }


      })

      def test = "[ ]+".r ~> "Test".r ^^ (term => (term, None))

      var sVersion = "NNNAAAA"

      def scalaVersion = "scalaVersion :=" ~> qWord ^^ (term => {
        sVersion = term._1 // XXX side effect
        term
      })

      def dep = "libraryDependencies +=" ~> rep(qWord | test) <~ opt("[ ]+//.*".r) ~ nl ^^ (term => {
        if (term.size >= 3) {
          val addVersion = term.find(_._2.isDefined)
            .map(_ => "_" + sVersion.replaceFirst("\\.[0-9]+$", ""))
          ProjectMod.Dep(SelfRef.undef,
            groupId = term(0)._1,
            artifactId = term(1)._1 + addVersion.getOrElse(""),
            version = term(2)._1,
            "", "", "", "")
        } else {
          ProjectMod.Dep(SelfRef.undef,
            groupId = "TODO",
            artifactId = "TODO",
            version = "TODO",
            "d", "e", "f", "g")
        }

      })

      val p = rep(scalaVersion | dep | ignore)

      val str = "\n" + in.linesIterator.map(_.trim).mkString("\n") + "\n"
      parseAll(p, str) match {
        case Success(vp, v) => {
          val scala = Dep(SelfRef.undef,
            groupId = "org.scala-lang",
            artifactId = "scala-library",
            version = sVersion,
            "", "", "", "")
          val result = vp
            .filter(o => o.isInstanceOf[ProjectMod.Dep])
            .map(o => o.asInstanceOf[ProjectMod.Dep])
          scala +: result
        }
        case f: Failure => {
          logger.warn("", new IllegalStateException("failure: " + f.toString() + " >" + in + "<"))
          Nil

        }
        case Error(msg, next) => {
          logger.warn("", new IllegalStateException("Syntax error - " + msg + " >" + in + "<"))
          Nil

        }
      }
    }
  }

}
