package release

import com.typesafe.scalalogging.LazyLogging
import release.ProjectMod.{Dep, SelfRef}
import release.Starter.Opts

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import scala.jdk.CollectionConverters._
import scala.util.parsing.combinator.RegexParsers

case class SbtMod(file: File, repo: Repo, opts: Opts) extends ProjectMod {

  val selfVersion: String = "n/a" // TODO

  def sbtFile(file: File): Seq[File] = {
    val props = new File(new File(file.getParentFile, "project"), "build.properties")
    if (props.exists()) {
      Seq(props)
    } else {
      Nil
    }
  }

  val listDependecies: Seq[ProjectMod.Dep] = (Seq(file) ++ sbtFile(file))
    .map(f => Files.readAllLines(f.toPath, StandardCharsets.UTF_8)
      .asScala.mkString("\n"))
    .flatMap(SbtMod.SimpleParser.doParse)

  val listPluginDependencies: Seq[ProjectMod.PluginDep] = Nil // TODO

  val listProperties: Map[String, String] = Map("value-for-check" -> "TODO") // TODO

  val skipPropertyReplacement: Boolean = false

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

  def withRepo(workfolder: File, opts: Opts, aether: Repo): SbtMod = {
    SbtMod(buildSbt(workfolder), aether, opts)
  }

  object SimpleParser extends RegexParsers with LazyLogging {
    override val skipWhitespace = false

    def doParse(in: String): Seq[ProjectMod.Dep] = {

      def nl = "\n" ^^ (term => "NL")

      def ignore = ".*".r <~ nl ^^ (term => term)

      def sep = "[%]{1,2}".r ^^ (term => term)

      def word = "[^ ]+".r ^^ (term => {
        term
      })

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

      var sVersion: Option[String] = None
      var sbtVersionL: Option[String] = None

      def scalaVersion = "scalaVersion :=" ~> qWord ^^ (term => {
        sVersion = Some(term._1) // XXX side effect
        term
      })

      def sbtVersion = "sbt.version=" ~> word ^^ (term => {
        sbtVersionL = Some(term.stripLineEnd) // XXX side effect
        term
      })

      def dep = "libraryDependencies +=" ~> rep(qWord | test) <~ opt("[ ]+//.*".r) ~ nl ^^ (term => {
        if (term.size >= 3) {
          val addVersion = term.find(_._2.isDefined)
            .map(_ => {
              val suf = sVersion match {
                case lv if lv.get.startsWith("3") => lv.get.replaceFirst("\\.[0-9]+\\.[0-9]+$", "")
                case lv => lv.get.replaceFirst("\\.[0-9]+$", "")
              }
              "_" + suf
            })
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

      val p = rep(sbtVersion | scalaVersion | dep | ignore)

      val str = "\n" + in.linesIterator.map(_.trim).mkString("\n") + "\n"
      parseAll(p, str) match {
        case Success(vp, v) => {
          val scala = sVersion match {
            case None => Nil
            case lv if lv.get.startsWith("3") => Seq(Dep(SelfRef.undef,
              groupId = "org.scala-lang",
              artifactId = "scala3-library",
              version = lv.get,
              "", "", "", ""))
            case lv => Seq(Dep(SelfRef.undef,
              groupId = "org.scala-lang",
              artifactId = "scala-library",
              version = lv.get,
              "", "", "", ""))
          }

          val sbt = sbtVersionL match {
            case None => Nil
            case lv => Seq(Dep(SelfRef.undef,
              groupId = "org.scala-sbt",
              artifactId = "sbt",
              version = lv.get,
              "", "", "", ""))
          }
          val result = vp
            .filter(o => o.isInstanceOf[ProjectMod.Dep])
            .map(o => o.asInstanceOf[ProjectMod.Dep])
          sbt ++ scala ++ result
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
