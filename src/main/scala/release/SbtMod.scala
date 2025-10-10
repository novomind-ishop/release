package release

import com.typesafe.scalalogging.LazyLogging
import release.PomMod.DepTree
import release.ProjectMod.{Dep, Gav3, SelfRef}
import release.SbtMod.SbtModel

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.concurrent.atomic.AtomicBoolean
import scala.jdk.CollectionConverters.*
import scala.util.parsing.combinator.RegexParsers

case class SbtMod(file: File, repoZ: RepoZ, opts: Opts) extends ProjectMod {

  override lazy val repo: RepoZ = repoZ
  override lazy val depInFiles: Seq[(ProjectMod.Dep, File)] = Nil // TODO
  private val value: SbtModel = {

    def read(f: File) = Files.readAllLines(f.toPath, StandardCharsets.UTF_8)
      .asScala.mkString("\n")

    val allFiles = sbtFile(file)
      .map(read)
    val main = read(file)
    val mainModel = SbtMod.SloppyParser.doParse()(main)

    val otherDeps = allFiles
      .map(SbtMod.SloppyParser.doParse()).flatMap(_.deps)
    mainModel.copy(deps = mainModel.deps ++ otherDeps)
  }
  override val listDependencies: Seq[ProjectMod.Dep] = value.deps
  override val listDependenciesPlugin: Seq[ProjectMod.Dep] = Nil
  override val listRawDeps: Seq[ProjectMod.Dep] = listDependencies

  val selfVersion: String = {
    value.selfVersion.getOrElse("n/a")
  }

  def sbtFile(file: File): Seq[File] = {
    val props = new File(new File(file.getParentFile, "project"), "build.properties")
    val plugins = new File(new File(file.getParentFile, "project"), "plugins.sbt")
    Seq(props, plugins).filter(_.exists())
  }

  val listPluginDependencies: Seq[ProjectMod.PluginDep] = Nil // TODO

  val listProperties: Map[String, String] = Map("value-for-check" -> "TODO") // TODO

  def listRemoteRepoUrls(): Seq[String] = {
    // TODO cat ~/.sbt/repositories
    Nil
  }

  def getDepTreeFileContents: Map[File, DepTree] = Map.empty

  val skipPropertyReplacement: Boolean = false

  def isShop: Boolean = false

  val selfDepsMod: Seq[ProjectMod.Dep] = Nil // TODO

  def suggestReleaseVersion(branchNames: Seq[String], tagNames: Seq[String], increment: Option[Increment] = None): Seq[String] = throw new UnsupportedOperationException()

  def suggestNextRelease(releaseVersion: String): String = throw new UnsupportedOperationException()

  def listSnapshotDependenciesDistinct: Seq[ProjectMod.Dep] = throw new UnsupportedOperationException()

  def writeTo(targetFolder: File): Unit = throw new UnsupportedOperationException()

  def changeVersion(newVersion: String): Unit = throw new UnsupportedOperationException()

  def changeDependecyVersion(patch: Seq[(Gav3, String)]): Unit = throw new UnsupportedOperationException()

  def depTreeFilenameList(): Seq[String] = throw new UnsupportedOperationException()
}

object SbtMod {

  case class SelfVersion(version: Option[String])

  case class SbtVersion(version: Option[String])

  case class ScalaVersion(version: Option[String])

  case class Predefined(value: String)

  case class Val(value: String)

  case class ValDef(key: String, value: String)

  case class Dep(groupId: Either[String, Val], artifactId: Either[String, Val], version: Either[String, Val], typeN: String,
                 scope: String, packaging: String, classifier: String) {
  }

  case class SbtModel(deps: Seq[ProjectMod.Dep], selfVersion: Option[String])

  def buildSbt(file: File): File = {
    new File(file, "build.sbt")
  }

  def withRepo(workfolder: File, opts: Opts, repo: RepoZ): SbtMod = {
    SbtMod(buildSbt(workfolder), repo, opts)
  }

  object SloppyParser extends RegexParsers with LazyLogging {
    override val skipWhitespace = false

    def doParse(strict: Boolean = false)(in: String): SbtModel = {

      def nl = "\n" ^^ (term => "NL")

      def predfined = "(^[ ]*(//|publish|scalacOptions|name|logLevel|assembly).*)".r <~ nl ^^ (term => Predefined(term))

      def valP = "(^val[ ]+)".r ~> word ~ " = " ~ word <~ nl ^^ (term => ValDef(term._1._1, term._2))

      def ignore = ".*".r <~ nl ^^ (term => term)

      def sep = "[%]{1,2}".r ^^ (term => term)

      def word = "[^\n ]+".r ^^ (term => {
        term
      })

      trait DepLineElement {
        val value: String
      }

      case class ValWord(value: String) extends DepLineElement
      case class LiteralWord(value: String) extends DepLineElement
      case class PredefWord(value: String) extends DepLineElement

      def word3: Parser[(ValWord, None.type)] = "[ ]+".r ~> word <~ opt("[ ]+".r ~ sep) ^^ (term => {
        (ValWord(term), None)
      })

      def quotedWord: Parser[(LiteralWord, Option[Unit])] = "[ ]*\"".r ~> "[^\"]+".r ~ "\"" ~ opt("[ ]+".r ~ sep) ^^ (term => {
        val a = term._2.map(_._2)
        val b = term._1._1
        if (a.contains("%%")) {
          (LiteralWord(b), Some(()))
        } else {
          (LiteralWord(b), None)
        }
      })

      def test: Parser[(PredefWord, None.type)] = "[ ]+".r ~> "Test".r ^^ (term => (PredefWord(term), None))

      var sVersion: Option[String] = None
      var selfVersionL: Option[String] = None
      var sbtVersionL: Option[String] = None

      def selfVersion = "version := " ~> quotedWord ^^ (term => {
        selfVersionL = Some(term._1.value) // XXX side effect
        SelfVersion(selfVersionL)
      })

      def scalaVersion = "scalaVersion := " ~> quotedWord ^^ (term => {
        sVersion = Some(term._1.value) // XXX side effect
        ScalaVersion(sVersion)
      })

      def sbtVersion = "sbt.version=" ~> word ^^ (term => {
        sbtVersionL = Some(term.stripLineEnd) // XXX side effect
        SbtVersion(sbtVersionL)
      })

      def toE(in: DepLineElement, addV: Option[String] = None): Either[String, Val] = {
        in match {
          case lw: LiteralWord => Left(lw.value + addV.getOrElse(""))
          case vw: ValWord => Right(Val(vw.value))
          case _ => throw new IllegalStateException("upasdf")
        }
      }

      def pluginDep = "addSbtPlugin(" ~> rep(quotedWord) <~ "[ ]*\\)".r ~ opt("[ ]+//.*".r) ~ nl ^^ (term => {
        // addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.6.1")
        val termL = term
        if (termL.size >= 3) {
          SbtMod.Dep(
            groupId = toE(termL(0)._1),
            artifactId = toE(termL(1)._1),
            version = toE(termL(2)._1),
            "a", "b", "c", "d")
        } else {
          SbtMod.Dep(
            groupId = toE(LiteralWord("TODO")),
            artifactId = toE(LiteralWord("TODO")),
            version = toE(LiteralWord("TODO")),
            "a", "b", "c", "d")
        }
      })

      def dep = "libraryDependencies += " ~> rep(test | quotedWord | word3) <~ opt("[ ]+//.*".r) ~ nl ^^ (term => {

        if (term.size >= 3) {
          val addVersion = term.find(_._2.isDefined)
            .map(_ => {
              val suf = sVersion match {
                case lv if lv.get.startsWith("3") => lv.get.replaceFirst("\\.[0-9]+\\.[0-9]+$", "")
                case lv => lv.get.replaceFirst("\\.[0-9]+$", "")
              }
              "_" + suf
            })

          SbtMod.Dep(
            groupId = toE(term(0)._1),
            artifactId = toE(term(1)._1, addVersion),
            version = toE(term(2)._1),
            "", "", "", ""
          )
        } else {
          SbtMod.Dep(
            groupId = toE(LiteralWord("TODO")),
            artifactId = toE(LiteralWord("TODO")),
            version = toE(LiteralWord("TODO")),
            "a", "b", "c", "d")
        }

      })

      val p = rep(pluginDep | sbtVersion | scalaVersion | selfVersion | dep | valP | predfined | ignore)

      val str = "\n" + in.linesIterator.map(_.trim).mkString("\n") + "\n"
      parseAll(p, str) match {
        case Success(vp, v) => {
          val scala = sVersion match {
            case None => Nil
            case lv if lv.get.startsWith("3") => Seq(ProjectMod.Dep(SelfRef.undef,
              groupId = "org.scala-lang",
              artifactId = "scala3-library_3",
              version = Some(lv.get),
              "", "", "", "", Nil))
            case lv => Seq(ProjectMod.Dep(SelfRef.undef,
              groupId = "org.scala-lang",
              artifactId = "scala-library",
              version = Some(lv.get),
              "", "", "", "", Nil))
          }

          val sbt = sbtVersionL match {
            case None => Nil
            case lv => Seq(ProjectMod.Dep(SelfRef.undef,
              groupId = "org.scala-sbt",
              artifactId = "sbt",
              version = Some(lv.get),
              "", "", "", "", Nil))
          }
          val allVals: Map[String, String] = vp.collect({ case o: ValDef => o })
            .map(kv => (kv.key -> kv.value))
            .map(t => t.copy(_2 = t._2.replaceFirst("""^\"""", "").replaceFirst("""\"$""", "")))
            .toMap

          def eval(d: Map[String, String])(et: Either[String, Val]): String = {
            et match {
              case s: Left[String, Val] => s.value
              case r: Right[String, Val] => d(r.value.value)
            }
          }

          val firstWarn = new AtomicBoolean(true)
          val result = vp
            .map(o => {
              if (!o.isInstanceOf[SbtMod.Dep]) {
                o match {
                  case str1: String if str1.isEmpty => // skip
                  case _: ScalaVersion => // skip
                  case _: SbtVersion => // skip
                  case _: SelfVersion => // skip
                  case _: Predefined => // skip
                  case _: ValDef => // skip
                  case in =>
                    if (strict) {
                      throw new IllegalStateException(s"SLOPPY SBT PARSER: ${in}")
                    } else {
                      if (firstWarn.getAndSet(false)) {
                        println()
                      }
                      println(s"SLOPPY SBT PARSER WARNING / skipped line: ${in}")
                    }

                }

              }
              o
            })
            .collect({ case e: SbtMod.Dep => e })
            .map(d => ProjectMod.Dep(SelfRef.undef,
              groupId = eval(allVals)(d.groupId),
              artifactId = eval(allVals)(d.artifactId),
              version = Some(eval(allVals)(d.version)),
              "", "", "", "", Nil))
          SbtModel(deps = sbt ++ scala ++ result, selfVersion = selfVersionL)
        }
        case f: Failure => {
          logger.warn("", new IllegalStateException("failure: " + f.toString() + " >" + in + "<"))
          SbtModel(deps = Nil, selfVersion = None)

        }
        case Error(msg, next) => {
          logger.warn("", new IllegalStateException("Syntax error - " + msg + " >" + in + "<"))
          SbtModel(deps = Nil, selfVersion = None)

        }
      }
    }
  }

}
