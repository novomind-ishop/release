package release

import release.ProjectMod.{Dep, Gav, PluginDep, SelfRef}
import release.Release.findBadLines
import release.Starter.{PreconditionsException}

import java.io.{File, PrintStream}
import java.nio.file.Path
import java.util.regex.Pattern
import scala.collection.parallel.CollectionConverters._

object PomChecker {

  def checkOwnArtifactNames(relevants: Seq[(Dep, File)], rootFile: File): Unit = {
    val problem = getOwnArtifactNames(relevants, rootFile)._2
    if (problem.isDefined) {
      throw new ValidationException(problem.get)
    }
  }

  def commonStriped(in: Seq[(ProjectMod.Gav2, File)]): Seq[(ProjectMod.Gav2, File)] = {

    val ma = in.map(e => ((Util.Similarity.spl(e._1.groupId), Util.Similarity.spl(e._1.artifactId)), e._1, e._2))
    val g = ma.flatMap(_._1._1).groupBy(a => a).map(k => (k._1, k._2.size)).filter(_._2 > 1).keySet.toSeq.sortBy(-_.length)
    val aa = ma.flatMap(_._1._2).groupBy(a => a).map(k => (k._1, k._2.size)).filter(_._2 > 1).keySet.toSeq.sortBy(-_.length)

    val regex = "(?<![a-z])[^a-z]|\\b[^a-z](?![a-z])".r
    val result = ma.map(a => {
      val str = aa.foldLeft(a._2.artifactId)((z, y) => regex.replaceAllIn(z.replaceFirst(y, ""), ""))
      val ng = a._2.copy(
        groupId = g.foldLeft(a._2.groupId)((z, y) => regex.replaceAllIn(z.replaceFirst(y, ""), "")),
        artifactId = str
      )
      (ng, a._3)
    }).filterNot(a => a._1.artifactId.isBlank && a._1.groupId.isBlank)
    result
  }

  def getOwnArtifactNames(relevants: Seq[(Dep, File)], rootFile: File): (Seq[ProjectMod.Gav2], Option[String]) = {
    implicit class Crossable[X](xs: Iterable[X]) {
      def cross[Y](ys: Iterable[Y]): Iterable[(X, Y)] = for {x <- xs; y <- ys} yield (x, y)
    }
    val simplified = relevants.map(e => (e._1.pomRef.gav3.toGav2(), e._2))
    val value1 = (simplified ++ commonStriped(simplified)).cross(simplified).toSeq
      .filterNot(k => k._1 == k._2)
      .map(t => {
        val tElementsSorted = Seq(t._1, t._2).sortBy(_.toString)
        (tElementsSorted(0), tElementsSorted(1))
      })

    val distinct = value1.distinct
    val value = distinct
      .map(t => (t._1, t._2, Util.Similarity.levenshtein(t._1._1.formatted, t._2._1.formatted)))
    prResult(simplified, value, rootFile)
  }

  def prResult(simplified: Seq[(ProjectMod.Gav2, File)],
               value: Seq[((ProjectMod.Gav2, File), (ProjectMod.Gav2, File), Int)],
               rootFile: File): (Seq[ProjectMod.Gav2], Option[String]) = {

    def rel(a: File, b: File): File = {
      b.toPath.relativize(a.toPath).toFile
    }

    val pr = value
      .filterNot(_._3 > 3)
      .filter(e => {
        val formatted = e._1._1
        val formatted1 = e._2._1
        val score = Util.Similarity.similarSplitMax(formatted, formatted1)
        score < 2
      })

    if (pr.nonEmpty) {
      val msg = pr.map(e => {
        val identOrSimilarMsg = if (e._1._1.formatted == e._2._1.formatted) {
          "identical"
        } else {
          "too similar"
        }

        "»" + e._1._1.formatted + "« (in " + rel(e._1._2, rootFile).toString + s") is ${identOrSimilarMsg} to »" +
          e._2._1.formatted + "« (in " + rel(e._2._2, rootFile).toString + ")"
      }).mkString("; ") + ". Please choose distinguishable names."
      (simplified.map(_._1), Some(msg))
    } else {
      (simplified.map(_._1), None)
    }
  }

  def printSnapshotsInFiles(gitFiles: Seq[String], out: PrintStream): Unit = {

    val snapsF: Seq[(Int, String, Path)] = getSnapshotsInFiles(gitFiles)

    if (snapsF != Nil) {
      out.println()
      out.println("Warning: Found SNAPSHOT occurrences in following files")
      snapsF.foreach(in => {
        out.println(in._3.toFile.getAbsolutePath + ":" + in._1)
        out.println("  " + in._2.trim())
      })
      out.println()
    }
  }

  def getSnapshotsInFiles(gitFiles: Seq[String]): Seq[(Int, String, Path)] = {
    val pattern = Pattern.compile("(?: |[^\\?\\*/\\\\]|^)[a-zA-Z_0-9\\-\\.]+-SNAPSHOT")
    val relevant = gitFiles.par
      .filterNot(in => in.endsWith(".list"))
      .filterNot(in => in.endsWith(".tree"))
      .filterNot(in => in.endsWith(".java"))
      .filterNot(in => in.endsWith(".png"))
      .filterNot(in => in.endsWith(".potx"))
      .filterNot(in => in.endsWith(".jpg"))
      .filterNot(in => in.matches(".*[/\\\\]src[/\\\\]test[/\\\\]resources[/\\\\]app\\-manifest.*"))
      .filterNot(in => in.endsWith("pom.xml"))
      .filterNot(in => in.endsWith(".mvn/extensions.xml"))
      .filterNot(in => in.endsWith("build.sbt"))
    val snapsF = relevant
      .flatMap(findBadLines(pattern))
      .seq
      .sortBy(_._3)
    snapsF
  }

  def checkExternalWithProjectScope(listRawDeps: Seq[Dep], selfDepsMod: Seq[Dep], listProperties: Map[String, String]) = {
    def k(iz: Seq[Dep]) = PomMod.replacedVersionProperties(listProperties.filter(t => t._1.startsWith("project")), skipPropertyReplacement = true)(iz)

    val externals = listRawDeps.filterNot(d => {
      selfDepsMod.map(_.gav()).contains(k(Seq(d)).head.gav()) || selfDepsMod.map(_.gav()).contains(d.gav())
    })
    val projectReplaced = k(externals)
    val relevant = externals.diff(projectReplaced).map(_.gav()).distinct
    if (relevant.nonEmpty) {
      throw new ValidationException(s"Project variables are not allowed in external dependencies: ${relevant.map(_.formatted).mkString(", ")}")
    }
  }

  def checkDepVersions(listDependecies: Seq[Dep], listDependeciesRaw: Seq[Dep]) = {
    val msgs = getDepVersionsProblems(listDependecies, listDependeciesRaw)
    if (msgs.nonEmpty) {
      throw new ValidationException(msgs.mkString("\n\n"))
    }
  }

  def getDepVersionsProblems(listDependecies: Seq[Dep], listDependeciesRaw: Seq[Dep]): Seq[String] = {

    val ziped = listDependecies.zip(listDependeciesRaw)
    val zg = ziped.groupBy(_._1).map(t => (t._1, t._2.map(_._2))).filterNot(_._2.size <= 1)

    val byRef = listDependecies
      .filterNot(_.version.getOrElse("").isBlank)
      .groupBy(_.pomRef)
    val msgs = (virtualParentRef(listDependecies) ++ byRef).flatMap(deps => {
      val value = deps._2.filterNot(_.pomPath == Seq("project", "build", "plugins", "plugin", "dependencies", "dependency"))
      val allGavs = value.flatMap(d => Seq(d) ++ zg.getOrElse(d, Nil)).map(_.gav()).distinct
      val withoutVersion = allGavs.map(_.copy(version = None)).groupBy(x => x).filter(_._2.size > 1).keySet
      val diff = allGavs.filter(g => withoutVersion.contains(g.copy(version = None)))
      if (diff.nonEmpty) {
        val msg = "found overlapping versions in\n" + deps._1.id + "\n" + diff.sortBy(_.toString).map(select => {
          val str = "  " + select.formatted
          str
        }).mkString("\n")
        Some(msg)
      } else {
        None
      }
    })
    msgs.toSeq
  }

  def checkDepScopes(listDependecies: Seq[Dep], listDependeciesRaw: Seq[Dep]) = {
    val msgs = getDepScopeAndOthers(listDependecies, listDependeciesRaw)
    if (msgs.nonEmpty) {
      throw new ValidationException(msgs.mkString("\n"))
    }
  }

  def virtualParentRef(listDependecies: Seq[Dep]): Map[SelfRef, Seq[Dep]] = {
    val parentRefs: Map[SelfRef, Seq[Dep]] = Map(SelfRef.virtualParent -> listDependecies.filter(_.pomPath.contains("parent")).distinctBy(_.gav()))
    parentRefs
  }

  def getDepScopeAndOthers(listDependecies: Seq[Dep], listDependeciesRaw: Seq[Dep]): Seq[String] = {
    val byRef: Map[SelfRef, Seq[Dep]] = listDependecies.groupBy(_.pomRef)

    val msgs = (virtualParentRef(listDependecies) ++ byRef).flatMap(deps => {
      val all = deps._2.map(_.gav())
      val allWithoutScope = all.map(_.copy(scope = "", packageing = ""))
      val withoutScope = allWithoutScope.distinct
      val times = all.map(_.copyWithNonEmptyScope()).groupBy(e => e).toSeq.map(e => (e._1, e._2.size)).filter(_._2 > 1)
      val prefix = s"found copies, use only one dependency in ${deps._1.id}\n"

      val copyMsg = times.map(e => {
        s"${prefix}  ${e._1.formatted} (times ${e._2}). This can also happen if you override an unused dependencyManagement."
      })
      val value = times.map(_._1).map(_.copy(scope = "", packageing = ""))
      val diff = Util.symmetricDiff(allWithoutScope, withoutScope).filterNot(e => {
        value.contains(e)
      })
      if (diff.nonEmpty) {

        val msg = "found overlapping scopes\n" + diff.map(select => {

          val str = select.formatted + "\n found in\n" +
            listDependecies.filter(dep => select == (dep.copy(scope = "").gav()))
              .map(d => s"${d.pomRef.id} with scope: ${d.gav().formatted.replace(select.formatted, "...")}")
              .mkString("\n")
          str
        }).mkString("\n\n")
        Seq(msg) ++ copyMsg
      } else {
        copyMsg
      }
    })
    msgs.toSeq
  }

  private val path = Seq("plugin", "plugins", "build", "project")

  private[release] def checkRootFirstChildPropertiesVar(opts: Opts, childPomFiles: Seq[RawPomFile]): Unit = {
    case class DepProps(dep: Dep, parentDep: Dep, properties: Map[String, String])

    val allP: Seq[DepProps] = childPomFiles.flatMap(in => {
      in.selfDep.map(s => {
        DepProps(s, in.parentDep.getOrElse(s), PomMod.createPropertyMap(in.document).view.filterKeys(key => !opts.skipProperties.contains(key)).toMap)
      })
    })

    def ga(gav: Gav): String = Gav.format(Seq(gav.groupId, gav.artifactId, gav.version.getOrElse("")))

    val depProps = Util.groupedFiltered(allP)
    val depPropsMod = depProps.toList.map(in => {
        val inner = in._2.filter(o => {
          val a = ga(o.parentDep.gav())
          val b = ga(in._1.dep.gav())
          a == b
        })
        (in._1, inner)
      }).filter(_._2 != Nil)
      .map(in => {
        val allPropsFromDocs: Seq[(String, String)] = in._2.flatMap(_.properties) ++ in._1.properties
        val withRootProps: Seq[(String, String)] = Util.symmetricDiff(allPropsFromDocs, allPropsFromDocs.distinct)
        val diff = withRootProps intersect allPropsFromDocs
        if (diff.nonEmpty) {
          (diff, (Seq(in._1) ++ in._2).map(_.dep.gav()))
        } else {
          (Nil, Nil)
        }

      }).filter(_._2 != Nil)

    if (depPropsMod != Nil) {
      throw new ValidationException("unnecessary/multiple property definition (move property to parent pom or remove from sub poms):\n" +
        depPropsMod.map(in => "  " + in._1.map(t => "(" + t._1 + " -> " + t._2 + ")").mkString(", ") +
          "\n      -> " + in._2.map(ga).mkString("\n      -> ")).mkString("\n"))
    }

  }

  def checkGavFormat(deps: Seq[ProjectMod.Dep], out: PrintStream): Unit = {
    val unusual: Seq[ProjectMod.Gav] = deps.map(_.gav()).filter(_.feelsUnusual()).sortBy(_.formatted)

    if (unusual != Nil) {
      out.println()
      out.println("Warning: Found dependencies with unusual symbols - please check your dependencies")
      unusual.foreach(in => {
        out.println("  \"" + in.formatted + "\"")
      })
      out.println()
    }
  }

  def checkPlugins(plugins: Seq[PluginDep]): Unit = {
    checkIshopMaven(plugins)
    val depPlugins = PomMod.dependecyPlugins(plugins)
    depPlugins.foreach(plugin => {
      if (plugin.execs == Nil) {
        throw new ValidationException("please add at least one execution to you maven-dependency-plugin")
      } else {
        val treeOrLists = plugin.execs.filter(in => in.goals.contains("tree") || in.goals.contains("list"))
        val invalidPhase = treeOrLists.filter(_.phase != "validate")
        if (invalidPhase != Nil) {
          throw new ValidationException("maven-dependency-plugin goals " +
            invalidPhase.flatMap(_.goals).mkString(", ") + " must be executed on phase \"validate\"")
        } else {
          val defectconfig = treeOrLists.filter(in => {
            val outputFileValue = in.config.get("outputFile")
            outputFileValue.isEmpty || outputFileValue.get.contains("/")
          })
          if (defectconfig != Nil) {
            throw new ValidationException("Please check your pom.xml's. The maven-dependency-plugin execution with id " +
              defectconfig.map(_.id).mkString(", ") + " has no configuration-element or the outputFile-tag contains slashes")
          }
          checkDefaultPath(plugin)
        }
      }
    })
  }

  private[release] def checkIshopMaven(_plugins: Seq[PluginDep]): Unit = {
    val plugins = PomMod.findPluginsByName(_plugins, "ishop-maven-plugin")
    // TODO ishop maven plugin muss definiert sein
    plugins.foreach(ishopMaven => {
      val execs = Util.only(ishopMaven.execs, ishopMaven.pomRef.id + " - a single execution section for ishop maven plugin please required")
      if (!execs.goals.contains("check-for-changes-before")) {
        throw new ValidationException("please add \"check-for-changes-before\" to your ishop maven plugin")
      }
      if (!execs.goals.contains("check-for-changes-package")) {
        throw new ValidationException("please add \"check-for-changes-package\" to your ishop maven plugin")
      }

      checkDefaultPath(ishopMaven)
    })
  }

  private def checkDefaultPath(plugin: PluginDep): Unit = {
    if (plugin.pomPath != path) {
      throw new ValidationException("please check your pom.xml's and move your " + plugin.artifactId + " to " +
        path.reverse.map(in => s"<$in>").mkString("/") +
        ". Your path in xml is " + plugin.pomPath.reverse.map(in => s"<$in>").mkString("/"))
    }
  }

  class ValidationException(msg: String) extends PreconditionsException(msg)

}
