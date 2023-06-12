package release

import japicmp.cmp.{JApiCmpArchive, JarArchiveComparator, JarArchiveComparatorOptions}
import japicmp.config.Options
import japicmp.filter.ClassFilter
import japicmp.model.JApiClass
import japicmp.output.semver.SemverOut
import javassist.CtClass
import release.Repo.VersionString
import release.Starter.{Opts, compressToGav, connectLeftRight}

import java.io.{File, FileNotFoundException, FileOutputStream}
import java.nio.file.Files
import java.util
import java.util.Collections
import java.util.jar.{JarEntry, JarFile}
import java.util.zip.ZipException
import scala.collection.parallel.CollectionConverters._
import scala.jdk.CollectionConverters._
import scala.util.{Try, Using}

object ApiDiff {
  def apidiff(inOpt: Opts, left: String, right: String): Opts = {
    println("")
    // TODO load api diff definition from file
    val repo = Repo.of(inOpt)
    val workDirFile = new File(".").getAbsoluteFile // TODO get this from other location

    val pommod = PomMod.withRepo(workDirFile, inOpt, repo)
    val gavs = pommod.listSelf.map(mo => mo.gav())

    val gas = gavs
      .map(gav => (gav.groupId, gav.artifactId, gav.packageing))
      .sorted
      .distinct
    val gasResolved: Seq[(Try[(File, VersionString, String)], Try[(File, VersionString, String)])] = gas.par
      .map(ga => {
        val groupId = ga._1
        val artifactId = ga._2
        println(s"download artifacts for diff of: ${groupId}:${artifactId}:(${left}..${right})")

        val ty = s":${ga._3}:"
        val ar = Repo.tryResolveReqWorkNexus(repo)(groupId + ":" + artifactId + ty + left).map(t => (t._1, t._2, ga._3))
        val br = Repo.tryResolveReqWorkNexus(repo)(groupId + ":" + artifactId + ty + right).map(t => (t._1, t._2, ga._3))
        (ar, br)
      }).seq
    if (inOpt.apiDiff.pomOnly) {
      println("pom-only-mode")

      def jarElements(jf: File): Option[(JarFile, util.Enumeration[JarEntry])] = {
        try {
          val jar = new JarFile(jf)
          val enumEntries = jar.entries
          Some(jar, enumEntries)
        } catch {
          case e: ZipException => {
            System.err.println("E: ZipException: " + e.getMessage + " " + jf.getAbsolutePath)
            None
          }
          case e: Exception => {
            e.printStackTrace()
            None
          }
        }

      }

      def extractPomFile(inFile: File, destFile: File): File = {
        destFile.mkdir()
        if (inFile.getName.endsWith(".jar") || inFile.getName.endsWith(".war")) {
          val jO = jarElements(inFile).getOrElse((null, Collections.enumeration(Nil.asJava)))
          val enumEntries = jO._2
          var fSide = Seq.empty[File]
          while (enumEntries.hasMoreElements) {
            val file: JarEntry = enumEntries.nextElement
            val f = new File(destFile, file.getName)
            if (file.isDirectory) {
              val mkd = f.mkdirs
              if (!mkd) {
                System.exit(1)
              }
            } else {
              try {
                if (file.getName.endsWith("pom.xml")) {
                  Using.resource(jO._1.getInputStream(file))(is => {
                    Using.resource(new FileOutputStream(f))(fos => {
                      while (is.available > 0) {
                        fos.write(is.read)
                      }
                    })
                  })
                  fSide = fSide :+ f
                }

              } catch {
                case e: FileNotFoundException => println(e.getMessage) // TODO handle
                case e: Exception => e.printStackTrace() // TODO handle
              }

            }
          }

          fSide.headOption.map(_.getParentFile).get
        } else if (inFile.getName.endsWith(".pom")) {
          if (inFile.exists()) {
            val src = inFile.toPath
            val target = new File(inFile.getParentFile, "pom.xml").toPath
            Files.move(src, target)
            target.toFile.getParentFile
          } else {
            throw new IllegalStateException("file not found " + inFile.getAbsolutePath)
          }

        } else {
          throw new IllegalStateException("unknown file " + inFile.getAbsolutePath)
        }

      }

      val oo: Seq[(
        (Seq[ProjectMod.Dep], Seq[ProjectMod.Gav3], Map[String, String]),
          (Seq[ProjectMod.Dep], Seq[ProjectMod.Gav3], Map[String, String]))] = gasResolved.flatMap(abr => {
        try {
          val ar = abr._1
          val br = abr._2
          if (ar.isSuccess && br.isSuccess) {

            val aPoms = extractPomFile(ar.get._1, Files.createTempDirectory("release-").toFile)
            val bPoms = extractPomFile(br.get._1, Files.createTempDirectory("release-").toFile)
            val modA = PomMod.withRepo(aPoms, inOpt, repo, skipPropertyReplacement = true, withSubPoms = false)
            val aDeps = modA.listDependencies
            val selfA = modA.selfDepsMod.map(_.gav().simpleGav())
            val modB = PomMod.withRepo(bPoms, inOpt, repo, skipPropertyReplacement = true, withSubPoms = false)
            val bDeps = modB.listDependencies

            val selfB = modB.selfDepsMod.map(_.gav().simpleGav())
            Seq(((aDeps, selfA, modA.listProperties), (bDeps, selfB, modB.listProperties)))
          } else {
            if (ar.isFailure) {
              println("W: " + ar.failed.get.getMessage)
            }
            if (br.isFailure) {
              println("W: " + br.failed.get.getMessage)
            }
            Nil
          }
        } catch {
          case e: Exception => {
            e.printStackTrace()
            Nil
          }
        }

      })

      val allSelf = (oo.flatMap(_._1._2) ++ oo.flatMap(_._2._2)).distinct
      val allProps: Map[String, String] = (oo.flatMap(_._1._3) ++ oo.flatMap(_._2._3)).distinct.toMap

      val t: (Seq[ProjectMod.Gav3], Seq[ProjectMod.Gav3]) = (compressToGav(allSelf, allProps)(oo.flatMap(_._1._1)),
        compressToGav(allSelf, allProps)(oo.flatMap(_._2._1)))

      println("diff:")
      val xDiff = connectLeftRight(t)

      def emptyTo(in: Seq[String], fill: String) = in match {
        case Nil => Seq(fill)
        case o => o
      }

      xDiff
        .map(line => emptyTo(line._1.map(_.formatted), "NEW").mkString(", ") + " => " +
          emptyTo(line._2.map(_.formatted), "REMOVED").mkString(", "))
        .foreach(println)
      Nil
    } else {
      val options = Options.newDefault()
      options.setIgnoreMissingClasses(true)
      options.setOutputOnlyModifications(true)

      options.setOutputOnlyBinaryIncompatibleModifications(inOpt.apiDiff.incompatibleModifications)

      val exclStartsWith = Set()
      val eStF = exclStartsWith.map(fv => new ClassFilter {
        override def matches(ctClass: CtClass): Boolean = ctClass.getPackageName.startsWith(fv)
      })
      val comparatorOptions: JarArchiveComparatorOptions = JarArchiveComparatorOptions.of(options)
      comparatorOptions.getFilters.getExcludes.addAll(eStF.asJava)
      val jApiClasses = gasResolved
        .flatMap(abr => {
          try {
            val ar = abr._1
            val br = abr._2
            if (ar.isSuccess && br.isSuccess) {
              if (ar.get._3 == "pom") {
                Nil
              } else {
                // TODO extract all poms and diff
                val a = new JApiCmpArchive(ar.get._1, ar.get._2)
                val b = new JApiCmpArchive(br.get._1, br.get._2)

                val jarArchiveComparator = new JarArchiveComparator(comparatorOptions)
                val out = jarArchiveComparator.compare(a, b)
                println(new XoutOutputGenerator(options, out).generate())
                out.asScala
              }
            } else {
              if (ar.isFailure) {
                println("W: " + ar.failed.get.getMessage)
              }
              if (br.isFailure) {
                println("W: " + br.failed.get.getMessage)
              }
              Nil
            }
          } catch {

            case e: Exception => {
              e.printStackTrace()
              Nil
            }
          }

        })

      println("Semver change: " + new SemverOut(options, new util.ArrayList[JApiClass](jApiClasses.asJava)).generate())
    }

    System.exit(5)
    null
  }

}
