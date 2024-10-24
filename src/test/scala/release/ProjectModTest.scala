package release

import org.junit.rules.TemporaryFolder
import org.junit.{Assert, Rule, Test}
import org.mockito.ArgumentMatchers.anyString
import org.mockito.MockitoSugar._
import org.mockito.{ArgumentMatchers, Mockito}
import org.scalatestplus.junit.AssertionsForJUnit
import release.PomMod.DepTree
import release.ProjectMod.{Dep, Gav, Gav3, GavWithRef, SelfRef, UpdatePrinter}
import release.ProjectModTest.MockMod
import release.SbtModTest.d
import release.Starter.OptsDepUp

import java.io.File
import java.time.ZonedDateTime
import scala.collection.immutable.ListMap
import scala.util.{Failure, Success, Try}

object ProjectModTest {

  def parseSelfRef(id: String): SelfRef = {
    val splited = id.split(":").toSeq
    splited.size match {
      case 3 => SelfRef.ofGav(Gav(splited.head, splited(1), Some(splited(2))))
      case 4 => SelfRef.ofGav(Gav(splited.head, splited(1), Some(splited(2)), splited(3)))
      case size => throw new IllegalStateException(s"not valid id for ref splitting ${id} (${size})")
    }
  }

  def depOfUndef(groupId: String, artifactId: String, version: Option[String] = None, typeN: String = "",
                 scope: String = "", packaging: String = "", classifier: String = "", pomPath: Seq[String] = Nil): Dep = {
    depOf(pomRef = SelfRef.undef, groupId, artifactId, version, typeN, scope, packaging, classifier, pomPath)
  }

  def depOf(pomRef: SelfRef, groupId: String, artifactId: String, version: Option[String] = None, typeN: String = "",
            scope: String = "", packaging: String = "", classifier: String = "", pomPath: Seq[String] = Nil): Dep = {
    Dep(pomRef, groupId, artifactId, version, typeN, scope, packaging, classifier, pomPath)
  }

  def depOfShort(g: String, a: String, v: String, scope: String = ""): ProjectMod.Dep = {
    depOf(pomRef = SelfRef.undef, groupId = g, artifactId = a, version = Some(v), scope = scope)
  }

  class MockMod extends ProjectMod {

    override val file: File = new File("")
    override val opts: Starter.Opts = Starter.Opts()
    override lazy val repo: Repo = throw new IllegalStateException("use a mock please")
    override val selfVersion: String = "???"
    override val listDependencies: Seq[ProjectMod.Dep] = Nil
    override val listRawDeps: Seq[Dep] = Nil
    override val listPluginDependencies: Seq[ProjectMod.PluginDep] = Nil
    override val listProperties: Map[String, String] = Map("undef" -> "undef")

    override def isShop: Boolean = false

    override val skipPropertyReplacement: Boolean = false

    override val selfDepsMod: Seq[ProjectMod.Dep] = Nil

    override val depInFiles: Seq[(Dep, File)] = Nil

    override def suggestReleaseVersion(branchNames: Seq[String], tagNames: Seq[String], increment: Option[Increment]): Seq[String] = Nil

    override def suggestNextRelease(releaseVersion: String): String = "???"

    override def listSnapshotDependenciesDistinct: Seq[ProjectMod.Dep] = Nil

    override def writeTo(targetFolder: File): Unit = {

    }

    override def changeVersion(newVersion: String): Unit = {

    }

    override def changeDependecyVersion(patch: Seq[(ProjectMod.Gav3, String)]): Unit = {

    }

    override def depTreeFilenameList(): Seq[String] = Nil

    override def listRemoteRepoUrls(): Seq[String] = Nil
    override def getDepTreeFileContents: Map[File, DepTree] = Map.empty
  }

}

class ProjectModTest extends AssertionsForJUnit {

  val _temporarayFolder = new TemporaryFolder()

  @Rule def temp = _temporarayFolder
  implicit def toOpt(in: String): Option[String] = Option(in)

  @Test
  def testFilterCentral(): Unit = {
    val repo = Repo.ofUrl("http://localhost")
    Assert.assertEquals(Seq(repo), RepoProxy(Seq(Repo.ofUrl("http://central"), Repo.ofUrl(":"), repo)).repos)
  }

  @Test
  def testUnwantedLiteral(): Unit = {
    Assert.assertFalse(ProjectMod.isUnwanted(Gav3(groupId = "???", artifactId = "???", version = "1.0.0")))

    Assert.assertFalse(ProjectMod.isUnwanted(Gav3(groupId = "???", artifactId = "???", version = "1.0.0-android")))
    Assert.assertTrue(ProjectMod.isUnwanted(Gav3(groupId = "com.google.guava", artifactId = "guava", version = "1.0.0-android")))
  }

  @Test
  def testDepUpdateTwo(): Unit = {
    val now = ZonedDateTime.now()
    val anyDep = d("org.example", "any-library", "1.0.0")

    val term = Term.select("xterm", "os", simpleChars = false, isInteractice = false)
    val rootDeps: Seq[ProjectMod.Dep] = Seq(anyDep)
    val selfDepsModX: Seq[ProjectMod.Dep] = Nil

    val repoMock1 = mock[Repo]
    when(repoMock1.workNexusUrl()).thenReturn("repo1")
    when(repoMock1.getRelocationOf(anyString(), anyString(), anyString())).thenReturn(None)
    when(repoMock1.newerAndPrevVersionsOf(anyDep.groupId, anyDep.artifactId, anyDep.version.get)).thenReturn(Seq(anyDep.version.get, "2.13.1"))
    when(repoMock1.depDate(anyDep.groupId, anyDep.artifactId, anyDep.version.get)).thenReturn(Some(now))
    when(repoMock1.depDate(anyDep.groupId, anyDep.artifactId, "2.13.1")).thenReturn(Some(now))
    when(repoMock1.depDate(anyDep.groupId, anyDep.artifactId, "2.13.2")).thenReturn(None)

    val repoMock2 = mock[Repo]
    when(repoMock2.workNexusUrl()).thenReturn("repo2")
    when(repoMock2.getRelocationOf(anyString(), anyString(), anyString())).thenReturn(None)
    when(repoMock2.newerAndPrevVersionsOf(anyDep.groupId, anyDep.artifactId, anyDep.version.get)).thenReturn(Seq(anyDep.version.get, "2.13.2"))
    when(repoMock2.depDate(anyDep.groupId, anyDep.artifactId, anyDep.version.get)).thenReturn(Some(now))
    when(repoMock2.depDate(anyDep.groupId, anyDep.artifactId, "2.13.1")).thenReturn(None)
    when(repoMock2.depDate(anyDep.groupId, anyDep.artifactId, "2.13.2")).thenReturn(Some(now))

    val result = TermTest.withOutErr[Unit]()(sys => {
      val opts = OptsDepUp().copy(showLibYears = true)

      val innerResult: Seq[(GavWithRef, Seq[(String, Try[ZonedDateTime])])] = ProjectMod.collectDependencyUpdates(
        new UpdatePrinter(100, term, sys, printProgress = true), opts,
        rootDeps, selfDepsModX, RepoProxy(Seq(repoMock1, repoMock2)), checkOnline = false, ws = "")

      val any = GavWithRef(SelfRef.undef, anyDep.gav())
      val scala2 = (any, Seq(
        ("1.0.0", Success(now)),
        ("2.13.1", Failure(new UnsupportedOperationException("libyears are not computed for org.example:any-library:2.13.1"))),
        ("2.13.2", Success(now)),
      ))

      Assert.assertEquals(Seq(scala2).map(_._1), innerResult.map(_._1))
      Assert.assertEquals(Seq(scala2).map(_._2.filter(_._2.isSuccess)), innerResult.map(_._2.filter(_._2.isSuccess)))
      Assert.assertEquals(Seq(scala2).map(_._2.filter(_._2.isFailure).map(_._2.failed.get.getMessage))
        , innerResult.map(_._2.filter(_._2.isFailure).map(_._2.failed.get.getMessage)))
    })
    Assert.assertEquals("", result.err)
  }

  @Test
  def testScalaDeps(): Unit = {
    val now = ZonedDateTime.now()
    val scala = d("org.scala-lang", "scala-library", "2.13.0")

    val term = Term.select("xterm", "os", simpleChars = false, isInteractice = false)
    val rootDeps: Seq[ProjectMod.Dep] = Seq(scala)
    val selfDepsModX: Seq[ProjectMod.Dep] = Nil
    val repoMock = mock[Repo]
    when(repoMock.allRepoUrls()).thenReturn(Nil)
    Mockito.when(repoMock.createAll(ArgumentMatchers.any())).thenReturn(Seq(repoMock))
    Mockito.when(repoMock.allRepoZ()).thenReturn(Seq(repoMock))
    when(repoMock.getRelocationOf(anyString(), anyString(), anyString())).thenReturn(None)
    when(repoMock.newerAndPrevVersionsOf(scala.groupId, scala.artifactId, scala.version.get)).thenReturn(Seq("2.12.0", scala.version.get, "2.13.1"))
    when(repoMock.depDate(anyString(), anyString(), anyString())).thenReturn(None)
    when(repoMock.depDate(scala.groupId, scala.artifactId, scala.version.get)).thenReturn(Some(now))
    when(repoMock.depDate(scala.groupId, scala.artifactId, "2.13.1")).thenReturn(Some(now))
    when(repoMock.newerAndPrevVersionsOf(scala.groupId, "scala3-library_3", "-1")).thenReturn(Seq("-1", "3.0.1"))
    when(repoMock.depDate(scala.groupId, "scala3-library_3", "-1")).thenReturn(None)
    when(repoMock.depDate(scala.groupId, "scala3-library_3", "3.0.1")).thenReturn(Some(now.minusDays(1)))
    val result = TermTest.withOutErr[Unit]()(sys => {
      val opts = OptsDepUp().copy(showLibYears = true)

      val innerResult: Seq[(GavWithRef, Seq[(String, Try[ZonedDateTime])])] = ProjectMod.collectDependencyUpdates(
        new UpdatePrinter(100, term, sys, printProgress = true), opts,
        rootDeps, selfDepsModX, new RepoProxy(Seq(repoMock)), checkOnline = false, ws = "")

      val sca0 = GavWithRef(SelfRef.undef, Gav("org.scala-lang", "scala-library", "2.13.0"))
      val scala0 = (sca0, Seq(
        ("2.13.0", Success(now)),
        ("2.13.1", Success(now)),
      ))

      val sca3 = GavWithRef(SelfRef.undef, Gav("org.scala-lang", "scala3-library_3", "-1"))
      val scala3 = (sca3, Seq(
        ("-1", Failure(new UnsupportedOperationException("no date found for org.scala-lang:scala3-library_3:-1"))),
        ("3.0.1", Success(now.minusDays(1))),
      ))
      Assert.assertEquals(tryToString(Seq(scala0, scala3)), tryToString(innerResult))
      val updatePrinter = new UpdatePrinter(shellWidth = 100,
        termOs = term,
        sys = sys, printProgress = true)
      val resultMod = new MockMod {

        override lazy val repo: Repo = repoMock

        override private[release] def listGavsForCheck() = rootDeps

        override val selfDepsMod: Seq[Dep] = selfDepsModX
      }.collectDependencyUpdates(opts, checkOn = false, updatePrinter, ws = "")
      Assert.assertEquals(tryToString(Seq(scala0, scala3)), tryToString(resultMod._1))
    })
    Assert.assertEquals("", result.err)
  }

  def tryToString(e: Seq[(GavWithRef, Seq[(String, Try[ZonedDateTime])])]): Seq[(GavWithRef, Seq[(String, String)])] = {
    val r = e.map(t => (t._1, t._2.map(k => (k._1, k._2.toString))))
    r
  }

  @Test
  def testListDependeciesForCheck_empty(): Unit = {
    val testee = new ProjectModTest.MockMod()
    Assert.assertEquals(Nil, testee.listGavsForCheck())
  }

  @Test
  def testListDependenciesForCheck(): Unit = {
    val testee = new ProjectModTest.MockMod() {

      override val selfDepsMod: Seq[ProjectMod.Dep] = Seq(
        ProjectModTest.depOf(pomRef = ProjectModTest.parseSelfRef("ou:x:x"), groupId = "gg", artifactId = "a", version = Some("v")),
      )
      override val listDependencies: Seq[ProjectMod.Dep] = Seq(
        ProjectModTest.depOf(pomRef = SelfRef.undef, groupId = "g", artifactId = "a", version = Some("v")),
        ProjectModTest.depOf(pomRef = SelfRef.undef, groupId = "g", artifactId = "a", version = Some("v"), scope = "runtime"),
        ProjectModTest.depOf(pomRef = SelfRef.undef, groupId = "g", artifactId = "a", version = Some("v"), scope = "john"),
        ProjectModTest.depOf(pomRef = SelfRef.undef, groupId = "g", artifactId = "a", version = None, scope = "john"),
        ProjectModTest.depOf(pomRef = SelfRef.undef, groupId = "g", artifactId = "a", version = Some("v")),
        ProjectModTest.depOf(pomRef = ProjectModTest.parseSelfRef("bert:x:x"), groupId = "g", artifactId = "a", version = Some("v")),
        ProjectModTest.depOf(pomRef = ProjectModTest.parseSelfRef("se:x:x"), groupId = "gg", artifactId = "a", version = Some("v")),
      )
    }
    Assert.assertEquals(Seq(
      ProjectModTest.depOf(pomRef = SelfRef.undef, groupId = "g", artifactId = "a", version = Some("v"))
    ), testee.listGavsForCheck())

    Assert.assertEquals(Seq(
      Gav("g", "a", "v", scope = "john"),
      Gav("g", "a", None, scope = "john"),
    ), testee.listGavsWithUnusualScope())
  }

  @Test
  def testGav(): Unit = {
    Assert.assertTrue(Gav("g", "a", "v", scope = "john").feelsUnusual())
    Assert.assertFalse(Gav("g", "a", "v").feelsUnusual())
    Assert.assertTrue(Gav("g", "a", "()").feelsUnusual())
    Assert.assertTrue(Gav("g", "a", "RELEASE").feelsUnusual())
    Assert.assertTrue(Gav("g", "a", "LATEST").feelsUnusual())
  }

  @Test
  def testGroupSortReleases(): Unit = {

    val result = ProjectMod.groupSortReleases(Seq(
      "a",
      "1.1", "1-alpha",
      "2", "2.0",
      "5.10.0.202012080955-r", "5.9.0.202009080501-r",
      "10.0.0",
    ))

    Assert.assertEquals(Seq(
      ("10", Seq("10.0.0")),
      ("5", Seq("5.9.0.202009080501-r", "5.10.0.202012080955-r")),
      ("2", Seq("2", "2.0")),
      ("1", Seq("1-alpha", "1.1")),
      ("-1", Seq("a")),
    ), result)
  }

  @Test
  def testFilter(): Unit = {

    val now = ZonedDateTime.parse("2024-03-26T12:00:00+00:00")
    val in: ListMap[Gav3, Seq[(String, Try[ZonedDateTime])]] = ListMap(
      Gav3(groupId = "g", artifactId = "a", version = "1.0.0") -> Nil,
      Gav3(groupId = "g", artifactId = "a1", version = "1.0.0") -> Seq("1.0.0").map((_, Success(now))),
      Gav3(groupId = "g", artifactId = "a2", version = "1.0.0-SNAPSHOT") -> Seq("1.0.0").map((_, Success(now))),
      Gav3(groupId = "g", artifactId = "a3", version = "1.0.0-M1") -> Seq("1.0.0").map((_, Success(now))),
      Gav3(groupId = "g", artifactId = "a4", version = "1.0.1-SNAPSHOT") -> Seq("1.0.0", "1.0.1", "2.0.0").map((_, Success(now))),
      Gav3(groupId = "g", artifactId = "a5", version = "1.0.1-M2") -> Seq("2.0.0", "1.0.1", "1.0.0").map((_, Success(now))),
      Gav3(groupId = "g", artifactId = "a6", version = "ok") -> Seq("1.0.0").map((_, Success(now))),
      Gav3(groupId = "g", artifactId = "a7", version = "ok") -> Seq("1.0.0", "ok").map((_, Success(now))),
      Gav3(groupId = "g", artifactId = "a8", version = "1.0.0-SNAPSHOT") -> Seq("1.0.1").map((_, Success(now))),
    )
    val result = ProjectMod.removeOlderVersions(in)
    val expected = ListMap(
      Gav3(groupId = "g", artifactId = "a", version = "1.0.0") -> Nil,
      Gav3(groupId = "g", artifactId = "a1", version = "1.0.0") -> Nil,
      Gav3(groupId = "g", artifactId = "a2", version = "1.0.0-SNAPSHOT") -> Seq("1.0.0").map((_, Success(now))),
      Gav3(groupId = "g", artifactId = "a3", version = "1.0.0-M1") -> Seq("1.0.0").map((_, Success(now))),
      Gav3(groupId = "g", artifactId = "a4", version = "1.0.1-SNAPSHOT") -> Seq("1.0.1", "2.0.0").map((_, Success(now))),
      Gav3(groupId = "g", artifactId = "a5", version = "1.0.1-M2") -> Seq("1.0.1", "2.0.0").map((_, Success(now))),
      Gav3(groupId = "g", artifactId = "a6", version = "ok") -> Seq("1.0.0").map((_, Success(now))),
      Gav3(groupId = "g", artifactId = "a7", version = "ok") -> Seq("ok", "1.0.0").map((_, Success(now))),
      Gav3(groupId = "g", artifactId = "a8", version = "1.0.0-SNAPSHOT") -> Seq("1.0.1").map((_, Success(now))),
    )
    Assert.assertEquals(expected, result)
    Assert.assertEquals(ProjectMod.toDepChangeSeq(expected), ProjectMod.removeOlderVersions(ProjectMod.toDepChangeSeq(in)))
  }

  @Test
  def testShowDependencyUpdates_empty(): Unit = {
    val term = Term.select("xterm", "os", simpleChars = false, isInteractice = false)
    val rootDeps: Seq[ProjectMod.Dep] = Nil
    val selfDepsMod: Seq[ProjectMod.Dep] = Nil
    val repoMock = mock[Repo]
    when(repoMock.allRepoUrls()).thenReturn(Nil)
    Mockito.when(repoMock.allRepoZ()).thenReturn(Seq(repoMock))
    Mockito.when(repoMock.createAll(ArgumentMatchers.any())).thenReturn(Seq(repoMock))
    val result = TermTest.withOutErr[Unit]()(sys => {
      val innerResult: Seq[(GavWithRef, Seq[(String, Try[ZonedDateTime])])] = ProjectMod.collectDependencyUpdates(
        new UpdatePrinter(100, term, sys, printProgress = true), OptsDepUp(),
        rootDeps, selfDepsMod, new RepoProxy(Seq(repoMock)), checkOnline = false, ws = "")
      Assert.assertEquals(Nil, innerResult)
      val updatePrinter = new UpdatePrinter(shellWidth = 100,
        termOs = term,
        sys = sys, printProgress = true)
      val resultMod = new MockMod() {
        override lazy val repo: Repo = repoMock
      }.collectDependencyUpdates(OptsDepUp(), checkOn = false, updatePrinter, ws = "")
      Assert.assertEquals(Nil, resultMod._1)
    })
    Assert.assertEquals("", result.err)
    val filteredOut = result.out
      .linesIterator
      .map(line => {
        if (line.matches(".* dependencies in [0-9\\.]+ [mμs]+ \\(20[0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)$")) {
          line.replaceFirst(" in.*", " in ...")
        } else {
          line
        }
      })
      .mkString("\n")
    Assert.assertEquals(
      """
        |I: checking dependencies against nexus - please wait
        |I: checked 0 dependencies in ...
        |I: checking dependencies against nexus - please wait
        |I: checked 0 dependencies in ...
        |""".stripMargin.trim, filteredOut)

  }

  @Test
  def test_libyear_more(): Unit = {
    val timestamps: Seq[(String, Try[ZonedDateTime])] = Seq(
      ("1.0.9", Success(ZonedDateTime.parse("2024-02-13T08:19:20+01:00"))),
      ("1.0.7", Success(ZonedDateTime.parse("2024-02-14T08:19:20+01:00"))),
      ("2.0.4", Success(ZonedDateTime.parse("2024-02-10T08:19:20+01:00"))),
      ("2.0.5", Success(ZonedDateTime.parse("2024-02-13T08:19:20+01:00"))),
      ("2.0.7", Success(ZonedDateTime.parse("2024-02-15T08:19:20+01:00"))),
      ("2.0.9", Success(ZonedDateTime.parse("2024-02-16T08:19:20+01:00"))),
    )

    val gav = Gav3(groupId = "org.example", artifactId = "some", version = "2.0.7")
    Assert.assertEquals(" (libyears: 0Y 0M [-2 days])",
      ProjectMod.libyear(showYear = true, gav, major = Some("1"), timestamps, _ => {}))
    Assert.assertEquals(" (libyears: 0Y 0M [1 days])",
      ProjectMod.libyear(showYear = true, gav, major = Some("2"), timestamps, _ => {}))
  }

  @Test
  def test_libyear_two(): Unit = {
    val timestamps: Seq[(String, Try[ZonedDateTime])] = Seq(
      ("1.0.9", Success(ZonedDateTime.parse("2024-02-14T08:19:20+01:00"))),
      ("1.0.7", Success(ZonedDateTime.parse("2024-02-13T08:19:20+01:00"))),
    )

    val gav = Gav3(groupId = "org.example", artifactId = "some", version = "1.0.7")
    val result = ProjectMod.libyear(showYear = true, gav, major = Some("1"), timestamps, _ => {})
    Assert.assertEquals(" (libyears: 0Y 0M [1 days])", result)
  }

  @Test
  def test_libyear_one(): Unit = {
    val timestamps: Seq[(String, Try[ZonedDateTime])] = Seq(
      ("1.0.7", Success(ZonedDateTime.parse("2024-02-13T08:19:20+01:00"))),
    )

    val gav = Gav3(groupId = "org.example", artifactId = "some", version = "1.0.7")
    val result = ProjectMod.libyear(showYear = true, gav, major = Some("1"), timestamps, _ => {})
    Assert.assertEquals(" (libyears: ???)", result)
  }

  @Test
  def testFindOrphanTrees(): Unit = {

    // GIVEN
    val root = temp.newFolder("root")
    val file1 = new File(root, "dep.tree")
    file1.createNewFile()
    val file2 = temp.newFile("dep.tree")
    // WHEN
    val result = ProjectMod.findOrphanTrees(root, Seq(file2))

    // THEN
    Assert.assertEquals(Seq(file1.toPath), result)
  }

}
