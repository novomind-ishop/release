package release

import org.junit.{Assert, Test}
import release.ProjectMod.Gav3

import java.util.regex.{Matcher, Pattern}

class DepTreeVersionTest {

  @Test
  def batchUpdatesModulesWithClassifiersAndScopes(): Unit = {
    val changes = Seq(
      Gav3("com.example", "first", Some("49x-SNAPSHOT")) -> "49.1.3",
      Gav3("com.example", "second", Some("49x-SNAPSHOT")) -> "49.1.3")
    val tree = """com.example:first:pom:49x-SNAPSHOT
      |+- com.example:first:jar:tests:49x-SNAPSHOT:test
      |+- com.example:second:war:49x-SNAPSHOT:compile
      |+- com.example:second:jar:sources:49x-SNAPSHOT:runtime
      |+- com.example:external:jar:49x-SNAPSHOT:compile
      |+- com.example:second:jar:48.1.0:compile
      |""".stripMargin
    val expected = """com.example:first:pom:49.1.3
      |+- com.example:first:jar:tests:49.1.3:test
      |+- com.example:second:war:49.1.3:compile
      |+- com.example:second:jar:sources:49.1.3:runtime
      |+- com.example:external:jar:49x-SNAPSHOT:compile
      |+- com.example:second:jar:48.1.0:compile
      |""".stripMargin
    Assert.assertEquals(expected, PomMod.depTreeVersionUpdater(changes)(tree))
  }

  @Test
  def batchPreservesLiteralRegexCharactersAndMavenProperties(): Unit = {
    val changes = Seq(
      Gav3("com.example", "library+api", Some("1.0.0")) -> "${revision}",
      Gav3("com.example", "library+api", Some("${revision}")) -> "2.0.0-SNAPSHOT")
    val tree = "+- com.example:library+api:jar:tests:1.0.0:test\n" +
      "+- comXexample:library+api:jar:1.0.0:compile\n" +
      "+- com.example:library+api:jar:1X0X0:compile\n"
    val expected = "+- com.example:library+api:jar:tests:2.0.0-SNAPSHOT:test\n" +
      "+- comXexample:library+api:jar:1.0.0:compile\n" +
      "+- com.example:library+api:jar:1X0X0:compile\n"
    Assert.assertEquals(expected, PomMod.depTreeVersionUpdater(changes)(tree))
  }

  @Test
  def batchPreservesSequentialReplacementBehavior(): Unit = {
    val changes = Seq(
      Gav3("com.example", "library", Some("1.0.0")) -> "1.0.0-SNAPSHOT",
      Gav3("com.example", "library", Some("1.0.0")) -> "1.0.0-SNAPSHOT",
      Gav3("com.example", "other", Some("${revision}")) -> "2.0.0"
    )
    val tree = "com.example:library:jar:1.0.0\r\n" +
      "+- com.example:other:jar:tests:${revision}:test\r\n" +
      "+- external:unchanged:jar:3.0.0-SNAPSHOT-SNAPSHOT-SNAPSHOT:compile\r\n"
    val update = PomMod.depTreeVersionUpdater(changes)
    Assert.assertEquals(originalUpdate(tree, changes), update(tree))
    Assert.assertEquals(originalUpdate("", changes), update(""))
    Assert.assertEquals(tree, PomMod.depTreeVersionUpdater(Nil)(tree))
    Assert.assertEquals(originalUpdate(tree, changes), update(tree))
  }

  @Test
  def batchMatchesPreviousImplementationForManyModulesAndTrees(): Unit = {
    val modules = (1 to 55).map(i => Gav3("com.example", s"module-${i}", Some("49x-SNAPSHOT")))
    // The project model includes repeated GAVs with different scopes, types and classifiers.
    val changes = (1 to 4).flatMap(_ => modules.map(_ -> "49.1.3"))
    val trees = (1 to 5).map(root =>
      s"com.example:module-${root}:pom:49x-SNAPSHOT\n" + modules.map(gav =>
        s"+- ${gav.groupId}:${gav.artifactId}:jar:tests:49x-SNAPSHOT:test\n" +
          s"+- ${gav.groupId}:${gav.artifactId}:jar:49x-SNAPSHOT:compile\n").mkString)

    val originalStart = System.nanoTime()
    val expected = trees.map(tree => originalUpdate(tree, changes))
    val originalMillis = (System.nanoTime() - originalStart) / 1_000_000
    val batchStart = System.nanoTime()
    val update = PomMod.depTreeVersionUpdater(changes)
    val actual = trees.map(update)
    val batchMillis = (System.nanoTime() - batchStart) / 1_000_000
    Assert.assertEquals(expected, actual)
    println(s"Dependency-tree comparison (55 modules, 5 trees): previous=${originalMillis}ms, batch=${batchMillis}ms")
  }

  private def originalUpdate(content: String, changes: Seq[(Gav3, String)]): String = {
    changes.foldLeft(content) { case (tree, (gav, newVersion)) =>
      val dependencyPattern = Pattern.quote(gav.groupId) + ":" + Pattern.quote(gav.artifactId)
      val versionPattern = Pattern.quote(gav.version.get)
      val prefix = Matcher.quoteReplacement(gav.groupId + ":" + gav.artifactId + ":")
      val version = Matcher.quoteReplacement(newVersion)
      tree.linesIterator
        .map(_.replaceFirst(dependencyPattern + ":([^:]*):([^:]*):" + versionPattern, prefix + "$1:$2:" + version))
        .map(_.replaceFirst(dependencyPattern + ":([^:]*):" + versionPattern, prefix + "$1:" + version))
        .map(_.replaceFirst("-SNAPSHOT-SNAPSHOT", "-SNAPSHOT"))
        .mkString("\n") + "\n"
    }
  }
}
