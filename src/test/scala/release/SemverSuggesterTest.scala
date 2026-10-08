package release

import javassist.{ClassPool, CtNewMethod}
import org.junit.{Assert, Rule, Test}
import org.junit.rules.TemporaryFolder
import org.mockito.Mockito
import org.eclipse.aether.transfer.ArtifactNotFoundException
import release.ProjectMod.Gav

import java.io.{ByteArrayOutputStream, File, FileOutputStream, PrintStream}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.jar.{JarEntry, JarOutputStream}
import scala.util.{Failure, Success, Using}

class SemverSuggesterTest {
  @Rule def temporaryFolder: TemporaryFolder = folder
  private val folder = new TemporaryFolder()
  private val module = Gav("example", "library", Some("49x-SNAPSHOT"), "jar")

  private def jar(methods: Seq[String], externalSuperclass: Boolean = false): File = {
    val pool = new ClassPool(true)
    val clazz = pool.makeClass("example.Library")
    if (externalSuperclass) {
      val parent = pool.makeClass("example.ExternalBase")
      parent.toBytecode // Materialize its default constructor, but do not include it in the archive.
      clazz.setSuperclass(parent)
    }
    try {
      methods.foreach(method => clazz.addMethod(CtNewMethod.make(method, clazz)))
      val file = folder.newFile()
      Using.resource(new JarOutputStream(new FileOutputStream(file))) { stream =>
        stream.putNextEntry(new JarEntry("example/Library.class"))
        stream.write(clazz.toBytecode)
        stream.closeEntry()
      }
      file
    } finally clazz.detach()
  }

  private def repository(oldJar: File, newJar: File): RepoZ = {
    val repo = Mockito.mock(classOf[RepoZ])
    Mockito.when(repo.tryResolveReqWorkNexus("example:library:jar:49.1.3"))
      .thenReturn(Success((oldJar, "49.1.3")))
    Mockito.when(repo.tryResolveReqWorkNexus("example:library:jar:49x-SNAPSHOT"))
      .thenReturn(Success((newJar, "49x-20261001.123456-1")))
    repo
  }

  @Test def choosesHighestStableReleaseInMajorSeries(): Unit = {
    Assert.assertEquals(Some("49.2.0"),
      SemverSuggester.baseline("49x-SNAPSHOT",
        Seq("v49.1.9", "v49.2.0", "v50.0.0", "v49.3.0-RC1", "other")))
    Assert.assertEquals(Some("49.1.3"), SemverSuggester.baseline("49.1.4-SNAPSHOT", Seq("v49.1.3")))
    Assert.assertEquals(None, SemverSuggester.baseline("${revision}", Seq("v49.1.3")))
  }

  @Test def recommendsMinorForNewPublicMethod(): Unit = {
    val repo = repository(jar(Seq("public int value() { return 1; }")),
      jar(Seq("public int value() { return 1; }", "public int additional() { return 2; }")))
    Assert.assertEquals(
      SemverSuggester.Recommendation("49.1.3", "49x-SNAPSHOT", "minor", "49.2.0", 1, true),
      SemverSuggester.calculate("49x-SNAPSHOT", Seq("v49.1.3"), Seq(module), repo)
    )
    Mockito.verify(repo).tryResolveReqWorkNexus("example:library:jar:49.1.3")
    Mockito.verify(repo).tryResolveReqWorkNexus("example:library:jar:49x-SNAPSHOT")
  }

  @Test def recommendsMajorForRemovedPublicMethod(): Unit = {
    val repo = repository(jar(Seq("public int value() { return 1; }")),
      jar(Seq("public int additional() { return 2; }")))
    Assert.assertEquals(
      SemverSuggester.Recommendation("49.1.3", "49x-SNAPSHOT", "major", "50.0.0", 1, true),
      SemverSuggester.calculate("49x-SNAPSHOT", Seq("v49.1.3"), Seq(module), repo)
    )
  }

  @Test def recommendsPatchForImplementationChange(): Unit = {
    val repo = repository(jar(Seq("public int value() { return 1; }")),
      jar(Seq("public int value() { return 2; }")))
    Assert.assertEquals(
      SemverSuggester.Recommendation("49.1.3", "49x-SNAPSHOT", "patch", "49.1.4", 1, false),
      SemverSuggester.calculate("49x-SNAPSHOT", Seq("v49.1.3"), Seq(module), repo)
    )
  }

  @Test def missingArtifactDoesNotProducePartialRecommendation(): Unit = {
    val repo = repository(jar(Nil), jar(Nil))
    Mockito.when(repo.tryResolveReqWorkNexus("example:library:jar:49x-SNAPSHOT"))
      .thenReturn(Failure(new IllegalStateException("not found")))
    val result = SemverSuggester.calculate("49x-SNAPSHOT", Seq("v49.1.3"), Seq(module), repo)
    Assert.assertTrue(result.isInstanceOf[SemverSuggester.Unavailable])
    Assert.assertTrue(result.message.contains("example:library:jar:49x-SNAPSHOT"))
    Assert.assertTrue(result.message, result.message.contains("not found"))
  }

  @Test def downloadFailureShowsCauseWithoutUrlCredentials(): Unit = {
    val repo = repository(jar(Nil), jar(Nil))
    Mockito.when(repo.tryResolveReqWorkNexus("example:library:jar:49.1.3"))
      .thenReturn(Failure(new IllegalStateException("Artifact resolution failed",
            new java.io.IOException("Transfer failed from https://user:secret@nexus.example/releases:\nstatus code: 401"))))
    val result = SemverSuggester.calculate("49x-SNAPSHOT", Seq("v49.1.3"), Seq(module), repo)
    Assert.assertTrue(result.isInstanceOf[SemverSuggester.Unavailable])
    Assert.assertTrue(result.message, result.message.contains("status code: 401"))
    Assert.assertTrue(result.message, result.message.contains("nexus.example/releases"))
    Assert.assertFalse(result.message, result.message.contains("secret"))
    Assert.assertFalse(result.message, result.message.contains("\n"))
  }

  @Test def privateMethodDoesNotRequireMinor(): Unit = {
    val repo = repository(jar(Seq("public int value() { return 1; }")),
      jar(Seq("public int value() { return 1; }", "private int internal() { return 2; }")))
    Assert.assertEquals(
      SemverSuggester.Recommendation("49.1.3", "49x-SNAPSHOT", "patch", "49.1.4", 1, false),
      SemverSuggester.calculate("49x-SNAPSHOT", Seq("v49.1.3"), Seq(module), repo)
    )
  }

  @Test def missingSuperclassMakesRecommendationProvisional(): Unit = {
    val repo = repository(jar(Nil, externalSuperclass = true), jar(Nil, externalSuperclass = true))
    val result = SemverSuggester.calculate("49x-SNAPSHOT", Seq("v49.1.3"), Seq(module), repo)
      .asInstanceOf[SemverSuggester.Recommendation]
    Assert.assertEquals("patch", result.increment)
    Assert.assertTrue(result.incomplete)
    Assert.assertTrue(result.message, result.message.contains("missing classes ignored"))
    Assert.assertTrue(result.message, result.message.contains("provisional PATCH"))
  }

  @Test def excludesPomModulesFromDownloads(): Unit = {
    val repo = repository(jar(Seq("public int value() { return 1; }")), jar(Nil))
    val result = SemverSuggester.calculate("49x-SNAPSHOT", Seq("v49.1.3"),
      Seq(module, Gav("example", "parent", Some("49x-SNAPSHOT"), "pom")), repo)
    Assert.assertTrue(result.isInstanceOf[SemverSuggester.Recommendation])
    Mockito.verify(repo).tryResolveReqWorkNexus("example:library:jar:49.1.3")
    Mockito.verify(repo).tryResolveReqWorkNexus("example:library:jar:49x-SNAPSHOT")
    Mockito.verifyNoMoreInteractions(repo)
  }

  @Test def noBaselineDoesNotDownload(): Unit = {
    val repo = Mockito.mock(classOf[RepoZ])
    Assert.assertTrue(SemverSuggester.calculate("49x-SNAPSHOT", Nil, Seq(module), repo)
        .isInstanceOf[SemverSuggester.Unavailable])
    Mockito.verifyNoInteractions(repo)
  }

  @Test def fallsBackToPublishedTagButIncrementsNewestGitVersion(): Unit = {
    val oldJar = jar(Seq("public int value() { return 1; }"))
    val repo = repository(oldJar, oldJar)
    Mockito.when(repo.tryResolveReqWorkNexus("example:library:jar:49.1.3"))
      .thenReturn(Failure(Mockito.mock(classOf[ArtifactNotFoundException])))
    Mockito.when(repo.tryResolveReqWorkNexus("example:library:jar:49.1.2"))
      .thenReturn(Success((oldJar, "49.1.2")))
    val result = SemverSuggester.calculate("49x-SNAPSHOT", Seq("v49.1.2", "v49.1.3"), Seq(module), repo)
      .asInstanceOf[SemverSuggester.Recommendation]
    Assert.assertEquals("49.1.2", result.baseVersion)
    Assert.assertEquals("49.1.4", result.nextVersion)
    Assert.assertTrue(result.incomplete)
    Assert.assertTrue(result.message, result.message.contains("newest Git release is 49.1.3"))
  }

  @Test def newModuleFromGitManifestRequiresMinor(): Unit = {
    val repo = repository(jar(Nil), jar(Seq("public int value() { return 1; }")))
    val result = SemverSuggester.calculate("49x-SNAPSHOT", Seq("v49.1.3"), Seq(module), repo,
      releaseModules = Some(_ => Nil)).asInstanceOf[SemverSuggester.Recommendation]
    Assert.assertEquals("minor", result.increment)
    Assert.assertEquals("49.2.0", result.nextVersion)
    Assert.assertFalse(result.incomplete)
    Mockito.verify(repo, Mockito.never()).tryResolveReqWorkNexus("example:library:jar:49.1.3")
  }

  @Test def removedModuleFromGitManifestRequiresMajor(): Unit = {
    val repo = repository(jar(Seq("public int value() { return 1; }")), jar(Nil))
    val result = SemverSuggester.calculate("49x-SNAPSHOT", Seq("v49.1.3"), Nil, repo,
      releaseModules = Some(_ => Seq(module))).asInstanceOf[SemverSuggester.Recommendation]
    Assert.assertEquals("major", result.increment)
    Assert.assertEquals("50.0.0", result.nextVersion)
    Mockito.verify(repo, Mockito.never()).tryResolveReqWorkNexus("example:library:jar:49x-SNAPSHOT")
  }

  @Test def unavailableSnapshotIsNotMistakenForRemovedModule(): Unit = {
    val repo = repository(jar(Seq("public int value() { return 1; }")),
      jar(Seq("public int value() { return 1; }", "public int additional() { return 2; }")))
    val other = module.copy(artifactId = "other")
    Mockito.when(repo.tryResolveReqWorkNexus("example:other:jar:49.1.3"))
      .thenReturn(Success((jar(Nil), "49.1.3")))
    Mockito.when(repo.tryResolveReqWorkNexus("example:other:jar:49x-SNAPSHOT"))
      .thenReturn(Failure(new java.io.IOException("connection timed out")))
    val result = SemverSuggester.calculate("49x-SNAPSHOT", Seq("v49.1.3"), Seq(module, other), repo,
      releaseModules = Some(_ => Seq(module, other))).asInstanceOf[SemverSuggester.Recommendation]
    Assert.assertEquals("minor", result.increment)
    Assert.assertTrue(result.incomplete)
    Assert.assertTrue(result.message, result.message.contains("connection timed out"))
    Assert.assertFalse(result.message, result.message.contains("removed module"))
    Assert.assertEquals(1, result.modules)
  }

  @Test def accessFailureDoesNotTriggerOlderReleaseFallback(): Unit = {
    val repo = repository(jar(Nil), jar(Nil))
    Mockito.when(repo.tryResolveReqWorkNexus("example:library:jar:49.1.3"))
      .thenReturn(Failure(new java.io.IOException("status code: 403")))
    Assert.assertTrue(SemverSuggester.calculate("49x-SNAPSHOT", Seq("v49.1.3", "v49.1.2"), Seq(module), repo)
        .isInstanceOf[SemverSuggester.Unavailable])
    Mockito.verify(repo, Mockito.never()).tryResolveReqWorkNexus("example:library:jar:49.1.2")
  }

  @Test def readsHistoricalModuleManifestWithoutChangingWorktree(): Unit = {
    val directory = folder.newFolder()
    val git = Sgit.init(directory)
    git.configSetLocal("user.name", "Test")
    git.configSetLocal("user.email", "test@example.com")
    val pom = new File(directory, "pom.xml")
    val original = "<project><modelVersion>4.0.0</modelVersion><groupId>example</groupId>" +
      "<artifactId>old-name</artifactId><version>49.1.3</version></project>"
    java.nio.file.Files.writeString(pom.toPath, original)
    git.add(pom)
    git.commitAll("release")
    git.doTag("49.1.3")
    java.nio.file.Files.writeString(pom.toPath, original.replace("old-name", "new-name"))
    val status = git.localChanges()
    val modules = SemverSuggester.modulesAtTag(git, "v49.1.3", Opts(), Mockito.mock(classOf[RepoZ]))
    Assert.assertEquals(Seq("old-name"), modules.map(_.artifactId))
    Assert.assertEquals(status, git.localChanges())
    Assert.assertTrue(java.nio.file.Files.readString(pom.toPath).contains("new-name"))
  }

  @Test def downloadsInBackgroundAndReportsOnceAtPromptBoundary(): Unit = {
    val repo = repository(jar(Seq("public int value() { return 1; }")), jar(Nil))
    val entered = new CountDownLatch(1)
    val proceed = new CountDownLatch(1)
    val oldJar = jar(Seq("public int value() { return 1; }"))
    Mockito.when(repo.tryResolveReqWorkNexus("example:library:jar:49.1.3")).thenAnswer(_ => {
      entered.countDown()
      if (!proceed.await(5, TimeUnit.SECONDS)) throw new IllegalStateException("test download timed out")
      Success((oldJar, "49.1.3"))
    })
    val output = new ByteArrayOutputStream()
    val out = new PrintStream(output)
    val running = SemverSuggester.start("49x-SNAPSHOT", Seq("v49.1.3"), Seq(module), repo)
    try {
      Assert.assertTrue(entered.await(5, TimeUnit.SECONDS))
      Assert.assertFalse(running.reportIfReady(out))
      Assert.assertEquals("", output.toString)
      proceed.countDown()
      Assert.assertTrue(running.reportIfReady(out, 5000L))
      val message = output.toString
      Assert.assertTrue(message.contains("MAJOR: 50.0.0"))
      Assert.assertTrue(running.reportIfReady(out))
      Assert.assertEquals(message, output.toString)
    } finally {
      proceed.countDown()
      running.cancel()
      out.close()
    }
  }
}
