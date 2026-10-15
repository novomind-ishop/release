package release

import com.sun.net.httpserver.HttpServer
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.net.{InetSocketAddress, URLDecoder}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
import java.nio.file.{Files, Path}
import org.junit.{Assert, Rule, Test}
import org.junit.rules.TemporaryFolder
import scala.jdk.CollectionConverters.*
import scala.sys.process.*

class ReleaseGitlabPushTest {
  private val folder = new TemporaryFolder()
  @Rule def temp: TemporaryFolder = folder

  private def git(repo: Path, args: String*): String =
    Process(Seq("git", "-C", repo.toString) ++ args).!!.trim

  private def withRepo(test: (Path, Path) => Unit): Unit = {
    val repo = folder.newFolder("local").toPath
    val remote = folder.newFolder("remote.git").toPath
    git(remote, "init", "--bare")
    git(remote, "config", "receive.advertisePushOptions", "true")
    val hook = remote.resolve("hooks/pre-receive")
    Files.writeString(
      hook,
      "#!/bin/sh\ni=0\nwhile [ \"$i\" -lt \"$GIT_PUSH_OPTION_COUNT\" ]; do\n" +
        "  eval \"option=\\$GIT_PUSH_OPTION_$i\"\n" +
        "  printf '%s\\n' \"$option\" >> \"$GIT_DIR/push-options\"\n" +
        "  i=$((i + 1))\ndone\n"
    )
    Assert.assertTrue(hook.toFile.setExecutable(true))
    git(repo, "init", "--initial-branch=main")
    git(repo, "config", "user.name", "Release Test")
    git(repo, "config", "user.email", "release@example.invalid")
    git(repo, "config", "commit.gpgsign", "false")
    Files.writeString(repo.resolve("version.txt"), "1.2.3\n")
    git(repo, "add", ".")
    git(repo, "commit", "-m", "Release 1.2.3")
    git(repo, "tag", "v1.2.3")
    git(repo, "branch", "release/1.2.3")
    git(repo, "remote", "add", "origin", remote.toString)
    git(repo, "push", "-u", "origin", "main")
    Files.writeString(repo.resolve("version.txt"), "1.2.4-SNAPSHOT\n")
    git(repo, "commit", "-am", "Prepare next iteration\n\nTask: ABC-123")
    test(repo, remote)
  }

  private def publish(repo: Path, isShop: Boolean = false, changedVersion: Boolean = true, answer: String = "y",
      envs: Map[String, String] = Map.empty): String = {
    val output = new ByteArrayOutputStream()
    val sys = new Term.Sys(new ByteArrayInputStream((answer + "\n").getBytes(java.nio.charset.StandardCharsets.UTF_8)), output, output)
    val opts = Opts(useJlineInput = false)
    val sgit = Sgit(repo.toFile, doVerify = false, out = sys.out, err = sys.err, gitBin = Some("git"), opts = opts)
    Release.publishToGitlab(sys, sgit, opts, "main", "main", changedVersion,
      if (isShop) "1.2.3-SNAPSHOT" else "1.2.3", isShop, envs)(sys.out.println("MANUAL"))
    output.toString("UTF-8")
  }

  @Test def publishesNextIterationAsMergeRequestAndReleaseTag(): Unit = withRepo { (repo, remote) =>
    val result = publish(repo)
    Assert.assertTrue(result, result.contains("done."))
    Assert.assertFalse(result, result.contains("MANUAL"))
    Assert.assertEquals(git(repo, "rev-parse", "v1.2.3"), git(remote, "rev-parse", "refs/tags/v1.2.3"))
    Assert.assertEquals(git(repo, "rev-parse", "main^"), git(remote, "rev-parse", "refs/heads/main"))
    Assert.assertEquals("refs/heads/main", git(repo, "config", "branch.main.merge"))
    val reviewBranches =
      git(remote, "for-each-ref", "--format=%(refname)", "refs/heads/").linesIterator.filterNot(_ == "refs/heads/main").toSeq
    Assert.assertEquals(1, reviewBranches.size)
    Assert.assertEquals(git(repo, "rev-parse", "main"), git(remote, "rev-parse", reviewBranches.head))
    val options = Files.readAllLines(remote.resolve("push-options")).asScala.toSeq
    Assert.assertTrue(options.contains("merge_request.create"))
    Assert.assertTrue(options.contains("merge_request.target=main"))
    Assert.assertTrue(options.contains("merge_request.title=Prepare next iteration"))
    Assert.assertTrue(options.contains("merge_request.description=Task: ABC-123"))
    Assert.assertFalse(options.contains("merge_request.auto_merge"))
  }

  @Test def publishesShopBranchWithoutNextIterationChange(): Unit = withRepo { (repo, remote) =>
    val result = publish(repo, isShop = true, changedVersion = false)
    Assert.assertTrue(result, result.contains("done."))
    Assert.assertEquals(git(repo, "rev-parse", "release/1.2.3"), git(remote, "rev-parse", "refs/heads/release/1.2.3"))
    Assert.assertEquals("", git(remote, "for-each-ref", "refs/tags/"))
    Assert.assertFalse(Files.exists(remote.resolve("push-options")))
  }

  @Test def detachedCheckoutStillPublishesTagWithoutMergeRequest(): Unit = withRepo { (repo, remote) =>
    git(repo, "checkout", "--detach")
    val result = publish(repo)
    Assert.assertTrue(result, result.contains("done."))
    Assert.assertEquals(git(repo, "rev-parse", "v1.2.3"), git(remote, "rev-parse", "refs/tags/v1.2.3"))
    Assert.assertFalse(Files.exists(remote.resolve("push-options")))
  }

  @Test def declinedPushShowsManualCommands(): Unit = withRepo { (repo, remote) =>
    val result = publish(repo, answer = "n")
    Assert.assertTrue(result, result.contains("MANUAL"))
    Assert.assertFalse(result, result.contains("done."))
    Assert.assertEquals("", git(remote, "for-each-ref", "refs/tags/"))
    Assert.assertFalse(Files.exists(remote.resolve("push-options")))
  }

  @Test def rejectedPushShowsManualCommandsAndDoesNotPublishTag(): Unit = withRepo { (repo, remote) =>
    Files.writeString(remote.resolve("hooks/pre-receive"), "#!/bin/sh\necho 'test rejection' >&2\nexit 1\n")
    val result = publish(repo)
    Assert.assertTrue(result, result.contains("E: Push failed - try manual"))
    Assert.assertTrue(result, result.contains("MANUAL"))
    Assert.assertFalse(result, result.contains("done."))
    Assert.assertEquals("", git(remote, "for-each-ref", "refs/tags/"))
  }

  private case class PipelineRequest(method: String, path: String, contentType: String, fields: Map[String, String])

  private def withPipelineApi(status: Int = 201)(test: (Map[String, String], LinkedBlockingQueue[PipelineRequest]) => Unit): Unit = {
    val requests = new LinkedBlockingQueue[PipelineRequest]()
    val server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
    server.createContext(
      "/",
      exchange => {
        val body = new String(exchange.getRequestBody.readAllBytes(), UTF_8)
        val fields = body.split("&").map { pair =>
          val parts = pair.split("=", 2)
          URLDecoder.decode(parts(0), UTF_8) -> URLDecoder.decode(parts(1), UTF_8)
        }.toMap
        requests.add(PipelineRequest(exchange.getRequestMethod, exchange.getRequestURI.getRawPath,
            exchange.getRequestHeaders.getFirst("Content-Type"), fields))
        val response = "{\"message\":\"test-job-token\"}".getBytes(UTF_8)
        exchange.sendResponseHeaders(status, response.length)
        exchange.getResponseBody.write(response)
        exchange.close()
      }
    )
    server.start()
    val envs = Map("CI_JOB_TOKEN" -> "test-job-token",
      "CI_API_V4_URL" -> s"http://127.0.0.1:${server.getAddress.getPort}/api/v4/",
      "CI_PROJECT_ID" -> "42")
    try test(envs, requests)
    finally server.stop(0)
  }

  private def trigger(tag: String, envs: Map[String, String]): String = {
    val output = new ByteArrayOutputStream()
    val sys = new Term.Sys(new ByteArrayInputStream(Array.emptyByteArray), output, output)
    GitlabPipeline.triggerTag(sys, tag, envs)
    output.toString("UTF-8")
  }

  @Test def successfulTagPushTriggersPipelineForActualTag(): Unit = withPipelineApi() { (envs, requests) =>
    withRepo { (repo, remote) =>
      val result = publish(repo, envs = envs)
      Assert.assertTrue(result, result.contains("done."))
      Assert.assertTrue(result, result.contains("GitLab pipeline triggered for tag v1.2.3"))
      Assert.assertEquals(git(repo, "rev-parse", "v1.2.3"), git(remote, "rev-parse", "refs/tags/v1.2.3"))
      val request = requests.poll(1, TimeUnit.SECONDS)
      Assert.assertNotNull(request)
      Assert.assertEquals("POST", request.method)
      Assert.assertEquals("/api/v4/projects/42/trigger/pipeline", request.path)
      Assert.assertTrue(request.contentType, request.contentType.startsWith("application/x-www-form-urlencoded"))
      Assert.assertEquals(Map("token" -> "test-job-token", "ref" -> "v1.2.3"), request.fields)
      Assert.assertTrue(requests.isEmpty)
      Assert.assertFalse(result, result.contains("test-job-token"))
    }
  }

  @Test def formEncodingPreservesSpecialCharacters(): Unit = withPipelineApi() { (envs, requests) =>
    val result = trigger("v1.2.3+build/test", envs.updated("CI_JOB_TOKEN", "token+&=secret"))
    Assert.assertTrue(result, result.contains("pipeline triggered"))
    Assert.assertEquals(Map("token" -> "token+&=secret", "ref" -> "v1.2.3+build/test"),
      requests.poll(1, TimeUnit.SECONDS).fields)
    Assert.assertFalse(result, result.contains("token+&=secret"))
  }

  @Test def missingOrBlankVariablesSkipPipelineRequest(): Unit = withPipelineApi() { (envs, requests) =>
    envs.keys.foreach { key =>
      Assert.assertEquals("", trigger("v1.2.3", envs - key))
      Assert.assertEquals("", trigger("v1.2.3", envs.updated(key, "   ")))
    }
    Assert.assertTrue(requests.isEmpty)
  }

  @Test def declinedPushDoesNotTriggerPipeline(): Unit = withPipelineApi() { (envs, requests) =>
    withRepo { (repo, _) =>
      val result = publish(repo, answer = "n", envs = envs)
      Assert.assertTrue(result, result.contains("MANUAL"))
      Assert.assertTrue(requests.isEmpty)
    }
  }

  @Test def failedPushDoesNotTriggerPipeline(): Unit = withPipelineApi() { (envs, requests) =>
    withRepo { (repo, remote) =>
      Files.writeString(remote.resolve("hooks/pre-receive"), "#!/bin/sh\necho 'test rejection' >&2\nexit 1\n")
      val result = publish(repo, envs = envs)
      Assert.assertTrue(result, result.contains("E: Push failed"))
      Assert.assertTrue(requests.isEmpty)
    }
  }

  @Test def shopBranchPushDoesNotTriggerTagPipeline(): Unit = withPipelineApi() { (envs, requests) =>
    withRepo { (repo, _) =>
      val result = publish(repo, isShop = true, changedVersion = false, envs = envs)
      Assert.assertTrue(result, result.contains("done."))
      Assert.assertTrue(requests.isEmpty)
    }
  }

  @Test def rejectedPipelineRequestKeepsPublishedReleaseAndWarns(): Unit = withPipelineApi(403) { (envs, requests) =>
    withRepo { (repo, remote) =>
      val result = publish(repo, envs = envs)
      Assert.assertTrue(result, result.contains("HTTP 403"))
      Assert.assertTrue(result, result.contains("start it manually"))
      Assert.assertTrue(result, result.contains("done."))
      Assert.assertFalse(result, result.contains("MANUAL"))
      Assert.assertFalse(result, result.contains("test-job-token"))
      Assert.assertEquals(git(repo, "rev-parse", "v1.2.3"), git(remote, "rev-parse", "refs/tags/v1.2.3"))
      Assert.assertEquals(1, requests.size())
    }
  }

  @Test def serverFailureIsNotRetried(): Unit = withPipelineApi(503) { (envs, requests) =>
    val result = trigger("v1.2.3", envs)
    Assert.assertTrue(result, result.contains("HTTP 503"))
    Assert.assertEquals(1, requests.size())
    Assert.assertFalse(result, result.contains("test-job-token"))
  }

  @Test def invalidApiUrlWarnsWithoutLeakingCredentials(): Unit = {
    val result = trigger("v1.2.3",
      Map("CI_JOB_TOKEN" -> "secret-token",
        "CI_API_V4_URL" -> "invalid URL secret-token", "CI_PROJECT_ID" -> "42"))
    Assert.assertTrue(result, result.contains("triggering its GitLab pipeline failed"))
    Assert.assertFalse(result, result.contains("secret-token"))
  }
}
