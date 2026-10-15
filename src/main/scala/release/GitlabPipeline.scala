package release

import org.apache.http.client.config.RequestConfig
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.client.methods.HttpPost
import org.apache.http.impl.client.HttpClients
import org.apache.http.message.BasicNameValuePair

import java.net.URLEncoder
import java.nio.charset.StandardCharsets.UTF_8
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.control.NonFatal

private[release] object GitlabPipeline {
  def triggerTag(sys: Term.Sys, tag: String, envs: Map[String, String]): Unit = {
    val settings = for {
      token <- envs.get("CI_JOB_TOKEN").filterNot(_.isBlank)
      apiUrl <- envs.get("CI_API_V4_URL").filterNot(_.isBlank)
      projectId <- envs.get("CI_PROJECT_ID").filterNot(_.isBlank)
    } yield (token, apiUrl, projectId)

    settings.foreach { case (token, apiUrl, projectId) =>
      try {
        val config = RequestConfig.custom()
          .setConnectTimeout(10000)
          .setConnectionRequestTimeout(10000)
          .setSocketTimeout(15000)
          .build()
        val request = new HttpPost(
          s"${apiUrl.stripSuffix("/")}/projects/${URLEncoder.encode(projectId, UTF_8)}/trigger/pipeline")
        request.setEntity(new UrlEncodedFormEntity(Seq(
              new BasicNameValuePair("token", token),
              new BasicNameValuePair("ref", tag)
            ).asJava, UTF_8))
        Using.resource(HttpClients.custom().setDefaultRequestConfig(config)
            .disableAutomaticRetries().disableRedirectHandling().build()) { client =>
          Using.resource(client.execute(request)) { response =>
            val status = response.getStatusLine.getStatusCode
            if (status >= 200 && status < 300) {
              sys.out.println(s"I: GitLab pipeline triggered for tag $tag.")
            } else {
              sys.err.println(s"W: Tag $tag was pushed, but triggering its GitLab pipeline failed (HTTP $status); start it manually.")
            }
          }
        }
      } catch {
        case NonFatal(e) =>
          // Do not log request details, response bodies, or credentials.
          sys.err.println(
            s"W: Tag $tag was pushed, but triggering its GitLab pipeline failed (${e.getClass.getSimpleName}); start it manually.")
      }
    }
  }
}
