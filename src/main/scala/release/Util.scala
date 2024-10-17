package release

import com.google.common.base.{Stopwatch, Strings}
import com.google.common.hash.{HashFunction, Hashing}
import org.apache.commons.codec.language.Soundex

import java.io.File
import java.net.{URI, URLEncoder}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.security.MessageDigest
import java.time.{Duration, Period, ZonedDateTime}
import java.util
import java.util.concurrent.TimeUnit
import javax.xml.bind.DatatypeConverter
import scala.collection.immutable.Iterable
import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, Future}
import scala.jdk.CollectionConverters._
import scala.math.BigDecimal.RoundingMode
import scala.util.Random

object Util {
  def byteToMb(value: Long): Long = {
    value / (1024 * 1024)
  }

  def toPeriod(d: Duration): Period = {
    val aDate = ZonedDateTime.parse("2018-05-31T00:10:52+00:00").toLocalDate
    Period.between(aDate, aDate.plusDays(d.toDays))
  }

  def roundDuration(d: Duration): Duration = {
    val fractionalSeconds = BigDecimal(d.getNano, 9)
    val totalSeconds = BigDecimal(d.getSeconds) + fractionalSeconds

    val roundedSeconds = totalSeconds.setScale(2, RoundingMode.HALF_UP)
    val roundedDuration = Duration.ofSeconds(roundedSeconds.longValue,
      (roundedSeconds.remainder(BigDecimal(1)) * BigDecimal(1000000000)).intValue)

    roundedDuration
  }

  object Similarity {
    def spl(a: String): Seq[String] = a.toLowerCase.split("[^a-z]").toSeq

    def soundexSplitMax(a: String, b: String) = {
      soundexMax(spl(a), spl(b))
    }

    def soundexSplitMin(a: String, b: String) = {
      soundexMin(spl(a), spl(b))
    }

    def soundexMin(a: Seq[String], b: Seq[String]) = {
      soundexRange(a, b).min
    }

    def soundexMax(a: Seq[String], b: Seq[String]) = {
      soundexRange(a, b).max
    }

    def soundexRange(a: Seq[String], b: Seq[String]): Seq[Int] = {
      if (a.size != b.size) {
        Seq(0)
      } else {
        a.zip(b).map(t => soundex(t._1, t._2))
      }
    }

    private val predefScores: Map[(String, String), Int] = Map(
      ("bre", "core") -> 2,
      ("ui", "vue") -> 2,
      ("api", "web") -> 2,
      ("app", "sba") -> 2,
    )

    def soundex(a: String, b: String) = {
      val s = Seq(a, b).sorted
      val t = (s.head, s.last)
      predefScores.get(t).getOrElse(Soundex.US_ENGLISH.difference(a, b))
    }

    def levenshtein(s1: String, s2: String): Int = {
      val lenStr1 = s1.length
      val lenStr2 = s2.length

      if (lenStr1 == 0) return lenStr2
      if (lenStr2 == 0) return lenStr1

      if (lenStr1 < lenStr2) return levenshtein(s2, s1)

      var previousRow = new Array[Int](lenStr2 + 1)
      var currentRow = new Array[Int](lenStr2 + 1)

      for (j <- 0 to lenStr2) previousRow(j) = j

      for (i <- 1 to lenStr1) {
        currentRow(0) = i
        for (j <- 1 to lenStr2) {
          val substitutionCost = if (s1(i - 1) == s2(j - 1)) 0 else 1
          currentRow(j) = math.min(math.min(previousRow(j) + 1, currentRow(j - 1) + 1), previousRow(j - 1) + substitutionCost)
        }
        val temp = previousRow
        previousRow = currentRow
        currentRow = temp
      }

      previousRow(lenStr2)
    }

  }

  class PluralString(in: String) {
    def pluralize(int: Int): String = {
      if (int > 1) {
        in + "s"
      } else {
        in
      }
    }

    /**
      * @see jdk11 has isBlank()
      */
    def blank(): Boolean = {
      in == null || in != null && in.trim.isEmpty
    }
  }

  implicit def pluralize(input: String): PluralString = new PluralString(input)

  def show(product: Product): String = {
    val className = product.productPrefix
    val fieldNames = product.productElementNames.toList
    val fieldValues = product.productIterator.toList
    val fields = fieldNames.zip(fieldValues).map {
      case (name, value) if value.isInstanceOf[Iterable[Any]] => {
        try {
          val value1 = value.asInstanceOf[Iterable[Product]]
          s"$name = ${value1.map(show)}"
        } catch {
          case _: Exception => {
            val value1 = value.asInstanceOf[Iterable[Any]]
            if (value1.nonEmpty && value1.head.isInstanceOf[String]) {
              s"$name = Seq(${value1.map(e => s"\"${e}\"").mkString(", ")})"
            } else {
              s"$name = Seq(${value1.mkString(", ")})"
            }

          }

        }
      }
      case (name, value: Product) => s"$name = ${show(value)}"
      case (name, value: String) => s"$name = \"${value}\""
      case (name, value) => s"$name = $value"
    }

    fields.mkString(s"$className(", ", ", ")")
  }

  def emptyToNone(in: String): Option[String] = in match {
    case "" => None
    case any if any.trim == "" => None
    case any => Some(any)
  }

  def groupedFiltered[R](in: Seq[R]): Map[R, Seq[R]] = {
    in.map(a => (a, in.filterNot(_ == a))).toMap
  }

  def symmetricDiff[R](left: Seq[R], right: Seq[R]): Seq[R] = {
    (left diff right) concat (right diff left)
  }

  def onlyOpt[T](ts: Seq[T]): Option[T] = ts match {
    case Nil => None
    case e if e.size == 1 => Some(e.head)
    case e => throw new IllegalArgumentException("any: " + e)
  }

  def only[T](ts: Seq[T], msgSupplier: => String): T = ts match {
    case Nil => throw new IllegalArgumentException(msgSupplier + ". Input is Nil.")
    case e if e.size == 1 => e.head
    case e => throw new IllegalArgumentException(msgSupplier + " (" + e.toList.mkString(", ") + ")")
  }

  lazy val localWork: File = new File(".").getAbsoluteFile match {
    case f: File if f.getParentFile.getName == "modules" => f.getParentFile.getParentFile.getParentFile
    case f: File => f
  }

  def timeout[T](k: Int, timeUnit: TimeUnit, fn: (Stopwatch) => T, fallbackFn: (Exception, Duration) => T): (T, Duration) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val stopwatch = Stopwatch.createStarted()
    val futureTask = Future {
      fn.apply(stopwatch)
    }
    val timeoutDuration: FiniteDuration = FiniteDuration(k, timeUnit)

    try {
      (Await.result(futureTask, timeoutDuration), stopwatch.elapsed())
    } catch {
      case e: Exception => {
        val last = stopwatch.elapsed()
        (fallbackFn.apply(e, last), last)
      }
    }

  }

  def distinctOn[A, K](in: Seq[A], fn: A => K): Seq[A] = {
    val b = Seq.newBuilder[A]
    val seen = mutable.HashSet[K]()
    for (x <- in) {
      val k = fn.apply(x)
      if (!seen(k)) {
        b += x
        seen += k
      }
    }
    b.result()
  }

  def ipFromUrl(in: String): Option[String] = {
    try {
      val host = URI.create(Strings.emptyToNull(in)).getHost
      if (host == null) {
        None
      } else {
        val address = java.net.InetAddress.getByName(host).getHostAddress
        Some(address)
      }
    } catch {
      case e: Exception => None
    }
  }

  case class UrlUserInfo(url: String, username: Option[String], passwd: Option[String])

  def extractedUserInfoUrl(url: String): UrlUserInfo = {
    val t: (Option[String], Option[String]) = try {
      val ui = createUri(url).getUserInfo
      val parts = ui.split(":").toSeq
      (parts.headOption, parts.drop(1).headOption)
    } catch {
      case _: Exception => {
        (None, None)
      }
    }
    UrlUserInfo(stripUserinfo(url), t._1, t._2)
  }

  def urlEncode(in: String): String = {
    URLEncoder.encode(in, "UTF-8")
  }

  def createUri(in: String): URI = {
    if (in.count(_ == '@') > 1) {
      val ats = in.split('@').toSeq
      val userInfoWithProto = ats.dropRight(1).mkString("@")
      val last = in.replace(userInfoWithProto, "")
      val splitOn = userInfoWithProto.split("://").toSeq
      val proto = splitOn.head
      val uinfo = urlEncode(splitOn.drop(1).mkString)
      URI.create(proto + "://" + uinfo + last)
    } else {
      URI.create(in)
    }
  }

  def removeUserinfo(uriWithUserInfo: URI): URI = {
    setUserinfo(uriWithUserInfo, null)
  }

  def setUserinfo(uri: URI, userinfo: String): URI = {
    val result = new URI(
      uri.getScheme(),
      userinfo,
      uri.getHost(),
      uri.getPort(),
      uri.getPath(),
      uri.getQuery(),
      uri.getFragment()
    )
    result
  }

  def stripUserinfo(url: String): String = {
    val nullSafe = Strings.nullToEmpty(url)
    try {
      val missingProto = nullSafe.contains("://") && nullSafe.nonEmpty
      val work = if (missingProto) {
        nullSafe
      } else {
        "git://" + nullSafe.replaceFirst(":", "/")
      }
      val result = removeUserinfo(createUri(work))
        .toString
      if (missingProto) {
        result
      } else {
        result.replaceFirst("^git://", "").replaceFirst("/", ":")
      }
    } catch {
      case e: Exception => nullSafe
    }
  }

  def systemEnvs(): Map[String, String] = toScalaMapNonBlank(System.getenv())

  def toScalaMapNonBlank[K, V](in: util.Map[K, V]): Map[K, V] = {
    toScalaMapNonNull(in).filterNot(_._1.toString.isBlank).filterNot(_._2.toString.isBlank)
  }

  def toScalaMapNonNull[K, V](in: util.Map[K, V]): Map[K, V] = {
    in.asScala.filterNot(_._1 == null).filterNot(_._2 == null).toMap
  }

  def toJavaList[V](map: Seq[V]): java.util.List[V] = map.asJava

  def hashMd5(in: String): String = {
    hashBy(MessageDigest.getInstance("MD5"), in)
  }

  def hashMd5Random(): String = Util.hashMd5(Random.nextLong().toString)

  private def hashMurmur3_32_fixed(): HashFunction = Hashing.murmur3_32_fixed()

  def hashMurmur3_32_fixed(in: String): String = {
    hashMurmur3_32_fixed().hashString(in, StandardCharsets.UTF_8).toString
  }

  def hashMurmur3_32_fixed(in: java.nio.file.Path): String = {
    hashMurmur3_32_fixed().hashBytes(Files.readAllBytes(in)).toString
  }

  def hashSha1(in: String): String = {
    hashBy(MessageDigest.getInstance("SHA1"), in)
  }

  private def hashBy(md: MessageDigest, in: String): String = {
    md.update(in.getBytes(StandardCharsets.UTF_8))
    DatatypeConverter.printHexBinary(md.digest()).toLowerCase
  }

  def isNullOrEmpty(in: String): Boolean = {
    in == null || in == ""
  }

  def nullToEmpty(in: String): String = if (in == null) {
    ""
  } else {
    in
  }
}
