package release

import com.google.common.base.Strings
import com.google.common.hash.Hashing
import com.google.common.net.InetAddresses
import org.apache.commons.codec.language.Soundex
import org.apache.http.client.utils.URIBuilder

import java.io.{File, IOException}
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{FileSystemException, Files, Path}
import java.security.MessageDigest
import javax.xml.bind.DatatypeConverter
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.Random
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileSystemException, FileVisitResult, FileVisitor, Files, Path}
import java.time.{Duration, LocalDateTime, Period, ZonedDateTime}
import java.util
import scala.collection.immutable.Iterable
import scala.language.implicitConversions

object Util {
  def toPeriod(d: Duration): Period = {
    val aDate = ZonedDateTime.parse("2018-05-31T00:10:52+00:00").toLocalDate
    Period.between(aDate, aDate.plusDays(d.toDays))
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
    )

    def soundex(a: String, b: String) = {
      val s = Seq(a, b).sorted
      val t = (s.head, s.last)
      predefScores.get(t).getOrElse(Soundex.US_ENGLISH.difference(a, b))
    }

    def levenshtein(s1: String, s2: String): Int = {
      val memorizedCosts = mutable.Map[(Int, Int), Int]()
      import scala.collection.mutable
      import scala.collection.parallel.ParSeq
      def lev: ((Int, Int)) => Int = {
        case (k1, k2) =>
          memorizedCosts.getOrElseUpdate((k1, k2), (k1, k2) match {
            case (i, 0) => i
            case (0, j) => j
            case (i, j) =>
              ParSeq(1 + lev((i - 1, j)),
                1 + lev((i, j - 1)),
                lev((i - 1, j - 1))
                  + (if (s1(i - 1) != s2(j - 1)) 1 else 0)).min
          })
      }

      lev((s1.length, s2.length))
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

  class LinuxPath(in: Path) {
    def toStringLinux: String = {
      in.toString.replace('\\', '/')
    }

  }

  implicit def linuxPath(input: Path): LinuxPath = new LinuxPath(input)

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

  def write(f: File, content: String): File = {
    write(f, content.linesIterator.toSeq)
  }

  def write(f: File, content: Seq[String]): File = {
    val list: java.util.List[String] = content.asJava
    Files.write(f.toPath, list)
    f
  }

  def read(f: File): String = {
    readLines(f).mkString("\n") + "\n"
  }

  def readLines(f: File): Seq[String] = {
    if (Files.isRegularFile(f.toPath)) {
      Files.readAllLines(f.toPath, StandardCharsets.UTF_8).asScala.toList
    } else {
      throw new IllegalStateException(f.getAbsolutePath + " is no regular file")
    }
  }

  def ipFromUrl(in: String): Option[String] = {
    try {
      Some(java.net.InetAddress.getByName(URI.create(Strings.emptyToNull(in)).getHost).getHostAddress)
    } catch {
      case e: Exception => None
    }
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

      val result = new URIBuilder(work)
        .setUserInfo(null)
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

  def recursive(start: File): Seq[File] = {
    var s: mutable.Seq[Path] = mutable.Seq()
    val value: FileVisitor[Path] = new FileVisitor[Path] {
      override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = {
        FileVisitResult.CONTINUE
      }

      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        s = s.appended(file)
        FileVisitResult.TERMINATE
      }

      override def visitFileFailed(file: Path, exc: IOException): FileVisitResult = {
        FileVisitResult.TERMINATE
      }

      override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
        FileVisitResult.CONTINUE
      }
    }
    Files.walkFileTree(start.toPath, value)
    s.toSeq.map(_.toFile)

  }

  def deleteRecursive(file: File): Unit = {
    if (file.isDirectory) {
      if (file.listFiles() != null) {
        file.listFiles().foreach(deleteRecursive)
      }
      file.delete()
    } else {
      file.delete()
    }
  }

  def handleWindowsFilesystem(fn: Unit => Unit): Unit = {
    try {
      fn.apply(())
    } catch {
      case e@(_: FileSystemException | _: IOException) => Term.Os.getCurrent match {
        case Term.Os.Windows => throw new IllegalStateException("Windows tends to lock file handles." +
          " Try to find handle or DLL that locks the file. e.g. with Sysinternals Process Explorer", e)
        case _ => throw e;
      }
      case o: Exception => throw o
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

  def hashMurmur3_32_fixed(in: String): String = {
    Hashing.murmur3_32_fixed().hashString(in, StandardCharsets.UTF_8).toString
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
