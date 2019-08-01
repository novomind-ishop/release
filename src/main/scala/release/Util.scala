package release

import java.io.{File, IOException}
import java.nio.charset.StandardCharsets
import java.nio.file.{FileSystemException, Files}
import java.security.MessageDigest

import javax.xml.bind.DatatypeConverter

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.Random

object Util {

  class PluralString(in: String) {
    def pluralize(int: Int): String = {
      if (int > 0) {
        in + "s"
      } else {
        in
      }
    }

    /**
      * @see jdk11 has isBlank()
      */
    def blank(): Boolean = {
      in != null && in.trim.isEmpty
    }
  }

  implicit def pluralize(input: String) = new PluralString(input)

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

  def write(f: File, content: Seq[String]): Unit = {
    val list: java.util.List[String] = content.asJava
    Files.write(f.toPath, list)
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
      case e@(_: FileSystemException | _: IOException) => Sgit.getOs match {
        case Sgit.Os.Windows => throw new IllegalStateException("Windows tends to lock file handles." +
          " Try to find handle or DLL that locks the file. e.g. with Sysinternals Process Explorer", e)
        case _ => throw e;
      }
      case o: Exception => throw o
    }
  }

  def toSet[A](set: java.util.Set[A]): Set[A] = set.asScala.toSet

  def toSeq[A](set: java.util.Collection[A]): Seq[A] = set.asScala.toList

  def toJavaMap[K, V](map: Map[K, V]): java.util.Map[K, V] = map.asJava

  def toJavaList[V](map: Seq[V]): java.util.List[V] = map.asJava

  def hashMd5(in: String): String = {
    val md: MessageDigest = MessageDigest.getInstance("MD5")
    md.update(in.getBytes(StandardCharsets.UTF_8))
    DatatypeConverter.printHexBinary(md.digest()).toLowerCase()
  }

  def hashMd5Random(): String = Util.hashMd5(Random.nextLong().toString)

  def hashSha1(in: String): String = {
    val md: MessageDigest = MessageDigest.getInstance("SHA1")
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
