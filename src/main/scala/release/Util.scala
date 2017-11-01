package release

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import scala.collection.{JavaConverters, mutable}

object Util {

  def emptyToNone(in:String):Option[String] = in match {
    case "" ⇒ None
    case any if any.trim == "" ⇒ None
    case any ⇒ Some(any)
  }

  def groupedFiltered[R](in:Seq[R]):Map[R, Seq[R]] = {
    in.map(a ⇒ (a, in.filterNot(_ == a))).toMap
  }

  def symmetricDiff[R](left: Seq[R], right: Seq[R]): Seq[R] = {
    (left diff right) union (right diff left)
  }

  def onlyOpt[T](ts: Seq[T]): Option[T] = ts match {
    case Nil ⇒ None
    case e if e.size == 1 ⇒ Some(e.head)
    case e ⇒ throw new IllegalArgumentException("any: " + e)
  }

  def only[T](ts: Seq[T], msg: String) = ts match {
    case Nil ⇒ throw new IllegalArgumentException(msg + ". Input is Nil.")
    case e if e.size == 1 ⇒ e.head
    case e ⇒ throw new IllegalArgumentException(msg + " (" + e.toList.mkString(", ") + ")")
  }

  lazy val localWork: File = new File(".").getAbsoluteFile match {
    case f: File if f.getParentFile.getName == "modules" ⇒ f.getParentFile.getParentFile.getParentFile
    case f: File ⇒ f
  }

  def distinctOn[A, K](in: Seq[A], fn: A ⇒ K): Seq[A] = {
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
    Files.write(f.toPath, JavaConverters.bufferAsJavaList(content.toBuffer))
  }

  def read(f:File): String = {
    readLines(f).mkString("\n") + "\n"
  }

  def readLines(f:File):Seq[String] = {
    JavaConverters.asScalaBuffer(Files.readAllLines(f.toPath, StandardCharsets.UTF_8))
  }

  def delete(file: File): Unit = {
    def deleteFile(allFiles: Array[File]): Unit = {
      if (allFiles != null) {
        allFiles.foreach(delete)
      }
    }

    if (file.isDirectory) {
      deleteFile(file.listFiles())
      file.delete()
    } else {
      file.delete()
    }
  }

}
