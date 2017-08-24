package release

import java.io.File

import scala.collection.mutable

object Util {

  def emptyToNone(in:String):Option[String] = in match {
    case "" ⇒ None
    case any if any.trim == "" ⇒ None
    case any ⇒ Some(any)
  }

  def symmetricDiff[R](left: Seq[R], right: Seq[R]): Seq[R] = {
    val s1 = left
    val s2 = right
    (s1 diff s2) union (s2 diff s1)
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
