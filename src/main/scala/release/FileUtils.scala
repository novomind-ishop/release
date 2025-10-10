package release

import com.google.common.collect.ImmutableSet

import java.io.{File, IOException, UncheckedIOException}
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileSystemException, FileVisitOption, FileVisitResult, FileVisitor, Files, Path}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Using
import scala.jdk.StreamConverters._
import scala.jdk.CollectionConverters._

object FileUtils {
  object Ext {
    extension (in:Path) {
      def toStringLinux: String = {
        in.toString.replace('\\', '/')
      }
    }
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

  def findAllInFile[T](filePath: Path, selector: String => (Boolean, T)): Seq[(T, Int)] = {
    Using.resource(Files.lines(filePath))(lines => {
      val result: LazyList[(T, Int)] = lines.toScala(LazyList).zipWithIndex.flatMap(lineIdx => {
        val tuple = selector.apply(lineIdx._1)
        if (tuple._1) {
          Some((tuple._2, lineIdx._2 + 1))
        } else {
          None
        }
      })
      try {
        result.toList
      } catch {
        case _: UncheckedIOException => {
          Nil
        }
      }
    })
  }

  def walk(rootFile: File): ListBuffer[Path] = {
    val na = ListBuffer.empty[Path]
    val skipDirNames = Set(".git", "target")
    Files.walkFileTree(rootFile.toPath, ImmutableSet.of(FileVisitOption.FOLLOW_LINKS), Int.MaxValue, new FileVisitor[Path] {
      override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = {
        if (skipDirNames.contains(dir.getFileName.toString)) {
          FileVisitResult.SKIP_SUBTREE
        } else {
          FileVisitResult.CONTINUE
        }
      }

      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        na += file
        FileVisitResult.CONTINUE

      }

      override def visitFileFailed(file: Path, exc: IOException): FileVisitResult = {
        FileVisitResult.CONTINUE
      }

      override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
        FileVisitResult.CONTINUE
      }
    })
    na
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


}
