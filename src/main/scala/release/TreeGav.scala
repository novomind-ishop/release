package release

import release.Lint.lineMax
import release.Starter.Opts
import release.Term.warnSoft

import java.io.{File, PrintStream}

object TreeGav {

  def format(value: Map[File, Seq[ProjectMod.Gav3]], out: PrintStream, opts: Opts): Unit = {
    val asf = {
      val k = value.values.flatten.groupBy(_.toGav2())
      k.map(e => (e._1, e._2.toSeq.distinct)).toSeq.sortBy(_._2.size)

    }
    asf
      .filter(_._2.size > 1)
      .foreach(e => out.println(warnSoft(s" ${e._1.formatted} {${e._2.map(_.version.getOrElse("NA")).mkString(", ")}}", opts, limit = lineMax)))
  }

}
