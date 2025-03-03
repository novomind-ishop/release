package release

import scala.quoted.*

sealed trait OptsChange[T] {
  def replace(in: T): Opts
}
sealed class DepInvalidsFocus(io:Opts) extends OptsChange[Seq[String]] {

  override def replace(in: Seq[String]): Opts = {
    io.copy(depUpOpts = io.depUpOpts.copy(diag = io.depUpOpts.diag.copy(invalids = in)))
  }
}

sealed class InvalidsFocus(io: Opts) extends OptsChange[Seq[String]] {

  override def replace(in: Seq[String]): Opts = {
    io.copy(diag = io.diag.copy(invalids = in))
  }
}

sealed class ApiInvalidsFocus(io: Opts) extends OptsChange[Seq[String]] {

  override def replace(in: Seq[String]): Opts = {
    io.copy(apiDiff = io.apiDiff.copy(diag = io.apiDiff.diag.copy(invalids = in)))
  }
}

sealed class LintInvalidsFocus(io: Opts) extends OptsChange[Seq[String]] {

  override def replace(in: Seq[String]): Opts = {
    io.copy(lintOpts = io.lintOpts.copy(diag = io.lintOpts.diag.copy(invalids = in)))
  }
}

extension (c: Opts) {
  def focusDepInvalids(): DepInvalidsFocus = {
    new DepInvalidsFocus(c)
  }

  def focusInvalids(): InvalidsFocus = {
    new InvalidsFocus(c)
  }

  def focusApiInvalids(): ApiInvalidsFocus = {
    new ApiInvalidsFocus(c)
  }

  def focusLintInvalids(): LintInvalidsFocus = {
    new LintInvalidsFocus(c)
  }

}


