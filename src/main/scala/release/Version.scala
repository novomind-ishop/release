package release

import com.google.common.base.Strings
import release.Version.{parseSloppy, removeTrailingSnapshots, undef}

import scala.annotation.tailrec

case class Version(pre: String, major: Int, minor: Int, patch: Int, low: String, rawInput: String) {

  private val rawInputNonNull = Strings.nullToEmpty(rawInput)
  lazy val primarys: (Int, Int, Int) = (major, minor, patch)
  lazy val primarysOpt: Option[(Int, Int, Int)] = Some(primarys)
  private val fallback = (-1, -1, -1)
  private lazy val primarysFallback = primarysOpt.getOrElse(fallback)
  lazy val isUndef: Boolean = {
    primarysFallback == fallback && this.copy(rawInput = "") == undef.copy(rawInput = "")
  }
  lazy val isOrdinal: Boolean = {
    val x = primarysFallback
    x._1 >= 0 && x._2 >= 0 && x._3 >= 0
  }
  lazy val isOrdinalOnly: Boolean = {
    isOrdinal && lowF.replaceFirst("_-SNAPSHOT", "") == "" && pre == ""
  }
  lazy val hasDigits:Boolean = {
    isOrdinal || rawInputNonNull.replaceAll("[^0-9]", "").toLongOption.isDefined
  }
  lazy val hasNoDigits:Boolean = !hasDigits
  lazy val isSnapshot: Boolean = rawInputNonNull.endsWith("-SNAPSHOT")
  lazy val textLowerCase: String = {
    if (isOrdinalOnly) {
      ""
    } else {
      rawInputNonNull.replaceFirst("-SNAPSHOT", "").toLowerCase.replaceAll("[^a-z]+", "")
    }

  }

  def isLowerEquals(in: Version): Boolean = {
    Seq(in, this).sorted.reverse.head == in
  }

  def isLowerEqualsOpt(in: Option[Version]): Boolean = {
    in.exists(isLowerEquals)
  }

  def isGreaterEquals(in: Version): Boolean = {
    Seq(in, this).sorted.head == in
  }

  def isGreaterEqualsOpt(in: Option[Version]): Boolean = {
    in.exists(isGreaterEquals)
  }

  def same(major: Int): Boolean = {
    this.major == major
  }

  def same(major: Int, minor: Int): Boolean = {
    same(major) && this.minor == minor
  }

  def same(major: Int, minor: Int, patch: Int): Boolean = {
    same(major, minor) && this.patch == patch
  }

  def same(v: Seq[Int]): Boolean = {
    v.size match {
      case 3 => same(v(0), v(1), v(2))
      case 2 => same(v(0), v(1))
      case 1 => same(v(0))
      case _ => false
    }

  }

  private val lowF: String = if (Util.isNullOrEmpty(low)) {
    ""
  } else {
    "_" + low
  }

  val lowOrdinalPart: Int = {
    Strings.nullToEmpty(low).replaceAll("[^0-9]+", "").toIntOption.getOrElse(Int.MaxValue)
  }

  private val patchF: String = if (patch == 0) {
    ""
  } else {
    "." + patch
  }

  @tailrec
  final def nextVersionResetZero(t3: (Int, Int, Int), limit: Int = 1): Version = {
    if (limit > 0 && t3._1 != 0 && t3._2 == 0 && t3._3 == 0) {
      nextVersionResetZero(t3.copy(_2 = minor * -1, _3 = patch * -1), limit - 1)
    } else if (limit > 0 && t3._2 != 0 && t3._3 == 0) {
      nextVersionResetZero(t3.copy(_3 = patch * -1), limit - 1)
    } else {
      nextVersion(t3)
    }
  }

  def nextVersion(t3: (Int, Int, Int)): Version = {
    copy(major = major + t3._1, minor = minor + t3._2, patch = patch + t3._3)
  }

  @tailrec
  final def nextIfKnown(known: Seq[Version], ref: Version = this): Version = {
    if (known.map(_.copy(rawInput = "")).contains(ref.copy(rawInput = ""))) {
      val version = ref.copy(patch = ref.patch + 1, low = "")
      nextIfKnown(known, version.copy(rawInput = version.format()))
    } else {
      ref
    }
  }

  def removeAllSnapshots(): Version = {
    if (isSnapshot) {
      parseSloppy(removeTrailingSnapshots(rawInput))
    } else {
      this
    }
  }

  def plusWeek(): Version = {
    val addWeek = minor.toInt + 1
    val nextWeek = if (addWeek > 52) {
      1
    } else {
      addWeek
    }
    val nextYear = if (addWeek > 52) {
      major.toInt + 1
    } else {
      major
    }
    copy(minor = nextWeek, major = nextYear)
  }

  def formatShop(): String = {
    pre.toUpperCase() + major + "." + "%02d".format(minor) + patchF + lowF
  }

  def format(): String = {
    pre + major + "." + minor + "." + patch + lowF
  }

  def formatAsSnapshot(): String = {
    pre + major + "." + minor + "." + patch + lowF + "-SNAPSHOT"
  }

  def formatShopAsSnapshot(): String = {
    formatShop() + "-SNAPSHOT"
  }

  def isMajor(): Boolean = {
    minor == 0 && patch == 0
  }
}

object Version {

  private[release] val semverPattern = "^([0-9]+)\\.([0-9]+)\\.([0-9]+)$".r
  private[release] val semverPatternNoBugfix = "^([0-9]+)\\.([0-9]+)$".r
  private[release] val semverPatternNoMinor = "^([0-9]+)$".r
  private[release] val semverPatternLowdash = "^([0-9]+)\\.([0-9]+)\\.([0-9]+)_([0-9]+)$".r
  private[release] val semverPatternLowdashString = "^([0-9]+)\\.([0-9]+)\\.([0-9]+)_(.+)$".r
  private[release] val semverPatternRCEnd = "^([0-9]+)\\.([0-9]+)\\.([0-9]+)-((?:RC|M)[1-9][0-9]*)$".r
  private[release] val semverGitTagForDockerTagPattern = "^v[0-9]+\\.[0-9]+\\.[0-9]+(?:-(?:RC|M)[1-9][0-9]*)?$".r
  private[release] val semverPatternLetterEnd = "^([0-9]+)\\.([0-9]+)\\.([0-9]+)-([0-9a-zA-Z]+)$".r
  private[release] val stableShop = "^([0-9]+x)-stable.*$".r
  private[release] val shopPattern = "^(RC-)([1-9][0-9]{3})\\.([0-9][0-9])?(?:\\.([1-9]+[0-9]*))?(?:_([1-9]+[0-9]*))?(?:-SNAPSHOT)?$".r
  private[release] val shopLikePattern = "^([rcRC]{2}.)([0-9][0-9]{3}).([0-9][0-9])?(?:.([0-9]+[0-9]*))?(?:.([0-9]+[0-9]*))?(?:.[SsNnAaPpHhOoTt]+)?$".r
  private[release] val betaTagPattern = "^(BETA-)(.+)$".r
  val shopBranchPattern = ("^release/" + shopPattern.regex.substring(1)).r
  private[release] val shopPatternSloppy = "^([Rr][Cc][-\\._])([0-9]{4})[_\\.-]([0-9][0-9]?)?(?:[_\\.-]([0-9]+[0-9]*))?(?:[-_\\.]([0-9]+[0-9]*))?$".r
  private[release] val number = "^([0-9]+)(.*)".r
  private[release] val number2 = "^([0-9]+)\\.([0-9]+)(.*)".r
  private[release] val number3 = "^([0-9]+)\\.([0-9]+)\\.([0-9]+)(.*)".r

  private def ordering1 = Ordering.by[Version, (Int, Int, Int, Int)](e => (e.major, e.minor, e.patch, e.lowOrdinalPart))

  private def ordering2: Ordering[Version] = Ordering.by[Version, (String, String)](e => (e.low, e.pre)).reverse

  private def ordering3: Ordering[Version] = ordering1.orElse(ordering2)

  implicit def ordering: Ordering[Version] = ordering3

  val undef: Version = Version("n/a", -1, -1, -1, "", "")

  private def nullToZero(in: String) = if (in == null || in == "") {
    0
  } else if (in.forall(_.isDigit)) {
    in.toInt
  } else {
    -1
  }

  val snapshot = "-SNAPSHOT"

  def applySnapshot(in: String): String = in + snapshot

  @tailrec
  def removeTrailingSnapshots(str: String): String = {
    val out = Strings.nullToEmpty(str).replaceFirst("-SNAPSHOT$", "").trim
    if (out.endsWith("-SNAPSHOT")) {
      removeTrailingSnapshots(out)
    } else {
      out
    }
  }

  def fromStringOpt(pre: String, major: String, minor: String, patch: String, low: String, original: String): Option[Version] = {
    Some(fromString(pre, major, minor, patch, low, original))
  }

  def fromString(pre: String, major: String, minor: String, patch: String, low: String, original: String): Version = {
    Version(pre, nullToZero(major), nullToZero(minor), nullToZero(patch), Util.nullToEmpty(low), original)
  }

  def toVersion(m: String): Option[Version] = m match {
    case semverPattern(ma, mi, b) => Version.fromStringOpt("", ma, mi, b, "", m)
    case semverPatternLowdash(ma, mi, b, low) => Version.fromStringOpt("", ma, mi, b, low, m)
    case _ => None
  }


  def parseShopLike(versionText: String): Option[Version] = {
    val awfr = try {
      versionText match {
        case shopLikePattern(pre, year, week, minor, low) => Some(Version.fromString(pre, year, week, minor, low, versionText))
        case any => None
      }
    } catch {
      case e: Exception => e.printStackTrace(); None
    }
    awfr.filterNot(e => parseSloppy(e.rawInput).isOrdinal)
  }

  def parseSloppy(versionText: String): Version = {

    try {
      versionText match {
        case semverPatternLowdash(major, minor, patch, low) => Version.fromString("", major, minor, patch, low, versionText)
        case semverPatternLowdashString(major, minor, patch, low) => Version.fromString("", major, minor, patch, low, versionText)
        case semverPattern(major, minor, patch) => Version.fromString("", major, minor, patch, "", versionText)
        case semverPatternNoBugfix(major, minor) => Version.fromString("", major, minor, "", "", versionText)
        case semverPatternNoMinor(major) => Version.fromString("", major, "", "", "", versionText)
        case shopPattern(pre, year, week, minor, low) => Version.fromString(pre, year, week, minor, low, versionText)
        case number3(major, minor, patch, low) => Version.fromString("", major, minor, patch, low, versionText)
        case number2(major, minor, low) => Version.fromString("", major, minor, "", low, versionText)
        case number(major, low) => Version.fromString("", major, "", "", low, versionText)

        case any => undef.copy(rawInput = any)
      }
    } catch {
      case e: Exception => e.printStackTrace(); undef.copy(rawInput = versionText)
    }
  }

  def parse(versionText: String): Version = {
    val snapped = removeTrailingSnapshots(versionText)

    try {
      snapped match {
        case stableShop(pre) => undef
        case semverPatternLowdash(ma, mi, b, low) => Version.fromString("", ma, mi, b, low, "")
        case semverPatternLowdashString(ma, mi, b, low) => Version.fromString("", ma, mi, b, low, "")
        case semverPattern(ma, mi, b) => Version.fromString("", ma, mi, b, "", "")
        case semverPatternNoBugfix(ma, mi) => Version.fromString("", ma, mi, "", "", "")
        case semverPatternNoMinor(ma) => Version.fromString("", ma, "", "", "", "")
        case shopPattern(pre, year, week, minor, low) => Version.fromString(pre, year, week, minor, low, "")
        case any => undef
      }
    } catch {
      case e: Exception => e.printStackTrace(); undef
    }
  }
}
