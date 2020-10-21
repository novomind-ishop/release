package release

import org.junit.{Assert, Test}
import org.scalatestplus.junit.AssertionsForJUnit

class ProjectModTest extends AssertionsForJUnit {

  @Test
  def testScalaDeps(): Unit = {

  }

  @Test
  def testGroupSortReleases(): Unit = {

    val result = ProjectMod.groupSortReleases(Seq(
      "a",
      "1.1", "1-alpha",
      "2", "2.0",
      "10.0.0",
    ))

    Assert.assertEquals(Seq(
      ("10", Seq("10.0.0")),
      ("2", Seq("2", "2.0")),
      ("1", Seq("1-alpha", "1.1")),
      ("-1", Seq("a")),
    ), result)
  }

}
