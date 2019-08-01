package release

import org.junit.Test
import org.scalatestplus.junit.AssertionsForJUnit

class TestHelperTest extends AssertionsForJUnit {

  @Test
  def testAssertException(): Unit = {

    TestHelper.assertException("asdf", classOf[IllegalStateException],
      () => throw new IllegalStateException("asdf"))

  }

  @Test
  def testAssertException_invalid_message(): Unit = {
    TestHelper.assertComparisonFailure("expected:<[asdf]> but was:<[mööp]>",
      () => {
        TestHelper.assertException("asdf", classOf[IllegalStateException],
          () => {
            throw new IllegalStateException("mööp")
          })
      })
  }

  @Test
  def testAssertException_invalid_exception(): Unit = {
    TestHelper.assertAssertionError("expected:<class java.lang.IllegalArgumentException> " +
      "but was:<class java.lang.IllegalStateException>", classOf[AssertionError],
      () => {
        TestHelper.assertException("mööp", classOf[IllegalArgumentException],
          () => {
            throw new IllegalStateException("mööp")
          })
      })
  }

}
