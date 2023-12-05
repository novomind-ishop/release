package release

import japicmp.cmp.{JarArchiveComparator, JarArchiveComparatorOptions}
import japicmp.config.Options
import japicmp.model.JApiClassType.ClassType
import japicmp.model.{AccessModifier, JApiChangeStatus, JApiClass, JApiClassType}
import javassist.CtClass
import org.junit.{Assert, Test}
import org.scalatestplus.junit.AssertionsForJUnit

import java.util
import java.util.Optional
import scala.jdk.CollectionConverters._

class XOutputGeneratorTest extends AssertionsForJUnit {

  @Test
  def test_empty(): Unit = {

    val in: Seq[JApiClass] = Seq()

    val testee = new XoutOutputGenerator(options = Options.newDefault(), jApiClasses = in.asJava)

    Assert.assertEquals(
      """Comparing source compatibility of  against
        |No changes.""".stripMargin, testee.generate)
  }

  @Test
  def test_class(): Unit = {

    val fullQName = "org.example.MyClass"
    val oldClass = japicmp.util.Optional.absent[CtClass]()
    val newClass = japicmp.util.Optional.absent[CtClass]()
    val changeStatus = JApiChangeStatus.REMOVED
    val classType = new JApiClassType(japicmp.util.Optional.of[ClassType](ClassType.CLASS),
      japicmp.util.Optional.of[ClassType](ClassType.CLASS), JApiChangeStatus.NEW)
    val options = Options.newDefault()
    options.setIgnoreMissingClasses(false)
    options.setOutputOnlyModifications(false)
    options.setOutputOnlyBinaryIncompatibleModifications(false)
    options.setAccessModifier(AccessModifier.PRIVATE)
    val cmp = new JarArchiveComparator(JarArchiveComparatorOptions.of(options))
    val clazz = new JApiClass(cmp, fullQName, oldClass, newClass, changeStatus, classType)
    val list = new util.ArrayList[JApiClass]()
    list.add(clazz)

    val testee = new XoutOutputGenerator(options = options, jApiClasses = list)

    Assert.assertEquals( // TODO later - here should be a detailed diff
      """Comparing source compatibility of  against
        |No changes.""".stripMargin, testee.generate)
  }

}
