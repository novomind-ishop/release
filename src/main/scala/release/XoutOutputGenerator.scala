package release

import java.util
import java.util.List

import japicmp.config._
import japicmp.model.JApiAnnotationElementValue.Type
import japicmp.model._
import japicmp.output._
import japicmp.util.Optional
import javassist.bytecode.annotation.MemberValue

import scala.jdk.CollectionConverters._

class XoutOutputGenerator(options: Options, jApiClasses: util.List[JApiClass]) extends
  OutputGenerator[String](options, jApiClasses) {

  override def generate: String = {
    val outputFilter: OutputFilter = new OutputFilter(options)
    outputFilter.filter(jApiClasses)
    val sb = new StringBuilder()
    sb.append(options.getDifferenceDescription.trim).append('\n')
    if (jApiClasses.size > 0) {
      for (jApiClass <- jApiClasses.asScala) {
        processClass(sb, jApiClass)
        processConstructors(sb, jApiClass)
        processMethods(sb, jApiClass)
        processAnnotations(sb, jApiClass, 1)
      }
    }
    else {
      sb.append("No changes.")
    }
    sb.toString
  }

  private def processAnnotations(sb: StringBuilder, jApiClass: JApiHasAnnotations, numberofTabs: Int): Unit = {
    val annotations: List[JApiAnnotation] = jApiClass.getAnnotations
    for (jApiAnnotation <- annotations.asScala) {
      appendAnnotation(sb, signs(jApiAnnotation), jApiAnnotation, numberofTabs)
      val elements: List[JApiAnnotationElement] = jApiAnnotation.getElements
      for (jApiAnnotationElement <- elements.asScala) {
        appendAnnotationElement(sb, signs(jApiAnnotationElement), jApiAnnotationElement, numberofTabs + 1)
      }
    }
  }

  private def processConstructors(sb: StringBuilder, jApiClass: JApiClass): Unit = {
    val constructors: List[JApiConstructor] = jApiClass.getConstructors
    for (jApiConstructor <- constructors.asScala) {
      appendMethod(sb, signs(jApiConstructor), jApiConstructor, "CONSTRUCTOR:")
      processAnnotations(sb, jApiConstructor, 2)
      processExceptions(sb, jApiConstructor, 2)
    }
  }

  private def processMethods(sb: StringBuilder, jApiClass: JApiClass): Unit = {
    val methods: List[JApiMethod] = jApiClass.getMethods
    for (jApiMethod <- methods.asScala) {
      appendMethod(sb, signs(jApiMethod), jApiMethod, "METHOD:")
      processAnnotations(sb, jApiMethod, 2)
      processExceptions(sb, jApiMethod, 2)
    }
  }

  private def processExceptions(sb: StringBuilder, jApiBehavior: JApiBehavior, indent: Int): Unit = {
    for (exception <- jApiBehavior.getExceptions.asScala) {
      appendException(sb, signs(exception), exception, indent)
    }
  }

  private def appendException(sb: StringBuilder, signs: String, jApiException: JApiException, indent: Int): Unit = {
    sb.append(tabs(indent)).append(signs).append(" ").append(jApiException.getChangeStatus).append(" EXCEPTION: ").append(jApiException.getName).append("\n")
  }

  private def processClass(sb: StringBuilder, jApiClass: JApiClass): Unit = {
    appendClass(sb, signs(jApiClass), jApiClass)
  }

  private def signs(hasChangeStatus: JApiHasChangeStatus): String = {
    val changeStatus: JApiChangeStatus = hasChangeStatus.getChangeStatus
    var retVal: String = "???"
    changeStatus match {
      case JApiChangeStatus.UNCHANGED =>
        retVal = "==="

      case JApiChangeStatus.NEW =>
        retVal = "+++"

      case JApiChangeStatus.REMOVED =>
        retVal = "---"

      case JApiChangeStatus.MODIFIED =>
        retVal = "***"

    }
    var binaryCompatible: Boolean = true
    var sourceCompatible: Boolean = true
    if (hasChangeStatus.isInstanceOf[JApiCompatibility]) {
      val jApiCompatibility: JApiCompatibility = hasChangeStatus.asInstanceOf[JApiCompatibility]
      binaryCompatible = jApiCompatibility.isBinaryCompatible
      sourceCompatible = jApiCompatibility.isSourceCompatible
    }
    if (binaryCompatible) {
      if (sourceCompatible) {
        retVal += " "
      }
      else {
        retVal += "*"
      }
    }
    else {
      retVal += "!"
    }
    return retVal
  }

  private def appendMethod(sb: StringBuilder, signs: String, jApiBehavior: JApiBehavior, classMemberType: String): Unit = {
    sb.append("\t").append(signs).append(" ").append(jApiBehavior.getChangeStatus).append(" ").append(classMemberType).append(" ").append(accessModifierAsString(jApiBehavior)).append(abstractModifierAsString(jApiBehavior)).append(staticModifierAsString(jApiBehavior)).append(finalModifierAsString(jApiBehavior)).append(syntheticModifierAsString(jApiBehavior)).append(bridgeModifierAsString(jApiBehavior)).append(returnType(jApiBehavior)).append(jApiBehavior.getName).append("(")
    var paramCount: Int = 0
    for (jApiParameter <- jApiBehavior.getParameters.asScala) {
      if (paramCount > 0) {
        sb.append(", ")
      }
      sb.append(jApiParameter.getType)
      paramCount += 1
    }
    sb.append(")\n")
  }

  private def returnType(jApiBehavior: JApiBehavior): String = {
    var returnTypeAsString: String = ""
    if (jApiBehavior.isInstanceOf[JApiMethod]) {
      val method: JApiMethod = jApiBehavior.asInstanceOf[JApiMethod]
      val jApiReturnType: JApiReturnType = method.getReturnType
      if (jApiReturnType.getChangeStatus eq JApiChangeStatus.UNCHANGED) {
        returnTypeAsString = jApiReturnType.getNewReturnType + " "
      }
      else {
        if (jApiReturnType.getChangeStatus eq JApiChangeStatus.MODIFIED) {
          returnTypeAsString = jApiReturnType.getNewReturnType + " (<-" + jApiReturnType.getOldReturnType + ") "
        }
        else {
          if (jApiReturnType.getChangeStatus eq JApiChangeStatus.NEW) {
            returnTypeAsString = jApiReturnType.getNewReturnType + " "
          }
          else {
            returnTypeAsString = jApiReturnType.getOldReturnType + " "
          }
        }
      }
    }
    return returnTypeAsString
  }

  private def appendAnnotation(sb: StringBuilder, signs: String, jApiAnnotation: JApiAnnotation, numberOfTabs: Int): Unit = {
    sb.append(String.format("%s%s %s ANNOTATION: %s\n", tabs(numberOfTabs), signs, jApiAnnotation.getChangeStatus, jApiAnnotation.getFullyQualifiedName))
  }

  private def appendAnnotationElement(sb: StringBuilder, signs: String, jApiAnnotationElement: JApiAnnotationElement, numberOfTabs: Int): Unit = {
    sb.append(String.format("%s%s %s ELEMENT: %s=", tabs(numberOfTabs), signs, jApiAnnotationElement.getChangeStatus, jApiAnnotationElement.getName))
    val oldValue: Optional[MemberValue] = jApiAnnotationElement.getOldValue
    val newValue: Optional[MemberValue] = jApiAnnotationElement.getNewValue
    if (oldValue.isPresent && newValue.isPresent) {
      if (jApiAnnotationElement.getChangeStatus eq JApiChangeStatus.UNCHANGED) {
        sb.append(elementValueList2String(jApiAnnotationElement.getNewElementValues))
      }
      else {
        if (jApiAnnotationElement.getChangeStatus eq JApiChangeStatus.REMOVED) {
          sb.append(String.format("%s (-)", elementValueList2String(jApiAnnotationElement.getOldElementValues)))
        }
        else {
          if (jApiAnnotationElement.getChangeStatus eq JApiChangeStatus.NEW) {
            sb.append(String.format("%s (+)", elementValueList2String(jApiAnnotationElement.getNewElementValues)))
          }
          else {
            if (jApiAnnotationElement.getChangeStatus eq JApiChangeStatus.MODIFIED) {
              sb.append(String.format("%s (<- %s)", elementValueList2String(jApiAnnotationElement.getNewElementValues), elementValueList2String(jApiAnnotationElement.getOldElementValues)))
            }
          }
        }
      }
    }
    else {
      if (!(oldValue.isPresent) && newValue.isPresent) {
        sb.append(String.format("%s (+)", elementValueList2String(jApiAnnotationElement.getNewElementValues)))
      }
      else {
        if (oldValue.isPresent && !(newValue.isPresent)) {
          sb.append(String.format("%s (-)", elementValueList2String(jApiAnnotationElement.getOldElementValues)))
        }
        else {
          sb.append(" n.a.")
        }
      }
    }
    sb.append("\n")
  }

  private def elementValueList2String(values: List[JApiAnnotationElementValue]): String = {
    val sb: StringBuilder = new StringBuilder
    for (value <- values.asScala) {
      if (sb.length > 0) {
        sb.append(",")
      }
      if (value.getName.isPresent) {
        sb.append(value.getName.get).append("=")
      }
      if ((value.getType ne Type.Array) && (value.getType ne Type.Annotation)) {
        if (value.getType eq Type.Enum) {
          sb.append(value.getFullyQualifiedName).append(".").append(value.getValueString)
        }
        else {
          sb.append(value.getValueString)
        }
      }
      else {
        if (value.getType eq Type.Array) {
          sb.append("{").append(elementValueList2String(value.getValues)).append("}")
        }
        else {
          if (value.getType eq Type.Annotation) {
            sb.append("@").append(value.getFullyQualifiedName).append("(").append(elementValueList2String(value.getValues)).append(")")
          }
        }
      }
    }
    return sb.toString
  }

  private def tabs(numberOfTabs: Int): String = {
    if (numberOfTabs <= 0) {
      return ""
    }
    else {
      if (numberOfTabs == 1) {
        return "\t"
      }
      else {
        if (numberOfTabs == 2) {
          return "\t\t"
        }
        else {
          val sb: StringBuilder = new StringBuilder
          for (i <- 0 until numberOfTabs) {
            sb.append("\t")
          }
          return sb.toString
        }
      }
    }
  }

  private def appendClass(sb: StringBuilder, signs: String, jApiClass: JApiClass): Unit = {
    sb.append(signs).append(" ").append(jApiClass.getChangeStatus).append(" ").append(processClassType(jApiClass)).append(": ").append(accessModifierAsString(jApiClass)).append(abstractModifierAsString(jApiClass)).append(staticModifierAsString(jApiClass)).append(finalModifierAsString(jApiClass)).append(syntheticModifierAsString(jApiClass)).append(jApiClass.getFullyQualifiedName).append(" ").append(javaObjectSerializationStatus(jApiClass)).append("\n")
    processClassFileFormatVersionChanges(sb, jApiClass)
    processInterfaceChanges(sb, jApiClass)
    processSuperclassChanges(sb, jApiClass)
    processFieldChanges(sb, jApiClass)
  }

  private def processClassFileFormatVersionChanges(sb: StringBuilder, jApiClass: JApiClass): Unit = {
    val classFileFormatVersion: JApiClassFileFormatVersion = jApiClass.getClassFileFormatVersion

    val show = classFileFormatVersion.getMajorVersionNew != -1 && classFileFormatVersion.getMinorVersionNew != -1 &&
      classFileFormatVersion.getMajorVersionOld != -(1) && classFileFormatVersion.getMinorVersionOld != -1 &&
      classFileFormatVersion.getMajorVersionNew != classFileFormatVersion.getMajorVersionOld &&
      classFileFormatVersion.getMinorVersionNew != classFileFormatVersion.getMinorVersionNew

    if (show) {
      sb.append(tabs(1))
        .append(signs(classFileFormatVersion))
        .append(" CLASS FILE FORMAT VERSION: ")
      if (classFileFormatVersion.getMajorVersionNew != -(1) && classFileFormatVersion.getMinorVersionNew != -(1)) {
        sb.append(classFileFormatVersion.getMajorVersionNew).append(".").append(classFileFormatVersion.getMinorVersionNew)
      }
      else {
        sb.append("n.a.")
      }
      sb.append(" <- ")
      if (classFileFormatVersion.getMajorVersionOld != -(1) && classFileFormatVersion.getMinorVersionNew != -(1)) {
        sb.append(classFileFormatVersion.getMajorVersionOld).append(".").append(classFileFormatVersion.getMinorVersionOld)
      }
      else {
        sb.append("n.a.")
      }
      sb.append("\n")
    }
  }

  private def processClassType(jApiClass: JApiClass): String = {
    val classType: JApiClassType = jApiClass.getClassType
    classType.getChangeStatus match {
      case JApiChangeStatus.NEW =>
        return classType.getNewType
      case JApiChangeStatus.REMOVED =>
        return classType.getOldType
      case JApiChangeStatus.MODIFIED =>
        return classType.getNewType + " (<- " + classType.getOldType + ") "
      case JApiChangeStatus.UNCHANGED =>
        return classType.getOldType
    }
    return "n.a."
  }

  private def javaObjectSerializationStatus(jApiClass: JApiClass): String = {
    return " (" + jApiClass.getJavaObjectSerializationCompatible.getDescription + ")"
  }

  private def processFieldChanges(sb: StringBuilder, jApiClass: JApiClass): Unit = {
    val jApiFields: List[JApiField] = jApiClass.getFields
    for (jApiField <- jApiFields.asScala) {
      sb.append(tabs(1)).append(signs(jApiField)).append(" ").append(jApiField.getChangeStatus).append(" FIELD: ").append(accessModifierAsString(jApiField)).append(staticModifierAsString(jApiField)).append(finalModifierAsString(jApiField)).append(syntheticModifierAsString(jApiField)).append(fieldTypeChangeAsString(jApiField)).append(jApiField.getName).append("\n")
      processAnnotations(sb, jApiField, 2)
    }
  }

  private def abstractModifierAsString(hasAbstractModifier: JApiHasAbstractModifier): String = {
    val modifier: JApiModifier[AbstractModifier] = hasAbstractModifier.getAbstractModifier
    return modifierAsString(modifier, AbstractModifier.NON_ABSTRACT)
  }

  private def finalModifierAsString(hasFinalModifier: JApiHasFinalModifier): String = {
    val modifier: JApiModifier[FinalModifier] = hasFinalModifier.getFinalModifier
    return modifierAsString(modifier, FinalModifier.NON_FINAL)
  }

  private def staticModifierAsString(hasStaticModifier: JApiHasStaticModifier): String = {
    val modifier: JApiModifier[StaticModifier] = hasStaticModifier.getStaticModifier
    return modifierAsString(modifier, StaticModifier.NON_STATIC)
  }

  private def accessModifierAsString(modifier: JApiHasAccessModifier): String = {
    val accessModifier: JApiModifier[AccessModifier] = modifier.getAccessModifier
    return modifierAsString(accessModifier, AccessModifier.PACKAGE_PROTECTED)
  }

  private def syntheticModifierAsString(modifier: JApiHasSyntheticModifier): String = {
    val syntheticModifier: JApiModifier[SyntheticModifier] = modifier.getSyntheticModifier
    return modifierAsString(syntheticModifier, SyntheticModifier.NON_SYNTHETIC)
  }

  private def bridgeModifierAsString(modifier: JApiHasBridgeModifier): String = {
    val bridgeModifier: JApiModifier[BridgeModifier] = modifier.getBridgeModifier
    return modifierAsString(bridgeModifier, BridgeModifier.NON_BRIDGE)
  }

  private def modifierAsString[T](modifier: JApiModifier[T], notPrintValue: T): String = {
    if (modifier.getOldModifier.isPresent && modifier.getNewModifier.isPresent) {
      if (modifier.getChangeStatus eq JApiChangeStatus.MODIFIED) {
        return modifier.getNewModifier.get.toString + " (<- " + modifier.getOldModifier.get + ") "
      }
      else {
        if (modifier.getChangeStatus eq JApiChangeStatus.NEW) {
          if (modifier.getNewModifier.get.toString != notPrintValue) {
            return modifier.getNewModifier.get.toString + "(+) "
          }
        }
        else {
          if (modifier.getChangeStatus eq JApiChangeStatus.REMOVED) {
            if (modifier.getOldModifier.get != notPrintValue) {
              return modifier.getOldModifier.get.toString + "(-) "
            }
          }
          else {
            if (modifier.getNewModifier.get.toString != notPrintValue) {
              return modifier.getNewModifier.get.toString + " "
            }
          }
        }
      }
    }
    else {
      if (modifier.getOldModifier.isPresent) {
        if (modifier.getOldModifier.get != notPrintValue) {
          return modifier.getOldModifier.get.toString + "(-) "
        }
      }
      else {
        if (modifier.getNewModifier.isPresent) {
          if (modifier.getNewModifier.get.toString != notPrintValue) {
            return modifier.getNewModifier.get.toString + "(+) "
          }
        }
      }
    }
    return ""
  }

  private def fieldTypeChangeAsString(field: JApiField): String = {
    val `type`: JApiType = field.getType
    if (`type`.getOldTypeOptional.isPresent && `type`.getNewTypeOptional.isPresent) {
      if (`type`.getChangeStatus eq JApiChangeStatus.MODIFIED) {
        return `type`.getNewTypeOptional.get + " (<- " + `type`.getOldTypeOptional.get + ") "
      }
      else {
        if (`type`.getChangeStatus eq JApiChangeStatus.NEW) {
          return `type`.getNewTypeOptional.get + "(+) "
        }
        else {
          if (`type`.getChangeStatus eq JApiChangeStatus.REMOVED) {
            return `type`.getOldTypeOptional.get + "(-) "
          }
          else {
            return `type`.getNewTypeOptional.get + " "
          }
        }
      }
    }
    else {
      if (`type`.getOldTypeOptional.isPresent && !(`type`.getNewTypeOptional.isPresent)) {
        return `type`.getOldTypeOptional.get + " "
      }
      else {
        if (!(`type`.getOldTypeOptional.isPresent) && `type`.getNewTypeOptional.isPresent) {
          return `type`.getNewTypeOptional.get + " "
        }
      }
    }
    return "n.a."
  }

  private def processSuperclassChanges(sb: StringBuilder, jApiClass: JApiClass): Unit = {
    val jApiSuperclass: JApiSuperclass = jApiClass.getSuperclass
    if (options.isOutputOnlyModifications && (jApiSuperclass.getChangeStatus ne JApiChangeStatus.UNCHANGED)) {
      sb.append(tabs(1)).append(signs(jApiSuperclass)).append(" ").append(jApiSuperclass.getChangeStatus).append(" SUPERCLASS: ").append(superclassChangeAsString(jApiSuperclass)).append("\n")
    }
  }

  private def superclassChangeAsString(jApiSuperclass: JApiSuperclass): String = {
    if (jApiSuperclass.getOldSuperclassName.isPresent && jApiSuperclass.getNewSuperclassName.isPresent) {
      return jApiSuperclass.getNewSuperclassName.get + " (<- " + jApiSuperclass.getOldSuperclassName.get + ")"
    }
    else {
      if (jApiSuperclass.getOldSuperclassName.isPresent && !(jApiSuperclass.getNewSuperclassName.isPresent)) {
        return jApiSuperclass.getOldSuperclassName.get
      }
      else {
        if (!(jApiSuperclass.getOldSuperclassName.isPresent) && jApiSuperclass.getNewSuperclassName.isPresent) {
          return jApiSuperclass.getNewSuperclassName.get
        }
      }
    }
    return "n.a."
  }

  private def processInterfaceChanges(sb: StringBuilder, jApiClass: JApiClass): Unit = {
    val interfaces: List[JApiImplementedInterface] = jApiClass.getInterfaces
    for (implementedInterface <- interfaces.asScala) {
      sb.append(tabs(1)).append(signs(implementedInterface)).append(" ").append(implementedInterface.getChangeStatus).append(" INTERFACE: ").append(implementedInterface.getFullyQualifiedName).append("\n")
    }
  }
}
