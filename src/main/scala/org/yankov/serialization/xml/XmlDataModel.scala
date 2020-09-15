package org.yankov.serialization.xml

object XmlDataModel {
  case class FieldValue(name: String, cls: Class[_], xmlNodes: List[XmlNode], value: Option[Any] = Option.empty)

  trait XmlDeserializationError

  case class FieldNotFoundError(message: String) extends XmlDeserializationError

  case class XmlParseError(message: String) extends XmlDeserializationError

  case class XmlNode(tag: String, attributes: Map[String, String], value: String)

  class FieldNotFoundException(message: String) extends Exception(message)

  class XmlParseException(message: String) extends Exception(message)
}
