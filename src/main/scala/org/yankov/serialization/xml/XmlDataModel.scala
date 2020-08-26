package org.yankov.serialization.xml

object XmlDataModel {
  case class Bytes(value: List[Byte])

  case class FieldValue(name: String, cls: Class[_], xmlNodes: List[String], value: Option[Any] = Option.empty)

  trait XmlDeserializationError

  case class FieldNotFoundError(message: String) extends XmlDeserializationError

  case class XmlParseError(message: String) extends XmlDeserializationError
}
