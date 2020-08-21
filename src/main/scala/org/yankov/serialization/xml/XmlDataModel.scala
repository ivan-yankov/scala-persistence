package org.yankov.serialization.xml

object XmlDataModel {

  case class Bytes(value: List[Byte])

  case class ObjectNode(name: String, value: Any)

  case class XmlNode(name: String,
                     `type`: String,
                     className: Option[String],
                     level: Int,
                     value: String)

  case class MapElement[K, V](key: K, value: V)

  //  case class FieldValue(name: String, cls: Class[_], jsonNodes: List[XmlNode], value: Option[Any] = Option.empty)

  //  trait JsonDeserializationError
  //
  //  case class FieldNotFoundError(message: String) extends JsonDeserializationError
  //
  //  case class JsonParseError(message: String) extends JsonDeserializationError
}
