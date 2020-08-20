package org.yankov.serialization.json

object JsonDataModel {
  case class Bytes(value: List[Byte])

  case class JsonNode(name: String, value: Any)

  case class JsonNodeString(name: String, value: String)

  case class FieldValue(name: String, cls: Class[_], jsonNodes: List[JsonNodeString], value: Option[Any] = Option.empty)

  trait JsonDeserializationError

  case class FieldNotFoundError(message: String) extends JsonDeserializationError

  case class JsonParseError(message: String) extends JsonDeserializationError
}
