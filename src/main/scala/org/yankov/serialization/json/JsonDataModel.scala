package org.yankov.serialization.json

object JsonDataModel {
  case class Bytes(value: List[Byte])

  case class JsonNode(name: String, value: Any)

  case class JsonNodeString(name: String, value: String)
}
