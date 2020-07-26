package org.yankov.serialization.json

object JsonDataModel {
  type Bytes = Array[Byte]

  case class JsonNode(name: String, value: Any)

  case class JsonNodeString(name: String, value: String)
}
