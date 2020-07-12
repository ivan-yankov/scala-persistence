package org.yankov.serialization.json

object JsonDataModel {
  type Bytes = Array[Byte]

  trait JsonElement

  case class JsonString(value: String) extends JsonElement

  case class JsonInteger(value: Long) extends JsonElement

  case class JsonDecimal(value: Double) extends JsonElement

  case class JsonBytes(value: Bytes) extends JsonElement

  case class JsonBoolean(value: Boolean) extends JsonElement

  case class JsonObject(value: Product) extends JsonElement

  case class JsonArray(value: Seq[_]) extends JsonElement

  case object JsonNull extends JsonElement
}
