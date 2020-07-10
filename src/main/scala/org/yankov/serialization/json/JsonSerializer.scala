package org.yankov.serialization.json

import java.util.Base64

import org.slf4j.LoggerFactory

trait JsonType
case class JsonObject(value: Product) extends JsonType
case class JsonArray(value: Seq[_]) extends JsonType
case class JsonValue(value: Any) extends JsonType
case object JsonNull extends JsonType

object JsonSerializer {
  type Bytes = Array[Byte]

  private val log = LoggerFactory.getLogger(JsonSerializer.getClass)
  private val numberOfDecimalPlaces = 16
  private val elementSeparator = ","
  private val `null` = "null"

  def toJson(obj: Product): String = {
    objectToString(obj)
  }

  private def determineJsonType(x: Any): JsonType = x match {
    case value: Short => JsonValue(value)
    case value: Int => JsonValue(value)
    case value: Long => JsonValue(value)
    case value: Float => JsonValue(value)
    case value: Double => JsonValue(value)
    case value: Char => JsonValue(value)
    case value: Boolean => JsonValue(value)
    case value: Byte => JsonValue(value)
    case value: Bytes => JsonValue(value)
    case value: String => JsonValue(value)
    case value: Seq[_] => JsonArray(value)
    case value: List[_] => JsonArray(value)
    case value: Vector[_] => JsonArray(value)
    case value: Set[_] => JsonArray(value.toList)
    case value: Product => JsonObject(value)
    case _ => JsonNull
  }

  private def valueToString(x: Any): String = x match {
    case value: Short => value.toString
    case value: Int => value.toString
    case value: Long => value.toString
    case value: Float => printDouble(value)
    case value: Double => printDouble(value)
    case value: Boolean => if (value) "true" else "false"
    case value: Char => wrapInQuotes(value.toString)
    case value: Byte => wrapInQuotes(encodeBytes(Seq(value)))
    case value: Bytes => wrapInQuotes(encodeBytes(value))
    case value: String => wrapInQuotes(value)
    case value =>
      log.error(s"Type [${x.getClass.getName}] is not a value")
      value.toString
  }

  private def collectionToString(items: Seq[_]): String = {
    val r = items
      .map(x => serializeItem(determineJsonType(x)))
      .mkString(elementSeparator)
    s"[$r]"
  }

  private def objectToString(value: Product): String = {
    def printPair(key: String, value: String) = s"$key:$value"

    val r = Range(0, value.productIterator.size)
      .toList
      .map(x => (value.productElementName(x), value.productElement(x)))
      .map(x => printPair(wrapInQuotes(x._1), serializeItem(determineJsonType(x._2))))
      .mkString(elementSeparator)
    s"{$r}"
  }

  private def serializeItem(item: JsonType): String = item match {
      case JsonValue(value) => valueToString(value)
      case JsonArray(value) => collectionToString(value)
      case JsonObject(value) => objectToString(value)
      case JsonNull => `null`
      case _ => `null`
    }

  private def printDouble(d: Double): String = String.format(s"%.${numberOfDecimalPlaces}f", d)

  private def encodeBytes(bytes: Seq[Byte]): String = Base64.getEncoder.withoutPadding.encodeToString(bytes.toArray)

  private def wrapInQuotes(str: String): String = s""""$str""""
}
