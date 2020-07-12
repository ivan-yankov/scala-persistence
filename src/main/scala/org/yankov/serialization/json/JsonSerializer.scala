package org.yankov.serialization.json

import java.util.Base64

import org.yankov.serialization.json.JsonDataModel._

object JsonSerializer {
  private val numberOfDecimalPlaces = 16

  def toJson(obj: Product): String = {
    objectToString(obj)
  }

  private def determineJsonType(x: Any): JsonElement = x match {
    case value: Short => JsonInteger(value)
    case value: Int => JsonInteger(value)
    case value: Long => JsonInteger(value)
    case value: Float => JsonDecimal(value)
    case value: Double => JsonDecimal(value)
    case value: Char => JsonString(value.toString)
    case value: Boolean => JsonBoolean(value)
    case value: Byte => JsonBytes(Array(value))
    case value: Bytes => JsonBytes(value)
    case value: String => JsonString(value)
    case value: Seq[_] => JsonArray(value)
    case value: List[_] => JsonArray(value)
    case value: Vector[_] => JsonArray(value)
    case value: Set[_] => JsonArray(value.toList)
    case value: Product => JsonObject(value)
    case _ => JsonNull
  }

  private def jsonElementToString(x: JsonElement): String = x match {
    case JsonInteger(value) => value.toString
    case JsonDecimal(value) => printDouble(value)
    case JsonBoolean(value) => if (value) "true" else "false"
    case JsonString(value) => wrapInQuotes(value)
    case JsonBytes(value) => wrapInQuotes(encodeBytes(value))
    case JsonObject(value) => objectToString(value)
    case JsonArray(value) => collectionToString(value)
    case JsonNull => JsonConstants.`null`
  }

  private def objectToString(value: Product): String = {
    def printPair(key: String, value: String) = s"$key${JsonConstants.keyValueSeparator}$value"

    val r = Range(0, value.productIterator.size)
      .toList
      .map(x => (value.productElementName(x), value.productElement(x)))
      .map(x => printPair(wrapInQuotes(x._1), jsonElementToString(determineJsonType(x._2))))
      .mkString(JsonConstants.elementSeparator)
    s"{$r}"
  }

  private def collectionToString(items: Seq[_]): String = {
    val r = items
      .map(x => jsonElementToString(determineJsonType(x)))
      .mkString(JsonConstants.elementSeparator)
    s"[$r]"
  }

  private def printDouble(d: Double): String = String.format(s"%.${numberOfDecimalPlaces}f", d)

  private def encodeBytes(bytes: Bytes): String = Base64.getEncoder.withoutPadding.encodeToString(bytes)

  private def wrapInQuotes(str: String): String = s""""$str""""
}
