package org.yankov.serialization.xml

object XmlCommons {
  val openTag: String = "<"
  val closeTag: String = ">"
  val closeTagNamePrefix: String = "/"
  val attributeKeyValueSeparator: String = "="
  val attributeValueWrapper: String = "\""

  val typeAttributeName: String = "type"
  val classNameAttributeName: String = "className"

  object Types {
    val short: String = "Short"
    val int: String = "Int"
    val long: String = "Long"
    val float: String = "Float"
    val double: String = "Double"
    val char: String = "Char"
    val boolean: String = "Boolean"
    val byte: String = "Byte"
    val bytes: String = "Bytes"
    val string: String = "String"
    val list: String = "List"
    val vector: String = "Vector"
    val set: String = "Set"
    val map: String = "Map"
    val option: String = "Option"
    val obj: String = "Object"
  }

  val baseTypes: List[String] = List(
    Types.short,
    Types.int,
    Types.long,
    Types.float,
    Types.double,
    Types.char,
    Types.boolean,
    Types.byte,
    Types.bytes,
    Types.string
  )
}
