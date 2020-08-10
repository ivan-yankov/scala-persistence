package org.yankov.serialization.json

import org.scalatest.{Matchers, WordSpec}

case class Simple(id: Int, name: String)

case class Complex(short: Short,
                   int: Int,
                   long: Long,
                   float: Float,
                   double: Double,
                   char: Char,
                   boolean: Boolean,
                   byte: Byte,
                   string: String,
                   seq: Seq[Int],
                   list: List[Byte],
                   vector: Vector[Int],
                   option: Option[String],
                   map: Map[Int, String])

class ReflectionUtilsTest extends WordSpec with Matchers {
  "create default instance should succeed" in {
    val result = ReflectionUtils.describe[Complex]

    result.defaultInstance shouldBe Complex(
      short = 0,
      int = 0,
      long = 0,
      float = 0.0f,
      double = 0.0,
      char = ' ',
      boolean = false,
      byte = 0,
      string = "",
      seq = Seq(),
      list = List(),
      vector = Vector(),
      option = Option.empty,
      map = Map()
    )

    result.fieldDescriptions shouldBe List(
      FieldDescription("short", "short"),
      FieldDescription("int", "int"),
      FieldDescription("long", "long"),
      FieldDescription("float", "float"),
      FieldDescription("double", "double"),
      FieldDescription("char", "char"),
      FieldDescription("boolean", "boolean"),
      FieldDescription("byte", "byte"),
      FieldDescription("string", "string"),
      FieldDescription("seq", "seq"),
      FieldDescription("list", "list"),
      FieldDescription("vector", "vector"),
      FieldDescription("option", "option"),
      FieldDescription("map", "map"),
    )
  }

  "set field should succeed" in {
    val description = ReflectionUtils.describe[Simple]
    ReflectionUtils.setField(description.defaultInstance, "id", 1) shouldBe Simple(1, "")
    ReflectionUtils.setField(description.defaultInstance, "name", "updated") shouldBe Simple(1, "updated")
  }
}
