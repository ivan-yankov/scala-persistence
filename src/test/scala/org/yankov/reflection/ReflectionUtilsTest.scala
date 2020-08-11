package org.yankov.reflection

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
                   map: Map[Int, String],
                   simple: Simple)

class ReflectionUtilsTest extends WordSpec with Matchers {
  "create default instance should succeed" in {
      ReflectionUtils.createDefaultInstance[Complex](List(("Simple", Simple(0, "")))) shouldBe Complex(
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
      map = Map(),
      Simple(0, "")
    )
  }

  "describe should succeed" in {
    ReflectionUtils.describe[Complex] shouldBe ClassDescription(
      typeName = "Complex",
      fieldDescriptions = List(
        FieldDescription("short", "Short"),
        FieldDescription("int", "Int"),
        FieldDescription("long", "Long"),
        FieldDescription("float", "Float"),
        FieldDescription("double", "Double"),
        FieldDescription("char", "Char"),
        FieldDescription("boolean", "Boolean"),
        FieldDescription("byte", "Byte"),
        FieldDescription("string", "String"),
        FieldDescription("seq", "Seq"),
        FieldDescription("list", "List"),
        FieldDescription("vector", "Vector"),
        FieldDescription("option", "Option"),
        FieldDescription("map", "Map"),
        FieldDescription("simple", "Simple")
      )
    )
  }

  "set field should succeed" in {
    val instance = ReflectionUtils.createDefaultInstance[Simple]()
    ReflectionUtils.setField(instance, "id", 1) shouldBe Simple(1, "")
    ReflectionUtils.setField(instance, "name", "updated") shouldBe Simple(1, "updated")
  }
}
