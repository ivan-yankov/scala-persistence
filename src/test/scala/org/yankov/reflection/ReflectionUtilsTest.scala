package org.yankov.reflection

import org.scalatest.{Matchers, WordSpec}

case class Simple(id: Int, name: String)

case class Base(short: Short,
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
  "create instance should succeed" in {
    val parameters = ReflectionUtils
      .getFields(classOf[Base])
      .map(x => x.cls)
      .map(x => ReflectionUtils.defaultValue(x))
      .map(x => x.get)
    ReflectionUtils.createInstance[Base](parameters) shouldBe Base(
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
  }

  "get fields for class name should succeed" in {
    ReflectionUtils.getFields(classOf[Complex]) shouldBe List(
      Field("short", classOf[Short]),
      Field("int", classOf[Int]),
      Field("long", classOf[Long]),
      Field("float", classOf[Float]),
      Field("double", classOf[Double]),
      Field("char", classOf[Char]),
      Field("boolean", classOf[Boolean]),
      Field("byte", classOf[Byte]),
      Field("string", classOf[String]),
      Field("seq", classOf[Seq[_]]),
      Field("list", classOf[List[_]]),
      Field("vector", classOf[Vector[_]]),
      Field("option", classOf[Option[_]]),
      Field("map", classOf[Map[_, _]]),
      Field("simple", classOf[Simple])
    )
  }

  "set field should succeed" in {
    val instance = ReflectionUtils.createInstance[Simple](List(0, ""))
    ReflectionUtils.setField(instance, "id", 1) shouldBe Simple(1, "")
    ReflectionUtils.setField(instance, "name", "updated") shouldBe Simple(1, "updated")
  }
}
