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
  "create default instance should succeed" in {
    ReflectionUtils.createDefaultInstance("org.yankov.reflection.Base") shouldBe Base(
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
    ReflectionUtils.getFields("org.yankov.reflection.Complex") shouldBe List(
      ClassDescription("short", "scala.Short"),
      ClassDescription("int", "scala.Int"),
      ClassDescription("long", "scala.Long"),
      ClassDescription("float", "scala.Float"),
      ClassDescription("double", "scala.Double"),
      ClassDescription("char", "scala.Char"),
      ClassDescription("boolean", "scala.Boolean"),
      ClassDescription("byte", "scala.Byte"),
      ClassDescription("string", "java.lang.String"),
      ClassDescription("seq", "scala.collection.immutable.Seq"),
      ClassDescription("list", "scala.collection.immutable.List"),
      ClassDescription("vector", "scala.collection.immutable.Vector"),
      ClassDescription("option", "scala.Option"),
      ClassDescription("map", "scala.collection.immutable.Map"),
      ClassDescription("simple", "org.yankov.reflection.Simple")
    )
  }

  "set field should succeed" in {
    val instance = ReflectionUtils.createDefaultInstance("org.yankov.reflection.Simple").asInstanceOf[Simple]
    ReflectionUtils.setField(instance, "id", 1) shouldBe Simple(1, "")
    ReflectionUtils.setField(instance, "name", "updated") shouldBe Simple(1, "updated")
  }
}
