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

class InstanceFactoryTest extends WordSpec with Matchers {
  "create default instance should succeed" in {
    InstanceFactory.createDefaultInstance[Complex] shouldBe Complex(
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

  "set field should succeed" in {
    val obj = InstanceFactory.createDefaultInstance[Simple]
    InstanceFactory.setField(obj, "id", 1) shouldBe Simple(1, "")
    InstanceFactory.setField(obj, "name", "updated") shouldBe Simple(1, "updated")
  }
}
