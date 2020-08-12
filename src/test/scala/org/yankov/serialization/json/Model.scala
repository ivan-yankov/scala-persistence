package org.yankov.serialization.json

import org.yankov.serialization.json.JsonDataModel.Bytes

case class Dependency(id: String, description: String)

case class Pair(id: Int, dependency: Dependency)

case class Entity(short: Short,
                  int: Int,
                  long: Long,
                  float: Float,
                  double: Double,
                  char: Char,
                  boolean1: Boolean,
                  boolean2: Boolean,
                  byte: Byte,
                  bytes: Bytes,
                  string: String,
                  emptySeq: Seq[Int],
                  valuesSeq: Seq[Int],
                  objectSeq: Seq[Dependency],
                  arraySeq: Seq[Seq[Double]],
                  valuesList: List[Int],
                  objectList: List[Dependency],
                  arrayList: List[List[Double]],
                  valuesVector: Vector[Int],
                  objectVector: Vector[Dependency],
                  arrayVector: Vector[Vector[Double]],
                  pair: Pair,
                  option1: Option[String],
                  option2: Option[String],
                  child: Option[Entity],
                  map: Map[Int, String])
