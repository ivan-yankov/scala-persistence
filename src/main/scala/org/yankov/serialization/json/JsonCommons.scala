package org.yankov.serialization.json

import scala.annotation.tailrec

object JsonCommons {
  val elementSeparator: String = ","
  val keyValueSeparator: String = ":"
  val `null`: String = "null"
  val openObject: String = "{"
  val closeObject: String = "}"
  val openArray: String = "["
  val closeArray: String = "]"
  val stringWrapper: String = "\""

  implicit class StringExtensions(s: String) {
    def wrapJsonString(): String = s"$stringWrapper$s$stringWrapper"

    def wrapJsonArray(): String = s"$openArray$s$closeArray"

    def wrapJsonObject(): String = s"$openObject$s$closeObject"

    def unwrapJsonString(): String = s.substring(stringWrapper.length, s.length - stringWrapper.length)

    def unwrapJsonArray(): String = s.substring(openArray.length, s.length - closeArray.length)

    def unwrapJsonObject(): String = s.substring(openObject.length, s.length - closeObject.length)

    def splitElements(): List[String] = {
      def isValidSeparatorIndex(index: Int, str: String): Boolean = {
        if (index < 0) true
        else {
          val substr = str.substring(0, index)
          val openObjectCount = substr.count(x => x.toString.equals(openObject))
          val closeObjectCount = substr.count(x => x.toString.equals(closeObject))
          val openArrayCount = substr.count(x => x.toString.equals(openArray))
          val closeArrayCount = substr.count(x => x.toString.equals(closeArray))
          val quotesCount = substr.count(x => x.toString.equals(stringWrapper))
          val bracesOk = openObjectCount + openArrayCount - closeObjectCount - closeArrayCount == 0
          val quotesOk = quotesCount % 2 == 0
          bracesOk && quotesOk
        }
      }

      @tailrec
      def separatorIndex(str: String, from: Int = 0): Int = {
        val index = str.indexOf(elementSeparator, from)
        if (isValidSeparatorIndex(index, str)) index
        else separatorIndex(str, index + 1)
      }

      @tailrec
      def iterate(str: String, acc: List[String]): List[String] = {
        val index = separatorIndex(str)
        if (index < 0) acc.appended(str)
        else iterate(str.substring(index + 1, str.length), acc.appended(str.substring(0, index)))
      }

      iterate(s, List())
    }

    def splitPair(): (String, String) = {
      val index = s.indexOf(keyValueSeparator)
      (s.substring(0, index).unwrapJsonString(), s.substring(index + 1, s.length))
    }
  }
}
