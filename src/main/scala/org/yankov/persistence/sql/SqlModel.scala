package org.yankov.persistence.sql

import org.yankov.datastructures.Types.Bytes

object SqlModel {
  case class ColumnDefinition(name: String, sqlType: String, constraint: String)

  trait Clause {
    def name: String
    def column: String
    def operator: String
    def value: SqlValue
  }

  case class WhereClause(column: String, operator: String, value: SqlValue) extends Clause {
    override def name: String = "WHERE"
  }
  case class AndClause(column: String, operator: String, value: SqlValue) extends Clause {
    override def name: String = "AND"
  }
  case class OrClause(column: String, operator: String, value: SqlValue) extends Clause {
    override def name: String = "OR"
  }

  trait SqlValue

  case class ShortSqlValue(value: Short) extends SqlValue

  case class IntSqlValue(value: Int) extends SqlValue

  case class LongSqlValue(value: Long) extends SqlValue

  case class FloatSqlValue(value: Float) extends SqlValue

  case class DoubleSqlValue(value: Double) extends SqlValue

  case class CharSqlValue(value: Char) extends SqlValue

  case class BooleanSqlValue(value: Boolean) extends SqlValue

  case class ByteSqlValue(value: Byte) extends SqlValue

  case class BytesSqlValue(value: Bytes) extends SqlValue

  case class StringSqlValue(value: String) extends SqlValue
}
