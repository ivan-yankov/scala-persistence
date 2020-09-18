package org.yankov.persistence.sql

import org.yankov.datastructures.Types.Bytes

object SqlModel {
  object DerbySqlTypes {
    val short: String = "INTEGER"
    val int: String = "INTEGER"
    val long: String = "BIGINT"
    val float: String = "DOUBLE"
    val double: String = "DOUBLE"
    val boolean: String = "BOOLEAN"
    val bytes: String = "BLOB"
    val string: String = "CLOB"

    def varchar(size: Int): String = s"VARCHAR($size)"
  }

  object DerbySqlConstraints {
    val notNull: String = "NOT NULL"
    val primaryKey: String = "PRIMARY KEY"
    val unique: String = "UNIQUE"
    val foreignKey: String = "FOREIGN KEY"
    val check: String = "CHECK"
  }

  case class ColumnDefinition(name: String, sqlType: String, constraint: String = "")

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

  trait SqlValue {
    private def msg(x: String) = s"Value is not [$x]"

    def asInt: Int = this match {
      case IntSqlValue(value) => value
      case _ => throw new UnsupportedOperationException(msg("Int"))
    }

    def asLong: Long = this match {
      case LongSqlValue(value) => value
      case _ => throw new UnsupportedOperationException(msg("Long"))
    }

    def asDouble: Double = this match {
      case DoubleSqlValue(value) => value
      case _ => throw new UnsupportedOperationException(msg("Double"))
    }

    def asBoolean: Boolean = this match {
      case BooleanSqlValue(value) => value
      case _ => throw new UnsupportedOperationException(msg("Boolean"))
    }

    def asBytes: Bytes = this match {
      case BytesSqlValue(value) => value
      case _ => throw new UnsupportedOperationException(msg("Bytes"))
    }

    def asString: String = this match {
      case StringSqlValue(value) => value
      case _ => throw new UnsupportedOperationException(msg("String"))
    }
  }

  case class IntSqlValue(value: Int) extends SqlValue

  case class LongSqlValue(value: Long) extends SqlValue

  case class DoubleSqlValue(value: Double) extends SqlValue

  case class BooleanSqlValue(value: Boolean) extends SqlValue

  case class BytesSqlValue(value: Bytes) extends SqlValue

  case class StringSqlValue(value: String) extends SqlValue
}
