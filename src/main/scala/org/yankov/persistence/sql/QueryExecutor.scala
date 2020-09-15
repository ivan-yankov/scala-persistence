package org.yankov.persistence.sql

import java.sql.{Connection, PreparedStatement, ResultSet, SQLException, Statement}

import org.slf4j.LoggerFactory
import org.yankov.persistence.sql.SqlModel._

case class QueryExecutor(connection: Connection) {
  private val log = LoggerFactory.getLogger(getClass)

  def createSchema(name: String): Either[Throwable, Unit] = {
    try {
      connection.prepareStatement(s"CREATE SCHEMA $name").execute()
      Right()
    } catch {
      case e: SQLException => Left(e)
    }
  }

  def createTable(schemaName: String, tableName: String, columns: List[ColumnDefinition]): Either[Throwable, Unit] = {
    try {
      val fields = columns
        .map(x => columnToString(x))
        .mkString(", ")
      connection.prepareStatement(s"CREATE TABLE $schemaName.$tableName($fields)").execute()
      Right()
    } catch {
      case e: SQLException => Left(e)
    }
  }

  def dropTable(schemaName: String, tableName: String): Either[Throwable, Unit] = {
    try {
      connection.prepareStatement(s"DROP TABLE $schemaName.$tableName").execute()
      Right()
    } catch {
      case e: SQLException => Left(e)
    }
  }

  def addColumn(schemaName: String, tableName: String, column: ColumnDefinition): Either[Throwable, Unit] = {
    try {
      connection.prepareStatement(s"ALTER TABLE $schemaName.$tableName ADD ${columnToString(column)}").execute()
      Right()
    } catch {
      case e: SQLException => Left(e)
    }
  }

  def dropColumn(schemaName: String, tableName: String, columnName: String): Either[Throwable, Unit] = {
    try {
      connection.prepareStatement(s"ALTER TABLE $schemaName.$tableName DROP $columnName").execute()
      Right()
    } catch {
      case e: SQLException => Left(e)
    }
  }

  def insert(schemaName: String, tableName: String, columns: List[String], data: List[List[SqlValue]]): Boolean = {
    try {
      val s = connection.prepareStatement(insertQuery(schemaName, tableName, columns))

      data.foreach(x => {
        addRow(s, x)
        s.addBatch()
      })

      val result = s.executeBatch()
      result.size == data.size && result.forall(x => x == 1 || x == Statement.SUCCESS_NO_INFO)
    } catch {
      case e: SQLException =>
        log.error("Unable to insert data", e)
        false
    }
  }

  def select(schemaName: String, tableName: String, columns: List[String] = List(), criteria: List[Clause] = List()): Option[ResultSet] = {
    try {
      val s = connection.prepareStatement(selectQuery(schemaName, tableName, columns, criteria))

      criteria
        .indices
        .toList
        .zip(criteria)
        .foreach(x => setStatementValue(s, x._1, x._2.value))

      Option(s.executeQuery())
    } catch {
      case e: SQLException =>
        log.error("Unable to select data", e)
        Option.empty
    }
  }

  private def columnToString(column: ColumnDefinition): String = s"${column.name} ${column.sqlType} ${column.constraint}"

  private def insertQuery(schemaName: String, tableName: String, columns: List[String]): String = {
    val placeholders = columns.map(_ => "?").mkString(",")
    s"INSERT INTO $schemaName.$tableName(${columns.mkString(",")}) VALUES($placeholders)"
  }

  private def setStatementValue(s: PreparedStatement, i: Int, x: SqlValue): Unit = x match {
    case ShortSqlValue(value) => s.setShort(i, value)
    case IntSqlValue(value) => s.setInt(i, value)
    case LongSqlValue(value) => s.setLong(i, value)
    case FloatSqlValue(value) => s.setFloat(i, value)
    case DoubleSqlValue(value) => s.setDouble(i, value)
    case CharSqlValue(value) => s.setString(i, value.toString)
    case BooleanSqlValue(value) => s.setBoolean(i, value)
    case ByteSqlValue(value) => s.setByte(i, value)
    case BytesSqlValue(value) => s.setBytes(i, value.value.toArray)
    case StringSqlValue(value) => s.setString(i, value)
  }

  private def addRow(s: PreparedStatement, values: List[SqlValue]): Unit = {
    values
      .indices
      .toList
      .zip(values)
      .foreach(x => setStatementValue(s, x._1, x._2))
  }

  private def selectQuery(schemaName: String, tableName: String, columns: List[String], criteria: List[Clause]): String = {
    val s = if (columns.nonEmpty) columns.mkString(",") else "*"
    val c = if (criteria.nonEmpty) criteria.map(x => s" ${x.name} ${x.column}${x.operator}?") else ""
    s"SELECT $s FROM $schemaName.$tableName$c"
  }
}
