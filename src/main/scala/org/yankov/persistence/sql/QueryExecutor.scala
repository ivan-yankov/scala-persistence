package org.yankov.persistence.sql

import java.sql._

import org.yankov.datastructures.Types.Bytes
import org.yankov.persistence.sql.SqlModel._

import scala.annotation.tailrec

case class QueryExecutor(connection: Connection) {
  private val columnName = "COLUMN_NAME"
  private val dataType = "DATA_TYPE"

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

  def insert(schemaName: String, tableName: String, columns: List[String], data: List[List[SqlValue]]): Either[Throwable, Unit] = {
    try {
      val s = connection.prepareStatement(insertQuery(schemaName, tableName, columns))

      data.foreach(x => {
        addRow(s, x)
        s.addBatch()
      })

      val result = s.executeBatch()
      val ok = result.size == data.size && result.forall(x => x == 1 || x == Statement.SUCCESS_NO_INFO)
      if (ok) Right()
      else throw new SQLException("Insert was not successful.")
    } catch {
      case e: SQLException => Left(e)
    }
  }

  def select(schemaName: String, tableName: String, columns: List[String] = List(), criteria: List[Clause] = List()): Either[Throwable, List[List[SqlValue]]] = {
    try {
      val s = connection.prepareStatement(selectQuery(schemaName, tableName, columns, criteria))

      criteria
        .indices
        .toList
        .map(x => x + 1)
        .zip(criteria)
        .foreach(x => setStatementValue(s, x._1, x._2.value))

      val result = s.executeQuery()

      @tailrec
      def iterate(acc: List[List[SqlValue]]): List[List[SqlValue]] = {
        if (!result.next()) acc
        else {
          val row = getRow(result, schemaName, tableName, columns)
          iterate(acc.appended(row))
        }
      }

      val r = iterate(List())
      Right(r)
    } catch {
      case e: SQLException => Left(e)
    }
  }

  private def columnToString(column: ColumnDefinition): String = s"${column.name} ${column.sqlType} ${column.constraint}"

  private def insertQuery(schemaName: String, tableName: String, columns: List[String]): String = {
    val placeholders = columns.map(_ => "?").mkString(",")
    s"INSERT INTO $schemaName.$tableName(${columns.mkString(",")}) VALUES($placeholders)"
  }

  private def selectQuery(schemaName: String, tableName: String, columns: List[String], criteria: List[Clause]): String = {
    val s = if (columns.nonEmpty) columns.mkString(",") else "*"
    val c = if (criteria.nonEmpty) criteria.map(x => s" ${x.name} ${x.column}${x.operator}?").mkString else ""
    s"SELECT $s FROM $schemaName.$tableName$c"
  }

  private def setStatementValue(s: PreparedStatement, i: Int, x: SqlValue): Unit = x match {
    case IntSqlValue(value) => s.setInt(i, value)
    case LongSqlValue(value) => s.setLong(i, value)
    case DoubleSqlValue(value) => s.setDouble(i, value)
    case BooleanSqlValue(value) => s.setBoolean(i, value)
    case BytesSqlValue(value) => s.setBytes(i, value.value.toArray)
    case StringSqlValue(value) => s.setString(i, value)
  }

  private def getResultValue(result: ResultSet, columnName: String, columnType: Int): SqlValue = columnType match {
    case Types.INTEGER => IntSqlValue(result.getInt(columnName))
    case Types.BIGINT => LongSqlValue(result.getLong(columnName))
    case Types.FLOAT => DoubleSqlValue(result.getDouble(columnName))
    case Types.DOUBLE => DoubleSqlValue(result.getDouble(columnName))
    case Types.BIT => BooleanSqlValue(result.getBoolean(columnName))
    case Types.BOOLEAN => BooleanSqlValue(result.getBoolean(columnName))
    case Types.BINARY => BytesSqlValue(Bytes(result.getBytes(columnName).toList))
    case Types.VARBINARY => BytesSqlValue(Bytes(result.getBytes(columnName).toList))
    case Types.LONGVARBINARY => BytesSqlValue(Bytes(result.getBytes(columnName).toList))
    case Types.BLOB => BytesSqlValue(Bytes(result.getBytes(columnName).toList))
    case Types.CHAR => StringSqlValue(result.getString(columnName))
    case Types.VARCHAR => StringSqlValue(result.getString(columnName))
    case Types.LONGVARCHAR => StringSqlValue(result.getString(columnName))
    case Types.CLOB => StringSqlValue(result.getString(columnName))
  }

  private def addRow(s: PreparedStatement, values: List[SqlValue]): Unit = {
    values
      .indices
      .toList
      .map(x => x + 1)
      .zip(values)
      .foreach(x => setStatementValue(s, x._1, x._2))
  }

  private def getRow(result: ResultSet, schemaName: String, tableName: String, columns: List[String]): List[SqlValue] = {
    val allTableColumnsResultSet = connection.getMetaData.getColumns(null, schemaName, tableName, null)

    @tailrec
    def getAllTableColumns(acc: List[(String, Int)]): List[(String, Int)] = {
      if (!allTableColumnsResultSet.next()) acc
      else getAllTableColumns(acc.appended((allTableColumnsResultSet.getString(columnName), allTableColumnsResultSet.getInt(dataType))))
    }

    val allTableColumns = getAllTableColumns(List())

    val selectColumns = if (columns.nonEmpty) columns else allTableColumns.map(x => x._1)

    allTableColumns
      .filter(x => selectColumns.contains(x._1))
      .map(x => getResultValue(result, x._1, x._2))
  }
}
