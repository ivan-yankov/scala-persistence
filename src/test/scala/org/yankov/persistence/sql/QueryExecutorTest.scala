package org.yankov.persistence.sql

import java.sql.{Connection, DatabaseMetaData, Types}

import org.scalatest.{Matchers, WordSpec}
import org.yankov.persistence.sql.SqlModel.ColumnDefinition

class QueryExecutorTest extends WordSpec with Matchers {
  private def createDatabase(name: String): Connection = {
    val connectionString = ConnectionStringFactory.createDerbyConnectionString(
      InMemoryDatabaseProtocol,
      name,
      Map("create" -> "true")
    )

    DatabaseConnection.connect(connectionString).getOrElse().asInstanceOf[Connection]
  }

  "create schema, create table and drop table should succeed" in {
    val executor = QueryExecutor(createDatabase("test-schema-table"))

    val schema = "SCM"
    val table = "TBL"
    val columns = List(
      ColumnDefinition("ID", "INT", "NOT NULL"),
      ColumnDefinition("VAL", "DOUBLE", "")
    )

    val createSchemaResult = executor.createSchema(schema)
    createSchemaResult.isRight shouldBe true

    val createTableResult = executor.createTable(schema, table, columns)
    createTableResult.isRight shouldBe true

    val tables = executor.connection.getMetaData.getTables(null, schema, null, Array("TABLE"))
    tables.next() shouldBe true
    tables.getString("TABLE_SCHEM") shouldBe schema
    tables.getString("TABLE_NAME") shouldBe table

    val dropTableResult = executor.dropTable(schema, table)
    dropTableResult.isRight shouldBe true

    executor
      .connection
      .getMetaData
      .getTables(null, schema, null, Array("TABLE"))
      .next() shouldBe false

    executor.connection.close()
  }

  "add column should succeed" in {
    val executor = QueryExecutor(createDatabase("test-add-column"))

    val schema = "SCM"
    val table = "TBL"
    val columns = List(
      ColumnDefinition("ID", "INT", "NOT NULL"),
      ColumnDefinition("VAL", "DOUBLE", "")
    )

    executor.createSchema(schema)
    executor.createTable(schema, table, columns)

    val tableColumns = executor.connection.getMetaData.getColumns(null, null, table, null)
    tableColumns.next() shouldBe true
    tableColumns.getString("COLUMN_NAME") shouldBe "ID"
    tableColumns.getInt("DATA_TYPE") shouldBe Types.INTEGER
    tableColumns.next() shouldBe true
    tableColumns.getString("COLUMN_NAME") shouldBe "VAL"
    tableColumns.getInt("DATA_TYPE") shouldBe Types.DOUBLE
    tableColumns.next() shouldBe false

    executor.addColumn(schema, table, ColumnDefinition("ADDED_COLUMN", "VARCHAR(256)", ""))

    val newTableColumns = executor.connection.getMetaData.getColumns(null, null, table, null)
    newTableColumns.next() shouldBe true
    newTableColumns.getString("COLUMN_NAME") shouldBe "ID"
    newTableColumns.getInt("DATA_TYPE") shouldBe Types.INTEGER
    newTableColumns.next() shouldBe true
    newTableColumns.getString("COLUMN_NAME") shouldBe "VAL"
    newTableColumns.getInt("DATA_TYPE") shouldBe Types.DOUBLE
    newTableColumns.next() shouldBe true
    newTableColumns.getString("COLUMN_NAME") shouldBe "ADDED_COLUMN"
    newTableColumns.getInt("DATA_TYPE") shouldBe Types.VARCHAR
    newTableColumns.next() shouldBe false

    executor.connection.close()
  }

  "drop column should succeed" in {
    val executor = QueryExecutor(createDatabase("test-drop-column"))

    val schema = "SCM"
    val table = "TBL"
    val columns = List(
      ColumnDefinition("ID", "INT", "NOT NULL"),
      ColumnDefinition("VAL", "DOUBLE", "")
    )

    executor.createSchema(schema)
    executor.createTable(schema, table, columns)

    val tableColumns = executor.connection.getMetaData.getColumns(null, null, table, null)
    tableColumns.next() shouldBe true
    tableColumns.getString("COLUMN_NAME") shouldBe "ID"
    tableColumns.getInt("DATA_TYPE") shouldBe Types.INTEGER
    tableColumns.next() shouldBe true
    tableColumns.getString("COLUMN_NAME") shouldBe "VAL"
    tableColumns.getInt("DATA_TYPE") shouldBe Types.DOUBLE
    tableColumns.next() shouldBe false

    executor.dropColumn(schema, table, "VAL")

    val newTableColumns = executor.connection.getMetaData.getColumns(null, null, table, null)
    newTableColumns.next() shouldBe true
    newTableColumns.getString("COLUMN_NAME") shouldBe "ID"
    newTableColumns.getInt("DATA_TYPE") shouldBe Types.INTEGER
    newTableColumns.next() shouldBe false

    executor.connection.close()
  }
}
