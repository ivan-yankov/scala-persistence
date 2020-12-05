package org.yankov.persistence.sql

import java.sql.{Connection, Types}

import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}
import org.yankov.datastructures.Types.Bytes
import org.yankov.persistence.sql.SqlModel._

class QueryExecutorTest extends WordSpec with Matchers with BeforeAndAfterAll {
  override def beforeAll(): Unit = System.setSecurityManager(null)

  "create schema, create table and drop table should succeed" in {
    val executor = QueryExecutor(createDatabase("test-schema-table"))

    val schema = "SCM"
    val table = "TBL"
    val columns = List(
      ColumnDefinition("ID", DerbySqlTypes.int, DerbySqlConstraints.notNull),
      ColumnDefinition("VAL", DerbySqlTypes.double)
    )

    executor.createSchema(schema).isRight shouldBe true
    executor.createTable(schema, table, columns).isRight shouldBe true

    val tables = executor.connection.getMetaData.getTables(null, schema, null, Array("TABLE"))
    tables.next() shouldBe true
    tables.getString("TABLE_SCHEM") shouldBe schema
    tables.getString("TABLE_NAME") shouldBe table

    executor.dropTable(schema, table).isRight shouldBe true

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
      ColumnDefinition("ID", DerbySqlTypes.int, DerbySqlConstraints.notNull),
      ColumnDefinition("VAL", DerbySqlTypes.double)
    )

    executor.createSchema(schema).isRight shouldBe true
    executor.createTable(schema, table, columns).isRight shouldBe true

    val tableColumns = executor.connection.getMetaData.getColumns(null, schema, table, null)
    tableColumns.next() shouldBe true
    tableColumns.getString("COLUMN_NAME") shouldBe "ID"
    tableColumns.getInt("DATA_TYPE") shouldBe Types.INTEGER
    tableColumns.next() shouldBe true
    tableColumns.getString("COLUMN_NAME") shouldBe "VAL"
    tableColumns.getInt("DATA_TYPE") shouldBe Types.DOUBLE
    tableColumns.next() shouldBe false

    executor.addColumn(schema, table, ColumnDefinition("ADDED_COLUMN", DerbySqlTypes.varchar(256))).isRight shouldBe true

    val newTableColumns = executor.connection.getMetaData.getColumns(null, schema, table, null)
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
      ColumnDefinition("ID", DerbySqlTypes.int, DerbySqlConstraints.notNull),
      ColumnDefinition("VAL", DerbySqlTypes.double)
    )

    executor.createSchema(schema).isRight shouldBe true
    executor.createTable(schema, table, columns).isRight shouldBe true

    val tableColumns = executor.connection.getMetaData.getColumns(null, schema, table, null)
    tableColumns.next() shouldBe true
    tableColumns.getString("COLUMN_NAME") shouldBe "ID"
    tableColumns.getInt("DATA_TYPE") shouldBe Types.INTEGER
    tableColumns.next() shouldBe true
    tableColumns.getString("COLUMN_NAME") shouldBe "VAL"
    tableColumns.getInt("DATA_TYPE") shouldBe Types.DOUBLE
    tableColumns.next() shouldBe false

    executor.dropColumn(schema, table, "VAL").isRight shouldBe true

    val newTableColumns = executor.connection.getMetaData.getColumns(null, schema, table, null)
    newTableColumns.next() shouldBe true
    newTableColumns.getString("COLUMN_NAME") shouldBe "ID"
    newTableColumns.getInt("DATA_TYPE") shouldBe Types.INTEGER
    newTableColumns.next() shouldBe false

    executor.connection.close()
  }

  "insert and select all rows, all columns should succeed" in {
    val executor = QueryExecutor(createDatabase("test-insert-select-all-rows-all-columns"))

    val schema = "SCM"
    val table = "TBL"
    val columns = List(
      ColumnDefinition("ID", DerbySqlTypes.int, DerbySqlConstraints.primaryKey),
      ColumnDefinition("LONG_COL", DerbySqlTypes.long),
      ColumnDefinition("DOUBLE_COL", DerbySqlTypes.double),
      ColumnDefinition("BOOLEAN_COL", DerbySqlTypes.boolean),
      ColumnDefinition("BYTES_COL", DerbySqlTypes.bytes),
      ColumnDefinition("STRING_COL", DerbySqlTypes.string),
      ColumnDefinition("VARCHAR_COL", DerbySqlTypes.varchar(256)),
    )

    executor.createSchema(schema).isRight shouldBe true
    executor.createTable(schema, table, columns).isRight shouldBe true

    val data = List(
      List(
        IntSqlValue(1),
        LongSqlValue(1),
        DoubleSqlValue(1.0),
        BooleanSqlValue(true),
        BytesSqlValue(Bytes(List(1, 2, 3).map(x => x.toByte))),
        StringSqlValue("string 1"),
        StringSqlValue("string 11")
      ),
        List(
        IntSqlValue(2),
        LongSqlValue(2),
        DoubleSqlValue(2.0),
        BooleanSqlValue(false),
        BytesSqlValue(Bytes(List(4, 5, 6).map(x => x.toByte))),
        StringSqlValue("string 2"),
        StringSqlValue("string 22")
      ),
        List(
        IntSqlValue(3),
        LongSqlValue(3),
        DoubleSqlValue(3.0),
        BooleanSqlValue(false),
        BytesSqlValue(Bytes(List(7, 8, 9).map(x => x.toByte))),
        StringSqlValue("string 3"),
        StringSqlValue("string 33")
      )
    )

    executor.insert(schema, table, columns.map(x => x.name), data).isRight shouldBe true

    val result = executor.select(schema, table)
    result.isRight shouldBe true
    result.getOrElse() shouldBe data
  }

  "insert and select some rows, all columns should succeed" in {
    val executor = QueryExecutor(createDatabase("test-insert-select-some-rows-all-columns"))

    val schema = "SCM"
    val table = "TBL"
    val columns = List(
      ColumnDefinition("ID", DerbySqlTypes.int, DerbySqlConstraints.primaryKey),
      ColumnDefinition("LONG_COL", DerbySqlTypes.long),
      ColumnDefinition("DOUBLE_COL", DerbySqlTypes.double),
      ColumnDefinition("BOOLEAN_COL", DerbySqlTypes.boolean),
      ColumnDefinition("BYTES_COL", DerbySqlTypes.bytes),
      ColumnDefinition("STRING_COL", DerbySqlTypes.string),
      ColumnDefinition("VARCHAR_COL", DerbySqlTypes.varchar(256)),
    )

    executor.createSchema(schema).isRight shouldBe true
    executor.createTable(schema, table, columns).isRight shouldBe true

    val data = List(
      List(
        IntSqlValue(1),
        LongSqlValue(1),
        DoubleSqlValue(1.0),
        BooleanSqlValue(true),
        BytesSqlValue(Bytes(List(1, 2, 3).map(x => x.toByte))),
        StringSqlValue("string 1"),
        StringSqlValue("string 11")
      ),
      List(
        IntSqlValue(2),
        LongSqlValue(2),
        DoubleSqlValue(2.0),
        BooleanSqlValue(false),
        BytesSqlValue(Bytes(List(4, 5, 6).map(x => x.toByte))),
        StringSqlValue("string 2"),
        StringSqlValue("string 22")
      ),
      List(
        IntSqlValue(3),
        LongSqlValue(3),
        DoubleSqlValue(3.0),
        BooleanSqlValue(false),
        BytesSqlValue(Bytes(List(7, 8, 9).map(x => x.toByte))),
        StringSqlValue("string 3"),
        StringSqlValue("string 33")
      )
    )

    executor.insert(schema, table, columns.map(x => x.name), data).isRight shouldBe true

    val result = executor.select(
      schemaName = schema,
      tableName = table,
      columns = List(),
      criteria = List(
        WhereClause("ID", "=", IntSqlValue(1)),
        OrClause("ID", "=", IntSqlValue(3))
      )
    )
    result.isRight shouldBe true
    result.getOrElse() shouldBe List(data.head, data.reverse.head)
  }

  "insert and select all rows, some columns should succeed" in {
    val executor = QueryExecutor(createDatabase("test-insert-select-all-rows-some-columns"))

    val schema = "SCM"
    val table = "TBL"
    val columns = List(
      ColumnDefinition("ID", DerbySqlTypes.int, DerbySqlConstraints.primaryKey),
      ColumnDefinition("LONG_COL", DerbySqlTypes.long),
      ColumnDefinition("DOUBLE_COL", DerbySqlTypes.double),
      ColumnDefinition("BOOLEAN_COL", DerbySqlTypes.boolean),
      ColumnDefinition("BYTES_COL", DerbySqlTypes.bytes),
      ColumnDefinition("STRING_COL", DerbySqlTypes.string),
      ColumnDefinition("VARCHAR_COL", DerbySqlTypes.varchar(256)),
    )

    executor.createSchema(schema).isRight shouldBe true
    executor.createTable(schema, table, columns).isRight shouldBe true

    val data = List(
      List(
        IntSqlValue(1),
        LongSqlValue(1),
        DoubleSqlValue(1.0),
        BooleanSqlValue(true),
        BytesSqlValue(Bytes(List(1, 2, 3).map(x => x.toByte))),
        StringSqlValue("string 1"),
        StringSqlValue("string 11")
      ),
      List(
        IntSqlValue(2),
        LongSqlValue(2),
        DoubleSqlValue(2.0),
        BooleanSqlValue(false),
        BytesSqlValue(Bytes(List(4, 5, 6).map(x => x.toByte))),
        StringSqlValue("string 2"),
        StringSqlValue("string 22")
      ),
      List(
        IntSqlValue(3),
        LongSqlValue(3),
        DoubleSqlValue(3.0),
        BooleanSqlValue(false),
        BytesSqlValue(Bytes(List(7, 8, 9).map(x => x.toByte))),
        StringSqlValue("string 3"),
        StringSqlValue("string 33")
      )
    )

    executor.insert(schema, table, columns.map(x => x.name), data).isRight shouldBe true

    val expectedData = List(
      List(
        LongSqlValue(1),
        DoubleSqlValue(1.0)
      ),
      List(
        LongSqlValue(2),
        DoubleSqlValue(2.0)
      ),
      List(
        LongSqlValue(3),
        DoubleSqlValue(3.0)
      )
    )

    val result = executor.select(
      schemaName = schema,
      tableName = table,
      columns = List("LONG_COL", "DOUBLE_COL"),
      criteria = List()
    )
    result.isRight shouldBe true
    result.getOrElse() shouldBe expectedData
  }

  "update should succeed" in {
    val executor = QueryExecutor(createDatabase("test-update"))

    val schema = "SCM"
    val table = "TBL"
    val columns = List(
      ColumnDefinition("ID", DerbySqlTypes.int, DerbySqlConstraints.primaryKey),
      ColumnDefinition("VAL1", DerbySqlTypes.string),
      ColumnDefinition("VAL2", DerbySqlTypes.double)
    )

    executor.createSchema(schema).isRight shouldBe true
    executor.createTable(schema, table, columns).isRight shouldBe true

    val data = List(
      List(
        IntSqlValue(1),
        StringSqlValue("string 1"),
        DoubleSqlValue(1.0)
      ),
      List(
        IntSqlValue(2),
        StringSqlValue("string 2"),
        DoubleSqlValue(2.0)
      ),
      List(
        IntSqlValue(3),
        StringSqlValue("string 3"),
        DoubleSqlValue(3.0)
      )
    )

    executor.insert(schema, table, columns.map(x => x.name), data).isRight shouldBe true

    val expectedData = List(
      List(
        IntSqlValue(1),
        StringSqlValue("string 11"),
        DoubleSqlValue(1.0)
      ),
      List(
        IntSqlValue(2),
        StringSqlValue("string 2"),
        DoubleSqlValue(2.0)
      ),
      List(
        IntSqlValue(3),
        StringSqlValue("string 33"),
        DoubleSqlValue(3.0)
      )
    )

    executor.update(
      schema,
      table,
      Map("VAL1" -> StringSqlValue("string 11")),
      List(WhereClause("ID", "=", IntSqlValue(1)))
    )

    executor.update(
      schema,
      table,
      Map("VAL1" -> StringSqlValue("string 33")),
      List(WhereClause("ID", "=", IntSqlValue(3)))
    )

    val result = executor.select(schema, table)
    result.isRight shouldBe true
    result.getOrElse() shouldBe expectedData
  }

  "delete should succeed" in {
    val executor = QueryExecutor(createDatabase("test-delete"))

    val schema = "SCM"
    val table = "TBL"
    val columns = List(
      ColumnDefinition("ID", DerbySqlTypes.int, DerbySqlConstraints.primaryKey),
      ColumnDefinition("VAL1", DerbySqlTypes.string),
      ColumnDefinition("VAL2", DerbySqlTypes.double)
    )

    executor.createSchema(schema).isRight shouldBe true
    executor.createTable(schema, table, columns).isRight shouldBe true

    val data = List(
      List(
        IntSqlValue(1),
        StringSqlValue("string 1"),
        DoubleSqlValue(1.0)
      ),
      List(
        IntSqlValue(2),
        StringSqlValue("string 2"),
        DoubleSqlValue(2.0)
      ),
      List(
        IntSqlValue(3),
        StringSqlValue("string 3"),
        DoubleSqlValue(3.0)
      )
    )

    executor.insert(schema, table, columns.map(x => x.name), data).isRight shouldBe true

    val expectedData = List(
      List(
        IntSqlValue(1),
        StringSqlValue("string 1"),
        DoubleSqlValue(1.0)
      ),
      List(
        IntSqlValue(3),
        StringSqlValue("string 3"),
        DoubleSqlValue(3.0)
      )
    )

    executor.delete(schema, table, List(WhereClause("ID", "=", IntSqlValue(2))))

    val result = executor.select(schema, table)
    result.isRight shouldBe true
    result.getOrElse() shouldBe expectedData
  }

  private def createDatabase(name: String): Connection = {
    val connectionString = ConnectionStringFactory.createDerbyConnectionString(
      InMemoryDatabaseProtocol,
      name,
      Map("create" -> "true")
    )

    DatabaseConnection.connect(connectionString).getOrElse().asInstanceOf[Connection]
  }
}
