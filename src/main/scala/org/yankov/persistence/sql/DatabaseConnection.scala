package org.yankov.persistence.sql

import java.sql.{Connection, DriverManager}

object DatabaseConnection {
  def connect(connectionString: String): Connection = DriverManager.getConnection(connectionString)

  def close(connection: Connection): Unit = connection.close()
}
