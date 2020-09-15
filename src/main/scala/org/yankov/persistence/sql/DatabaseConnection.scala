package org.yankov.persistence.sql

import java.sql.{Connection, DriverManager, SQLException}

object DatabaseConnection {
  def connect(connectionString: String): Either[Throwable, Connection] = {
    try {
      Right(DriverManager.getConnection(connectionString))
    } catch {
      case e: SQLException => Left(e)
    }
  }

  def close(connection: Connection): Either[Throwable, Unit] = {
    try {
      Right(connection.close())
    } catch {
      case e: SQLException => Left(e)
    }
  }
}
