package org.yankov.persistence.sql

import org.scalatest.{Matchers, WordSpec}

class DatabaseConnectionTest extends WordSpec with Matchers {
  "connect should succeed" in {
    val connectionString = ConnectionStringFactory.createDerbyConnectionString(
      InMemoryDatabaseProtocol,
      "test",
      Map("create" -> "true")
    )

    DatabaseConnection.connect(connectionString).isRight shouldBe true
  }
}
