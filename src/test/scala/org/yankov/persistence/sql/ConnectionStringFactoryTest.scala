package org.yankov.persistence.sql

import org.scalatest.{Matchers, WordSpec}

class ConnectionStringFactoryTest extends WordSpec with Matchers {
  "create derby connection string for in-memory database should succeed" in {
    ConnectionStringFactory.createDerbyConnectionString(
      InMemoryDatabaseProtocol,
      "test",
      Map("create" -> "true", "att" -> "value")
    ) shouldBe "jdbc:derby:memory:test;create=true;att=value"
  }

  "create derby connection string for directory database should succeed" in {
    ConnectionStringFactory.createDerbyConnectionString(
      DirectoryDatabaseProtocol,
      "test",
      Map("create" -> "true")
    ) shouldBe "jdbc:derby:directory:test;create=true"
  }
}
