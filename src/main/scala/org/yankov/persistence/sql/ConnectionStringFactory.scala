package org.yankov.persistence.sql

trait DatabaseProtocol

case object DirectoryDatabaseProtocol extends DatabaseProtocol

case object InMemoryDatabaseProtocol extends DatabaseProtocol

object ConnectionStringFactory {
  def createDerbyConnectionString(databaseProtocol: DatabaseProtocol,
                                 databaseName: String,
                                 attributes: Map[String, String]): String = {
    def getProtocol: String = databaseProtocol match {
      case DirectoryDatabaseProtocol => "directory"
      case InMemoryDatabaseProtocol => "memory"
      case _ => "directory"
    }
    val att = attributes.foldLeft("")((acc, x) => s"$acc;${x._1}=${x._2}")
    s"jdbc:derby:$getProtocol:$databaseName$att"
  }
}
