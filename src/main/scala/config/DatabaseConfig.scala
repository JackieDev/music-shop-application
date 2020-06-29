package config

import pureconfig._
import pureconfig.generic.semiauto._

case class DatabaseConfig(url: String, user: String, password: String, driver: String)

object DatabaseConfig {
  implicit val configReader: ConfigReader[DatabaseConfig] = deriveReader[DatabaseConfig]
}
