package db

import cats.effect.IO
import config.DatabaseConfig
import org.flywaydb.core.Flyway

class FlywayDatabaseMigrator {

  def flywayMigrateDatabase(dbConfig: DatabaseConfig): IO[Int] =
    IO {
        val flyway = Flyway.configure().dataSource(dbConfig.url, dbConfig.user, dbConfig.password).load()
        val migrationsApplied: Int = flyway.migrate()
        println(s"Successfully applied $migrationsApplied migrations to the `music-shop` database")
        migrationsApplied
       }
}
