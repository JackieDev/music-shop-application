import cats.effect._
import cats.implicits._
import com.typesafe.config.ConfigFactory
import routes._
import config._
import db._
import doobie._
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze._
import pureconfig.loadConfigOrThrow

import scala.io.StdIn

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    val migrator = new FlywayDatabaseMigrator

    val cfg = ConfigFactory.load(getClass().getClassLoader())

    val program = for {
      dbConfig      <- IO(loadConfigOrThrow[DatabaseConfig](cfg, "database"))
      //dbConfig    <- IO(DatabaseConfig.applicationConfig)
      ms            <- migrator.flywayMigrateDatabase(dbConfig)
      _             <- IO(println(s"Database Migration Result: $ms"))
      tx = Transactor.fromDriverManager[IO](dbConfig.driver, dbConfig.url, dbConfig.user, dbConfig.password)
      musicRepo      = new MusicProductRepository(tx)
      zdtRepo        = new ZonedDateTimeRepository(tx)
      productRoutes  = new ProductRoutes(musicRepo, zdtRepo)
      routes         = productRoutes.routes
      httpApp        = Router("/" -> routes).orNotFound
      server         = BlazeServerBuilder[IO].bindHttp(7000, "localhost").withHttpApp(httpApp)
      fiber          = server.resource.use(_ => IO(StdIn.readLine())).as(ExitCode.Success)
    } yield fiber
    program.attempt.unsafeRunSync match {
      case Left(e) =>
        IO {
          println("*** An error occurred! ***")
          if (e ne null) {
            println(e.getMessage)
          }
          ExitCode.Error
        }
      case Right(r) => r
    }
  }
}
