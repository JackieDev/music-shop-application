package db

import java.sql.Timestamp
import java.time.{ZoneOffset, ZonedDateTime}

import cats.effect.Sync
import models.WithZonedDateTime
import doobie._
import doobie.implicits._
import doobie.implicits.javasql._
import fs2.Stream

final class ZonedDateTimeRepository[F[_]: Sync](tx: Transactor[F]) {

  implicit val metaInstance: Meta[ZonedDateTime] = Meta[Timestamp]
    .imap(ts => ZonedDateTime.ofInstant(ts.toInstant, ZoneOffset.UTC))(zdt => Timestamp.from(zdt.toInstant))

  val createSqlTimestamp: ZonedDateTime => Timestamp = zonedDateTime => Timestamp.from(zonedDateTime.toInstant)

  private def addRowSql(zdt: WithZonedDateTime): ConnectionIO[Int] = {
    sql"INSERT INTO zoned_date_time (name, startTime, onAirTime) VALUES (${zdt.name}, ${zdt.startTime}, ${zdt.onAirTime})"
      .stripMargin
      .update
      .run
  }

  def addRow(zdt: WithZonedDateTime): F[Int] = {
    addRowSql(zdt)
      .transact(tx)
  }

  private def getZDTsSql(fromDate: Timestamp, toDate: Timestamp): Stream[F, WithZonedDateTime] =
    sql"SELECT name, startTime, onAirTime WHERE (startTime >= $fromDate AND startTime <= $toDate) FROM zoned_date_time"
      .query[WithZonedDateTime]
      .stream
    .transact(tx)

  def getZDTs(fromDate: ZonedDateTime, toDate: ZonedDateTime): Stream[F, WithZonedDateTime] = {
    println(s"################ fetching ZDTs from $fromDate to $toDate")
    println(s"############### using createSqlTimestamp dates that look like this: " +
      s"fromDate: ${createSqlTimestamp(fromDate)} and toDate: ${createSqlTimestamp(toDate)}")

     getZDTsSql(createSqlTimestamp(fromDate), createSqlTimestamp(toDate))

  }

  def getAllZDTs(): Stream[F, WithZonedDateTime] =
    sql"SELECT * FROM zoned_date_time"
      .query[WithZonedDateTime]
      .stream
      .transact(tx)

}
