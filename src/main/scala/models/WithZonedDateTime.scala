package models

import java.time.{ZoneId, ZonedDateTime}
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.temporal.ChronoField

import io.circe.{Decoder, Encoder, HCursor, Json}
import io.circe.syntax._

case class WithZonedDateTime(name: String, startTime: ZonedDateTime, onAirTime: Option[ZonedDateTime])

object WithZonedDateTime {

  val dateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss'Z'")
  val offSetDateTimeFormatter: DateTimeFormatter =
    new DateTimeFormatterBuilder().appendPattern("yyyy-MM-dd'T'HH:mm").appendOffsetId().toFormatter()

  val encoder1: Encoder[ZonedDateTime] =
    Encoder.encodeString.contramap[ZonedDateTime](_.withZoneSameInstant(
      ZoneId.of("Europe/London")).format(offSetDateTimeFormatter))

  val encoder2: Encoder[Option[ZonedDateTime]] = {
    case Some(zdt) => zdt.format(dateTimeFormatter).asJson
    case None => Json.obj()
  }

  private val timeWithSeconds = new DateTimeFormatterBuilder()
    .appendValue(ChronoField.HOUR_OF_DAY, 2)
    .appendLiteral(':')
    .appendValue(ChronoField.MINUTE_OF_HOUR, 2)
    .appendLiteral(':')
    .appendValue(ChronoField.SECOND_OF_MINUTE, 2)
    .toFormatter

  private val time = new DateTimeFormatterBuilder()
    .appendValue(ChronoField.HOUR_OF_DAY, 2)
    .appendLiteral(':')
    .appendValue(ChronoField.MINUTE_OF_HOUR, 2)
    .toFormatter

  def isoDateFormatter(time: DateTimeFormatter): DateTimeFormatter = new DateTimeFormatterBuilder().parseCaseInsensitive
    .append(DateTimeFormatter.ISO_LOCAL_DATE)
    .appendLiteral('T')
    .append(time)
    .appendOffsetId()
    .toFormatter

  val decoder1: Decoder[ZonedDateTime] =
    (c: HCursor) => c.value.as[String].map(str => ZonedDateTime.parse(str, isoDateFormatter(time)))

  val decoder2: Decoder[Option[ZonedDateTime]] =
    (c: HCursor) => c.value.as[Option[String]].map {
      case Some(zdt) => Some(ZonedDateTime.parse(zdt, isoDateFormatter(timeWithSeconds)))
      case _ => None
    }


  ///////////////////////////////////////////////////
  implicit val withZonedDateTimeEncoder: Encoder[WithZonedDateTime] = Encoder.instance { resp =>
    Json.obj(
      "name" -> resp.name.asJson,
      "startTime" -> resp.startTime.asJson(encoder1),
      "onAirTime" -> resp.onAirTime.asJson(encoder2)
    )
  }

  implicit val withZonedDateTimeDecoder: Decoder[WithZonedDateTime] = new Decoder[WithZonedDateTime] {
    def apply(c: HCursor): Decoder.Result[WithZonedDateTime] =
      for {
        name <- c.downField("name").as[String]
        startTime <- c.downField("startTime").as[ZonedDateTime](decoder1)
        onAirTime <- c.downField("onAirTime").as[Option[ZonedDateTime]](decoder2)
      } yield WithZonedDateTime(name, startTime, onAirTime)
  }


}
