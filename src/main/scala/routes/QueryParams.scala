package routes

import java.time.{LocalDateTime, ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter

import org.http4s.{ParseFailure, QueryParamDecoder, QueryParameterValue}
import org.http4s.dsl.impl.QueryParamDecoderMatcher

import cats.data.Validated._
import scala.util.{Failure, Success, Try}

object QueryParams {

  import Decoders._

  val londonZone: ZoneId = ZoneId.of("Europe/London")

  object FromQueryParamDecoderMatcher
    extends QueryParamDecoderMatcher[ZonedDateTime]("from")(dateTimeQueryParamDecoder(urlFriendlyFormatterWithHours))

  object ToQueryParamDecoderMatcher
    extends QueryParamDecoderMatcher[ZonedDateTime]("to")(dateTimeQueryParamDecoder(urlFriendlyFormatterWithHours))

  object Decoders {
    val urlFriendlyFormatterWithoutHours: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMdd")
    val urlFriendlyFormatterWithHours: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMddHHmm")

    def dateTimeQueryParamDecoder(format: DateTimeFormatter): QueryParamDecoder[ZonedDateTime] =
      (queryParam: QueryParameterValue) => {

        val maybeDate: Try[ZonedDateTime] = Try {
          ZonedDateTime.of(LocalDateTime.parse(queryParam.value, urlFriendlyFormatterWithHours), londonZone)
        }

        maybeDate match {
          case Success(date) => validNel(date)
          case Failure(e) =>
            val msg = s"Failure parsing date '${queryParam.value}', \n${e.getStackTrace.mkString("\n")}"
            invalidNel(ParseFailure(msg, msg))
        }
      }

  }

}
