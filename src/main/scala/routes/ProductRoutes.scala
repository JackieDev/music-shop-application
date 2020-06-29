package routes

import java.sql.Timestamp
import java.time.{Instant, ZoneOffset, ZonedDateTime}
import java.time.format.DateTimeFormatter

import io.circe.Json
import io.circe.syntax._
import cats.Applicative
import cats.effect.Sync
import cats.implicits._
import db._
import doobie.util.meta.Meta
import models._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl._
import fs2.Stream
import models.WithZonedDateTime._
import routes.QueryParams.{FromQueryParamDecoderMatcher, ToQueryParamDecoderMatcher}

class ProductRoutes[F[_]: Sync](repo: MusicProductRepository[F],
                                zdtRepo: ZonedDateTimeRepository[F]) extends Http4sDsl[F] {
  implicit def decodeProduct: EntityDecoder[F, MusicProduct]                    = jsonOf
  implicit def encodeProduct[A[_]: Applicative]: EntityEncoder[A, MusicProduct] = jsonEncoderOf

  implicit def decodeZDT: EntityDecoder[F, WithZonedDateTime] = jsonOf // I think this will pick up my wzdt decoder
  implicit def encodeZDT[A[_]: Applicative]: EntityEncoder[A, WithZonedDateTime] = jsonEncoderOf

  implicit val isoInstantCodec: QueryParamCodec[Instant] =
    QueryParamCodec.instantQueryParamCodec(DateTimeFormatter.ISO_INSTANT)
  // isoInstantCodec: org.http4s.QueryParamCodec[java.time.Instant] = org.http4s.QueryParamCodec$$anon$3@6c58da22

  //implicit val metaInstance: Meta[ZonedDateTime] = Meta[Timestamp]
    //.imap(ts => ZonedDateTime.ofInstant(ts.toInstant, ZoneOffset.UTC))(zdt => Timestamp.from(zdt.toInstant))

  object IsoInstantParamMatcher extends QueryParamDecoderMatcher[Instant]("timestamp")
  // defined object IsoInstantParamMatcher

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
      // View all items in the products table
    case GET -> Root / "products" => {
      val ps = repo.loadProducts()
        .collect {
          case p: MusicProduct => p
        }
        .map(s => s.asJson)
      val result: Stream[F, Json] = ps
      Ok(result)
    }

      // Search products in the database with a search string
    case GET -> Root / "products" / "search" / search => {
      println(s"Call received to search for $search")
      val ps = repo.searchProducts(search)
        .collect {
          case p: MusicProduct => p
        }
        .map { s =>
          println(s"Sending this to frontend: $s")
          s.asJson
        }
      val result: Stream[F, Json] = ps
      Ok(result)
    }

    // View all items in specified category
    case GET -> Root / "products" / category => {
      val ps = repo.loadProductsByCategory(category)
        .collect {
          case p: MusicProduct => p
        }
        .map(s => s.asJson)
      val result: Stream[F, Json] = ps
      Ok(result)
    }

    case GET -> Root / "last-id" =>
      val maxCurrentId = repo.getLastIdUsed
        .collect {
          case id: Int => id
        }
        .map(id => id.asJson)
      val result: Stream[F, Json] = maxCurrentId
      Ok(result)

      // Add an iem to the products table
    case req @ POST -> Root / "product" => {
      req
        .as[MusicProduct]
        .flatMap { p =>
          for {
            count <- repo.addProduct(p)
            res <- count match {
              case 0 => NotFound()
              case _ => NoContent()
            }
          } yield res
        }
        .handleErrorWith {
          case InvalidMessageBodyFailure(dets, _) => BadRequest(s"Error details: $dets")
        }
    }

    case GET -> Root / "status" =>
      Ok()

    //////////////////////// zoned date time
    case req @ PUT -> Root / "zdt" => {
      req
        .as[WithZonedDateTime]
        .flatMap { z =>
          for {
            count <- zdtRepo.addRow(z)
            res <- count match {
              case 0 => NotFound()
              case _ => NoContent()
            }
          } yield res
        }
        .handleErrorWith {
          case InvalidMessageBodyFailure(dets, _) => BadRequest(s"Error details: $dets")
        }
    }

    case req @ GET -> Root / "zdt" :? FromQueryParamDecoderMatcher(fromDate) +& ToQueryParamDecoderMatcher(toDate) => {
      val zdts = zdtRepo.getZDTs(fromDate, toDate)
        .collect {
          case zdt: WithZonedDateTime => zdt
        }
        .map(s => s.asJson)
      val result: Stream[F, Json] = zdts
      Ok(result)
    }

    case req @ GET -> Root / "zdt" => {
      val zdts = zdtRepo.getAllZDTs()
        .collect {
          case zdt: WithZonedDateTime => zdt
        }
        .map(s => s.asJson)
      val result: Stream[F, Json] = zdts
      Ok(result)
    }
  }

}
