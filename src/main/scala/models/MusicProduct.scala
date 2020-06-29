package models

import io.circe._
import io.circe.generic.semiauto._

case class MusicProduct(id: Int, name: String, description: String, price: String, category: String)

object MusicProduct {
  implicit val decode: Decoder[MusicProduct] = deriveDecoder[MusicProduct]
  implicit val encode: Encoder[MusicProduct] = deriveEncoder[MusicProduct]
}
