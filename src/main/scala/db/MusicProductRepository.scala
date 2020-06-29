package db

import cats.effect.Sync
import models.MusicProduct
import doobie._
import doobie.implicits._
import fs2.Stream

final class MusicProductRepository[F[_]: Sync](tx: Transactor[F]) {

  def loadProducts(): Stream[F, MusicProduct] =
    sql"SELECT * FROM products"
      .query[MusicProduct]
      .stream
      .transact(tx)

  def loadProductsByCategory(category: String): Stream[F, MusicProduct] =
    sql"SELECT * FROM products WHERE category = $category"
      .query[MusicProduct]
      .stream
      .transact(tx)

  def addProduct(p: MusicProduct): F[Int] = {
    sql"INSERT INTO products (id, name, description, price, category) VALUES (${p.id}, ${p.name}, ${p.description}, ${p.price}, ${p.category})"
      .update
      .run
      .transact(tx)
  }

  def searchProducts(searchStr: String): Stream[F, MusicProduct] =
    sql"SELECT * FROM products WHERE name LIKE ${'%' + searchStr + '%'} OR description LIKE ${'%' + searchStr + '%'}"
      .query[MusicProduct]
      .stream
      .transact(tx)

  def getLastIdUsed: Stream[F, Int] =
    sql"SELECT id FROM products ORDER BY id DESC LIMIT 1"
      .query[Int]
      .stream
      .transact(tx)
}
