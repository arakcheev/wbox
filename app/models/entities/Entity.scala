package models.entities

import models.db.MongoDB
import org.joda.time.DateTime
import reactivemongo.api.BSONSerializationPack.Writer
import reactivemongo.bson._

import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by artem on 23.11.14.
 */
trait Entity[E] extends MongoDB {
  self =>


  /**
   * TODO: broadcast fields to all entities
   */

  private[models] def save(doc: E, writer: BSONDocumentWriter[E]) = {
    val bson = writer.write(doc)
    collection.insert[BSONDocument](bson).map { wr =>
      if (wr.inError) {
        None
      } else {
        Some(doc)
      }
    }
  }

}

/**
 * TODO: macros for mappers
 * @tparam T
 */
trait Mapper[T] extends BSONDocumentWriter[T] with MapRW {

}

trait MapRW {
  implicit def ValueMapWriter[V](implicit vw: BSONWriter[V, _ <: BSONValue]): BSONDocumentWriter[Map[String, V]] =
    new BSONDocumentWriter[Map[String, V]] {
      def write(map: Map[String, V]): BSONDocument = {
        val elements = map.toStream.map {
          tuple =>
            tuple._1 -> vw.write(tuple._2)
        }
        BSONDocument(elements)
      }
    }

  implicit def MapReader[V](implicit vr: BSONDocumentReader[V]): BSONDocumentReader[Map[String, V]] = new BSONDocumentReader[Map[String, V]] {
    def read(bson: BSONDocument): Map[String, V] = {
      val elements = bson.elements.map { tuple =>
        // assume that all values in the document are BSONDocuments
        tuple._1 -> vr.read(tuple._2.seeAsTry[BSONDocument].get)
      }
      elements.toMap
    }
  }
}

object EntityRW extends MapRW {

  implicit object RepositoryReader extends BSONDocumentReader[Repository] {
    def read(doc: BSONDocument): Repository = {
      Repository(
        doc.getAs[BSONObjectID]("_id"),
        doc.getAs[String]("name").getOrElse(""),
        doc.getAs[Int]("status").getOrElse(0),
        doc.getAs[String]("user").getOrElse(""),
        doc.getAs[String]("uuid").getOrElse("")
      )
    }
  }

  implicit object RepositoryWriter extends BSONDocumentWriter[Repository] {
    def write(doc: Repository): BSONDocument = BSONDocument(
      "_id" -> doc.id.getOrElse(BSONObjectID.generate),
      "name" -> doc.name,
      "status" -> BSONInteger(doc.status),
      "user" -> doc.user,
      "uuid" -> doc.user
    )
  }


  implicit object UserWriter extends BSONDocumentWriter[User] {
    def write(user: User): BSONDocument = {
      BSONDocument(
        "_id" -> user.id.getOrElse(BSONObjectID.generate),
        "uuid" -> user.uuid.getOrElse(""),
        "email" -> user.email.getOrElse(""),
        "password" -> user.password.getOrElse(""),
        "creditCards" -> user.creditCards.getOrElse(BSONArray.empty),
        "account" -> user.account.getOrElse(BSONDocument.empty),
        "avatar" -> user.avatar.getOrElse(""),
        "status" -> BSONInteger(user.status),
        "subUser" -> user.subUser,
        "users" -> user.users.getOrElse(BSONArray.empty)
      )
    }
  }

  implicit object UserReader extends BSONDocumentReader[User] {
    def read(doc: BSONDocument): User = {
      User(
        doc.getAs[BSONObjectID]("_id"),
        doc.getAs[String]("uuid"),
        doc.getAs[String]("email"),
        doc.getAs[String]("password"),
        doc.getAs[BSONArray]("creditCards"),
        doc.getAs[BSONDocument]("account"),
        doc.getAs[String]("avatar"),
        doc.getAs[Int]("status").getOrElse(0),
        doc.getAs[Boolean]("subUser").getOrElse(true),
        doc.getAs[BSONArray]("users")
      )
    }
  }


  implicit object DocumentReader extends BSONDocumentReader[Document] {
    def read(doc: BSONDocument): Document = {
      Document(
        doc.getAs[BSONObjectID]("_id"),
        doc.getAs[String]("name"),
        doc.getAs[BSONObjectID]("mask"),
        doc.getAs[BSONDocument]("params").map(docs =>
          docs.elements.map { tuple =>
            tuple._1 -> tuple._2.seeAsTry[String].get
          }.toMap
        ).getOrElse(Map.empty),
        doc.getAs[BSONDateTime]("date").map(_.value).getOrElse(0l),
        doc.getAs[Int]("status").getOrElse(0),
        doc.getAs[Long]("publishDate"),
        doc.getAs[Long]("unpublishDate"),
        doc.getAs[BSONObjectID]("release"),
        doc.getAs[BSONObjectID]("user"),
        doc.getAs[Int]("revision"),
        doc.getAs[String]("uuid")
      )
    }
  }

  implicit object DocumentWriter extends BSONDocumentWriter[Document] {
    def write(doc: Document): BSONDocument = BSONDocument(
      "_id" -> doc.id,
      "name" -> doc.name,
      "mask" -> doc.mask,
      "params" -> doc.params,
      "date" -> BSONDateTime(doc.date),
      "status" -> BSONInteger(doc.status),
      "publishDate" -> BSONDateTime(doc.publishDate.getOrElse(DateTime.now().getMillis)),
      "unpublishDate" -> BSONDateTime(doc.unpublishDate.getOrElse(DateTime.now().plusYears(10).getMillis)),
      "release" -> doc.release,
      "user" -> doc.user,
      "revision" -> doc.revision,
      "uuid" -> doc.uuid
    )
  }


  implicit object MaskReader extends BSONDocumentReader[Mask] {
    def read(doc: BSONDocument): Mask = {
      Mask(
        doc.getAs[BSONObjectID]("_id"),
        doc.getAs[String]("name").getOrElse(""),
        doc.getAs[String]("title").getOrElse(""),
        doc.getAs[BSONDocument]("params").map(docs =>
          docs.elements.map {
            tuple =>
              tuple._1 -> tuple._2.seeAsTry[String].get
          }.toMap
        ).getOrElse(Map.empty),
        doc.getAs[String]("repo").getOrElse(""),
        doc.getAs[Int]("status").getOrElse(0)
      )
    }
  }

  implicit object MaskWriter extends BSONDocumentWriter[Mask] {
    def write(doc: Mask): BSONDocument = BSONDocument(
      "_id" -> doc.id.getOrElse(BSONObjectID.generate),
      "name" -> doc.name,
      "title" -> doc.title,
      "params" -> doc.params,
      "repo" -> doc.repo,
      "status" -> BSONInteger(doc.status)
    )
  }

  implicit object ReleaseReader extends BSONDocumentReader[Release] {
    def read(doc: BSONDocument): Release = {
      Release(
        doc.getAs[BSONObjectID]("_id"),
        doc.getAs[String]("name"),
        doc.getAs[BSONDateTime]("publishDate").map(_.value),
        doc.getAs[BSONDateTime]("unpublishDate").map(_.value),
        doc.getAs[BSONObjectID]("mask"),
        doc.getAs[BSONObjectID]("user")
      )
    }
  }

  implicit object ReleaseWriter extends BSONDocumentWriter[Release] {
    def write(rel: Release): BSONDocument = BSONDocument(
      "_id" -> rel.id,
      "name" -> rel.name,
      "publishDate" -> BSONDateTime(rel.publishDate.getOrElse(DateTime.now().getMillis)),
      "unpublishDate" -> BSONDateTime(rel.unpublishDate.getOrElse(DateTime.now().plusYears(10).getMillis)),
      "mask" -> rel.mask,
      "user" -> rel.user
    )
  }


}
