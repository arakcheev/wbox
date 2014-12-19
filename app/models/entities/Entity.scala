package models.entities

import models.db.MongoDB
import org.joda.time.DateTime
import reactivemongo.bson._

import scala.concurrent.ExecutionContext.Implicits.global

/*
 * Copyright 2014(23.11.14) Arakcheev Artem (artem.arakcheev@phystech.edu)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

trait Entity[E] extends MongoDB with Statuses {
  self =>

  type TT


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

trait Statuses {

  /**
   * Deleted status
   */
  val DELETED = -1

  /**
   * Status of new entity
   */
  val NEW = 1
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

  //todo: write method
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
        doc.getAs[String]("name"),
        doc.getAs[Int]("status").getOrElse(0),
        doc.getAs[String]("user"),
        doc.getAs[String]("uuid"),
        doc.getAs[Int]("revision"),
        doc.getAs[BSONDateTime]("date").map(_.value),
        doc.getAs[BSONDocument]("users").map(docs =>
          docs.elements.map {
            tuple =>
              tuple._1 -> tuple._2.seeAsTry[Int].get
          }.toMap
        ),
        doc.getAs[String]("description")
      )
    }
  }

  implicit object RepositoryWriter extends BSONDocumentWriter[Repository] {
    def write(doc: Repository): BSONDocument = BSONDocument(
      "_id" -> doc.id,
      "name" -> doc.name,
      "status" -> BSONInteger(doc.status),
      "user" -> doc.user,
      "uuid" -> doc.uuid,
      "revision" -> BSONInteger(doc.revision.getOrElse(1)), //need to increment revision of document. This value cannot be None
      "date" -> doc.date.map(BSONDateTime),
      "users" -> doc.users,
      "description" -> doc.description
    )
  }


  implicit object UserWriter extends BSONDocumentWriter[User] {
    def write(user: User): BSONDocument = {
      BSONDocument(
        "_id" -> user.id,
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
        doc.getAs[String]("mask"),
        doc.getAs[BSONDocument]("params").map(docs =>
          docs.elements.map { tuple =>
            tuple._1 -> tuple._2.seeAsTry[String].get
          }.toMap
        ).getOrElse(Map.empty),
        doc.getAs[BSONDateTime]("date").map(_.value).getOrElse(0l),
        doc.getAs[Int]("status").getOrElse(0),
        doc.getAs[BSONDateTime]("publishDate").map(_.value),
        doc.getAs[BSONDateTime]("unpublishDate").map(_.value),
        doc.getAs[BSONObjectID]("release"),
        doc.getAs[BSONObjectID]("user"),
        doc.getAs[Int]("revision"),
        doc.getAs[String]("uuid"),
        doc.getAs[List[String]]("tags")
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
      "revision" -> BSONInteger(doc.revision.getOrElse(1)), //need to increment revision of document. This value cannot be None
      "uuid" -> doc.uuid,
      "tags" -> doc.tags
    )
  }


  implicit object MaskReader extends BSONDocumentReader[Mask] {
    def read(doc: BSONDocument): Mask = {
      Mask(
        doc.getAs[BSONObjectID]("_id"),
        doc.getAs[String]("name"),
        doc.getAs[String]("title"),
        doc.getAs[BSONDocument]("params").map(docs =>
          docs.elements.map {
            tuple =>
              tuple._1 -> tuple._2.seeAsTry[String].get
          }.toMap
        ).getOrElse(Map.empty),
        doc.getAs[String]("repo"),
        doc.getAs[Int]("status").getOrElse(0),
        doc.getAs[String]("uuid"),
        doc.getAs[Int]("revision"),
        doc.getAs[BSONObjectID]("user"),
        doc.getAs[BSONDateTime]("date").map(_.value)
      )
    }
  }

  implicit object MaskWriter extends BSONDocumentWriter[Mask] {
    def write(doc: Mask): BSONDocument = BSONDocument(
      "_id" -> doc.id,
      "name" -> doc.name,
      "title" -> doc.title,
      "params" -> doc.params,
      "repo" -> doc.repo,
      "status" -> BSONInteger(doc.status),
      "uuid" -> doc.uuid,
      "revision" -> BSONInteger(doc.revision.getOrElse(1)), //need to increment revision of mask. This value cannot be None
      "user" -> doc.user,
      "date" -> doc.date.map(BSONDateTime)
    )
  }

  implicit object ReleaseReader extends BSONDocumentReader[Release] {
    def read(doc: BSONDocument): Release = {
      Release(
        doc.getAs[BSONObjectID]("_id"),
        doc.getAs[String]("name"),
        doc.getAs[BSONDateTime]("publishDate").map(_.value),
        doc.getAs[BSONDateTime]("unpublishDate").map(_.value),
        doc.getAs[BSONObjectID]("repo"),
        doc.getAs[BSONObjectID]("user"),
        doc.getAs[String]("uuid"),
        doc.getAs[Int]("revision"),
        doc.getAs[BSONDateTime]("date").map(_.value)
      )
    }
  }

  implicit object ReleaseWriter extends BSONDocumentWriter[Release] {
    def write(rel: Release): BSONDocument = BSONDocument(
      "_id" -> rel.id,
      "name" -> rel.name,
      "publishDate" -> BSONDateTime(rel.publishDate.getOrElse(DateTime.now().getMillis)),
      "unpublishDate" -> BSONDateTime(rel.unpublishDate.getOrElse(DateTime.now().plusYears(10).getMillis)),
      "repo" -> rel.repo,
      "user" -> rel.user,
      "uuid" -> rel.uuid,
      "revision" -> BSONInteger(rel.revision.getOrElse(1)), //need to increment revision of mask. This value cannot be None
      "date" -> rel.date.map(BSONDateTime)
    )
  }

  implicit object AttachmentReader extends BSONDocumentReader[Attachment] {
    def read(doc: BSONDocument): Attachment = {
      val att = Attachment.empty()
      att.id = doc.getAs[BSONObjectID]("_id")
      att.name = doc.getAs[String]("name")
      att.entity = doc.getAs[String]("entity")
      att.url = doc.getAs[String]("url")
      att.status = doc.getAs[Int]("status")
      att.user = doc.getAs[String]("user")
      att.uuid = doc.getAs[String]("uuid")
      att.revision = doc.getAs[Int]("revision")
      att.date = doc.getAs[BSONDateTime]("date").map(_.value)
      att.repo = doc.getAs[String]("entity")
      att
    }
  }

  implicit object AttachmentWriter extends BSONDocumentWriter[Attachment] {
    def write(att: Attachment): BSONDocument = BSONDocument(
      "_id" -> att.id,
      "name" -> att.name,
      "entity" -> att.entity,
      "url" -> att.url,
      "status" -> att.status,
      "user" -> att.user,
      "uuid" -> att.uuid,
      "revision" -> BSONInteger(att.revision.getOrElse(1)), //need to increment revision of mask. This value cannot be None
      "date" -> att.date.map(BSONDateTime),
      "repo" -> att.repo
    )
  }


}
